library(forecast)
# install.packages("quantmod")
library(quantmod)

# Pobranie danych
getSymbols(Symbols = "^GSPC", src = "yahoo", from = "2004-01-01", to = "2024-06-03")

# Pobranie danych jako time series
#getSymbols(Symbols = "^GSPC", src = "yahoo", from = "2014-01-01", to = "2024-06-03",
#           return.class = "ts")

# Format datasetu
#GSPC - GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted

head(GSPC)
str(GSPC)
sp500.close <- GSPC[, "GSPC.Close"]
sp500.close[ 1:10, ]

plot(sp500.close, main = "Wykres S&P 500 - 20 lat")
class(sp500.close) # xts, zoo

library(lattice)
xyplot(sp500.close, aspect = 1 /3)
#Panelowy: 
# xyplot(sp500.close, strip = TRUE, cut = list(number = 3, overlap = 0.333))

#install.packages("expsmooth")
# library(expsmooth)

## Wykresy autokorelacji:
lag.plot(sp500.close, lags = 12, do.lines = FALSE, main = "Wykres lag.plot dla SP500 z 20 lat")
# Raczej niesezonowe
par(mfrow = c(2, 1))
Acf(sp500.close, main = "Funkcja ACF")
Pacf(sp500.close, main = "Funkcja PACF")
# ACF wskazuje tutaj na silny trend
# Brak oscylacji w PACF sugeruje brak sezonowości
# Poniżej porównanie z białym szumem
bialy.szum <- as.ts(rnorm(200))
par(mfrow = c(2, 2))
Acf(sp500.close, main = "Funkcja ACF SP500")
Pacf(sp500.close, main = "Funkcja PACF Sp500")
Acf(bialy.szum, main = "Funkcja ACF WN")
Pacf(bialy.szum, main = "Funkcja PACF WN")

tsdisplay(sp500.close)
tsdisplay(bialy.szum)

## Box-Cox & differencing
# Transformacje boxa-Coxa zastosujemy celem ustabilizowania wariancji szeregu
# Różnicowanie będzie miało na celu usunięcie trendu z danych
sp500.sqrt <- BoxCox(sp500.close, lambda = 0.5)
sp500.log <- BoxCox(sp500.close, lambda = 0) #ln - z definicji
autolambda <- BoxCox.lambda(sp500.close) # 0.317 - auto lambda
sp500.autolambda <- BoxCox(sp500.close, lambda = autolambda)

par(mfrow = c(2, 2))
plot(sp500.close, main = "dane oryginalne")
grid()
plot(sp500.sqrt, main = "BoxCox dla lambda 0.5 - potęgowa")
grid()
plot(sp500.log, main = "BoxCox dla lambda 0 - logarytmiczna")
grid()
plot(sp500.autolambda, main = "BoxCox dla lambda automatyczne")
grid()
# Cieżko powiedzieć - dla niższych wartości lepsza logarytmiczna, ale dla wyższych potęgowa
# Lambda automatyczna 0.31 jest bardzo dobra
# Logarytmując dane możemy ustabilizować wariancję.
# Stabilizacja wariancji jest konieczna dla zastosowania modeli stacjonarnych.
# Arima(..., lambda)
# Ogólnie: tranformacja BoxaCoxa istotnie wpływa na konstrukcję przedziałów predykcyjnych,
# a nie na konstrukcje prognoz punktowych

## Differencing - różnicowanie
# cel: przekształcenie szeregu czasowego do postaci stacjonarnej
sp500.diff <- diff(sp500.close)
#par(mfrow = c(2, 1))
tsdisplay(sp500.close)
tsdisplay(sp500.diff)
# Spróbujmy róznicowania wielokrotnego celem eliminacji silnego trendu:
sp500.diff2 <- diff(sp500.close, differences = 2)
tsdisplay(sp500.diff2) 
# przy wielokrotnej robi się dziwnie patrząc na acf i pacf - wpływ negatywny
sp500.diff3 <- diff(sp500.close, differences = 3)
tsdisplay(sp500.diff3)
sp500.diff5 <- diff(sp500.close, differences = 5)
tsdisplay(sp500.diff5)

# różnicowanie lag=12 (sezonowe?)
sp500.diff48 <- diff(sp500.close, lag = 48) # trendy 4-letnie tzw. prezydenckie
sp500.diff48.diff1 <- diff(sp500.diff48, lag = 1)
tsdisplay(sp500.diff48.diff1)

# Zamiana formatu z xts na ts bo kompiler nie chciał inaczej
sp500_diff_ts <- ts(sp500.diff48.diff1, frequency = 1, start = c(2004,1,2))
spts <- na.omit(as.ts(sp500_diff_ts))
AR15 <- ar(spts, aic = FALSE, order.max = 15)
#reszty modelu
AR15.reszty <- AR15$resid
head(AR15.reszty, 25)

# Losowość reszt - test Ljung-Boxa
Box.test(AR15.reszty, lag = 48, type = "Ljung-Box") # istotny

plot(AR15.reszty)
Acf(AR15.reszty)

Arima.model <- Arima(sp500.close, order = c(0, 1, 48))
tsdiag(Arima.model, gof.lag = 48) # B. długo liczy !!!!!!!!!!!!!!!!!!

Arima.reszty <- residuals(Arima.model)
Box.test(Arima.reszty, type = "Ljung-Box", lag = 1)
Box.test(Arima.reszty, type = "Ljung-Box", lag = 12)
Box.test(Arima.reszty, type = "Ljung-Box", lag = 48)
# Reszty są losowe 

# Analiza normalności reszt:
hist(Arima.reszty, main = "histogram")
qqnorm(Arima.reszty, main = "wykres kwantylowy")
qqline(Arima.reszty)

# Zbudujmy model nr 2 i sprawdźmy dobroć
model2 <- Arima(sp500.close, order = c(48, 1, 0))
model1 <- Arima.model # (0, 1, 48)
model3 <- Arima(sp500.close, order = c(15, 1, 0))
model4 <- Arima(sp500.close, order = c(15, 1, 48)) # najlepszy AICc
summary(model1)
# AIC=48279.85   AICc=48280.81   BIC=48600.51
summary(model2)
# AIC=48273.29   AICc=48274.25   BIC=48593.96
summary(model3)
# AIC=48282.28   AICc=48282.38   BIC=48386.99
summary(model4)
# AIC=48252.46   AICc=48254.1   BIC=48671.29


# Różnicowanie automatyczne (testy statystyczne):
d.optymalne <- ndiffs(sp500.close) # 1
D.optymalne <- nsdiffs(sp500.close) # niesezonowe

# potem auto.arima()

## PROGNOZOWANIE
# prognoza random walk z dryfem po zastosowaniu transformcji logarytmicznej Boxa-Coxa
log.sp500.close.forecast.rwf <- rwf(x = BoxCox(sp500.close, lambda = 0), 
                                    drift = TRUE, h = 20)
plot(log.sp500.close.forecast.rwf, main = 
       "Prognoza na podstawie błądzenia losowego z dryfem logarytmiczna")

sp500.close.forecast.rwf <- rwf(x = sp500.close, drift = TRUE, h = 20, lambda = 0)
plot(sp500.close.forecast.rwf, main = 
       "Prognoza na podstawie błądzenia losowego z dryfem.")

































