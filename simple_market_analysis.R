library(forecast)
# install.packages("quantmod")
library(quantmod)
library(stats)
#install.packages("tseries")
library(tseries)
#install.packages("expsmooth")
# library(expsmooth)
setwd("C:/First_data_recognition/analyse_market_acf_pacf")
# Pobranie danych
getSymbols(Symbols = "^GSPC", src = "yahoo", from = "2004-01-01", to = "2024-06-03")
# Pobranie danych jako time series
#getSymbols(Symbols = "^GSPC", src = "yahoo", from = "2014-01-01", to = "2024-06-03",
#           return.class = "ts")
#GSPC - GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted

head(GSPC)
str(GSPC)
sp500.close <- GSPC[, "GSPC.Close"]
sp500.close[ 1:10, ]

plot(sp500.close, main = "Wykres S&P 500 - 20 lat")
class(sp500.close) # xts, zoo

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
sp500.sqrt <- BoxCox(sp500.close, lambda = 0.5) # potęgowy
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
# ------------------- wniosek: MA(22) oraz AR(30) i/lub AR(15) -----------------

sp500.diff.diff <- diff(sp500.diff, lag = 1)
tsdisplay(sp500.diff.diff)
# podwojne różnicowanie nic nie poprawiło - zostaje przy pojedynczym

# Różnicowanie automatyczne (testy statystyczne):
d.optymalne <- ndiffs(sp500.close) # 1
D.optymalne <- nsdiffs(sp500.close) # niesezonowe

# AR(p)
# Zamiana formatu z xts na ts bo kompiler nie chciał inaczej
sp500_ts <- ts(sp500.close, frequency = 1, start = c(2004,1,2))
# ts po różnicowaniu wrzucam do AR
sp500_diff_ts <- ts(sp500.diff, frequency = 1, start = c(2004,1,2))
spts <- na.omit(as.ts(sp500_diff_ts))

# Augmented Dickey-Fuller Test:
adf.test(spts) # szereg po zróżnicowaniu jest stacjonarny p < 0.05

AR15 <- ar(spts, aic = FALSE, order.max = 15)
#reszty modelu
AR15.reszty <- AR15$resid
head(AR15.reszty, 25)

# Losowość reszt - test Ljung-Boxa
Box.test(AR15.reszty, lag = 15, type = "Ljung-Box") # istotny
plot(AR15.reszty)
Acf(AR15.reszty)

# MA(q)
MA22 <- ma(spts, order = 22)

plot(spts)
lines(MA22,col="red")

# ARIMA(p, d, q)
Arima.model <- Arima(sp500.close, order = c(0, 1, 22))
tsdiag(Arima.model, gof.lag = 22) # czasem długo liczy 

Arima.reszty <- residuals(Arima.model)
Box.test(Arima.reszty, type = "Ljung-Box", lag = 1)
Box.test(Arima.reszty, type = "Ljung-Box", lag = 22)
Box.test(Arima.reszty, type = "Ljung-Box", lag = 48)
# Reszty są losowe bo p-value > 0,05

# Analiza normalności reszt:
hist(Arima.reszty, main = "histogram")
qqnorm(Arima.reszty, main = "wykres kwantylowy")
qqline(Arima.reszty)

# Zbudujmy model nr 2 i sprawdźmy dobroć
model1 <- Arima.model # (0, 1, 22)
model2 <- Arima(sp500.close, order = c(22, 1, 0))
model3 <- Arima(sp500.close, order = c(15, 1, 0))
# Podmiana danych z xts na ts bo inaczej nie ruszał model4
#model4 <- Arima(sp500.close, order = c(15, 1, 22)) # najlepszy AICc
model4 <- Arima(spts, order = c(15, 1, 22)) # najlepszy AICc
summary(model1)
# AIC=48279.85   AICc=48280.81   BIC=48600.51
summary(model2)
# AIC=48281.6   AICc=48281.82   BIC=48432.12
summary(model3)
# AIC=48282.28   AICc=48282.38   BIC=48386.99
summary(model4)
# AIC=48236.33   AICc=48236.91   BIC=48485

# Metryka dokładności:
accuracy(model1)
accuracy(model2)
accuracy(model3) 
accuracy(model4) # najlepsze wyniki
#               ME     RMSE      MAE       MPE MAPE      MASE   ACF1
#            0.3619275 26.26304 16.37449 -Inf  Inf 0.6774011 -0.0004927042

# auto.arima()
arima.optym.aicc <- auto.arima(sp500.close, ic = "aicc")
arima.optym.aicc
# ARIMA(0,1,2) with drift 
# AIC=48399.57   AICc=48399.58   BIC=48425.75
accuracy(arima.optym.aicc)

arima.optym.aic <- auto.arima(sp500.close, ic = "aic", stepwise = FALSE)
arima.optym.aic
# ARIMA(4,1,1) with drift
# AIC=48337.63   AICc=48337.66   BIC=48383.44
accuracy(arima.optym.aic)

arima.optym.long <- auto.arima(sp500.close, max.p = 15, max.q = 48)
arima.optym.long
# ARIMA(0,1,2) with drift 
accuracy(arima.optym.long) 

## ------------------- PROGNOZOWANIE -------------------------------------------
# prognoza random walk z dryfem po zastosowaniu transformcji logarytmicznej Boxa-Coxa

par(mfrow = c(2, 1))
#par(mfrow = c(1, 1))
log.sp500.close.forecast.rwf <- rwf(x = BoxCox(sp500.close, lambda = 0), 
                                   drift = TRUE, h = 2500)
plot(log.sp500.close.forecast.rwf, main = 
       "Prognoza na podstawie błądzenia losowego z dryfem logarytmiczna")
# ta prognoza powyżej wydaje się bardzo dobra

sp500.close.forecast.rwf <- rwf(x = sp500.close, drift = TRUE, h = 2500, lambda = 0)
plot(sp500.close.forecast.rwf, main = 
       "Prognoza na podstawie błądzenia losowego z dryfem.")

# Wiemy że mamy do czynienia z danymi z dryfem
# Zróbmy random walk forecast z dryfem dla 95% przedziału ufności
par(mfrow = c(3, 1))
sp500.rwf.95 <- rwf(sp500.close, drift = TRUE, h = 2500, level = 0.95)
plot(sp500.rwf.95, xlim = c(5000, 5220), ylim = c(4000, 6000)) # niesatysfakcjonujące
# rwf dla 80% i 95% przedziałów ufności:
sp500.rwf.8095 <- rwf(sp500.close, drift = TRUE, h = 2500, level = c(0.8, 0.95))
plot(sp500.rwf.8095, xlim = c(5000, 5220), ylim = c(4000, 6000))
# fanplot:
sp500.fan.rwf <- rwf(sp500.close, drift = TRUE, h = 2500, fan = TRUE)
plot(sp500.fan.rwf,xlim = c(5000, 5220), ylim = c(4000, 6500), 
     main = "Wykres wachlarzowy dla 0.5-0.99 poz. ufności")

# Analiza reszt (RWF):
reszty.1 <- residuals(log.sp500.close.forecast.rwf)
reszty.2 <- residuals(sp500.close.forecast.rwf)
reszty.3 <- residuals(sp500.rwf.95)
reszty.4 <- residuals(sp500.rwf.8095)
reszty.5 <- residuals(sp500.fan.rwf)

par(mfrow = c(3, 1))
plot(reszty.1, main = "log.sp500.close.forecast.rwf")
plot(reszty.2, main = "sp500.close.forecast.rwf")
plot(reszty.3, main = "sp500.rwf.95")
# ----------------------------------
#plot(reszty.4, main = "sp500.rwf.8095")
#plot(reszty.5, main = "sp500.fan.rwf")

# ACF dla reszt:
par(mfrow = c(3, 2))
Acf(reszty.1, lag.max = 30, main = "Reszty dla log.sp500.close.forecast.rwf")
hist(reszty.1, main = "Reszty dla log.sp500.close.forecast.rwf")
Acf(reszty.2, lag.max = 30, main = "Reszty dla sp500.close.forecast.rwf")
hist(reszty.2, main = "Reszty dla sp500.close.forecast.rwf")
Acf(reszty.3, lag.max = 30, main = "Reszty dla sp500.rwf.95")
hist(reszty.3, main = "Reszty dla sp500.rwf.95")

# Test białoszumowości Boxa-Ljunga:
Box.test(reszty.1, lag = 10, type = "Ljung-Box")
Box.test(reszty.2, lag = 10, type = "Ljung-Box")
Box.test(reszty.3, lag = 10, type = "Ljung-Box")
# Test odrzucił hipotezę o losowości reszt p-value < 0.05

## PROGNOZOWANIE metodyką ARIMA ------------------------------------------------
# Z analizy ACF(MA) i PACF(AR) wyszło nam: AR(15) i MA(22), I = 1
# modele: (15,1,0) oraz (0, 1, 22) a także (15, 1, 22)
ARIMA.model1 <- Arima(sp500.close,
                      order = c(15, 1, 0), include.drift=TRUE)
ARIMA.model2 <- Arima(sp500.close,
                      order = c(0, 1, 22), include.drift=TRUE)
ARIMA.model3 <- Arima(sp500.close,
                      order = c(15, 1, 22), include.drift=TRUE)
ARIMA.model4 <- arima.optym.aicc 

# Prognoza na 10 x 250 dni:
ARIMA.model1.prognoza10 <- forecast(ARIMA.model1, h = 2500)
ARIMA.model2.prognoza10 <- forecast(ARIMA.model2, h = 2500)
ARIMA.model3.prognoza10 <- forecast(ARIMA.model3, h = 2500)
ARIMA.model4.prognoza10 <- forecast(ARIMA.model4, h = 2500)

par(mfrow = c(2, 2))
plot(ARIMA.model1.prognoza10)
plot(ARIMA.model2.prognoza10)
plot(ARIMA.model3.prognoza10)
plot(ARIMA.model4.prognoza10) # Prognoza auto-arimą:

# Porównanie prognoz - 10 lat:
par(mfrow = c(1, 1))
ts.plot(ARIMA.model1.prognoza10$mean, 
        ARIMA.model2.prognoza10$mean,
        ARIMA.model3.prognoza10$mean,
        ARIMA.model4.prognoza10$mean,
        main = "Porównanie prognoz dla cen zamknięcia S&P500",
        col = c("black", "red", "green", "blue"))
grid()
legend("topleft", 
       legend = c("ARIMA (15, 1, 0)", "ARIMA (0, 1, 22)", "ARIMA (15, 1, 22)", "auto-arima"),
       col = c("black", "red", "green", "blue"), bg = "white")


# Modele z wyodrębnionym trendem deterministycznym: 
sp500_ts <- tsclean(sp500_ts)
plot(sp500_ts)
# decompose(x, type = c("additive", "multiplicative"), filter = NULL)
decomp <- decompose(sp500_ts, type = "additive", filter = "MA")  
plot(decomp) 
# Błąd w poleceniu ... :
# szereg czasowy nie posiada okresów lub ma ich mniej niż 2

# Skoro nie działa spróbujmy funkcję tslm()
sp500_ts_trend <- tslm(sp500_ts ~ trend)
summary(sp500_ts_trend)

sp500_ts_season <- tslm(sp500_ts ~ season)
# Non-seasonal data cannot be modelled using a seasonal factor

# Pomińmy sezonowość:
# Model liniowy
sp500.lin <- tslm(sp500_ts ~ trend)
# Model kwadratowy
sp500.kwadrat <- tslm(sp500_ts ~ I(trend^2))
# Model logarytmiczny
# Box-Cox z 0 -> log
sp500.log <- tslm(sp500_ts ~ trend, lambda = 0)

summary(sp500.lin)
summary(sp500.kwadrat)
summary(sp500.log)



























