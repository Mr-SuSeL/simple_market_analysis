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

##  Korekta kalendarzowa
#srednia.liczba.dni <- 365.25 / 12
#liczba.dni.w.miesiacu <- monthdays(sp500.close)


## Box-Cox & differencing
# Transformacje boxa-Coxa zastosujemy celem ustabilizowania wariancji szeregu
# Różnicowanie będzie miało na celu usunięcie trendu z danych
sp500.sqrt <- BoxCox(sp500.close, lambda = 0.5)
sp500.log <- BoxCox(sp500.close, lambda = 0) #ln - z definicji
par(mfrow = c(3, 1))
plot(sp500.close, main = "dane oryginalne")
grid()
plot(sp500.sqrt, main = "BoxCox dla lambda 0.5 - potęgowa")
grid()
plot(sp500.log, main = "BoxCox dla lambda 0 - logarytmiczna")
grid()
# Cieżko powiedzieć - dla niższych wartości lepsza logarytmiczna, ale dla wyższych potęgowa
# Logarytmując dane możemy ustabilizować wariancję.
# Stabilizacja wariancji jest konieczna dla zastosowania modeli stacjonarnych.

































