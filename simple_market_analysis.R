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

















