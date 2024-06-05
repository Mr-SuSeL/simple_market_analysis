library(forecast)
# install.packages("quantmod")
library(quantmod)

getSymbols(Symbols = "^GSPC", src = "yahoo", from = "2014-01-01", to = "2024-06-03")
#GSPC - GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted

