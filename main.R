library(fArma)
library(purrr)
library(dplyr)
library(quantmod)
library(TTR)
library(xts)
library(ggplot2)

raw <- quantmod::getSymbols("SPY", src = "yahoo", auto.assign = FALSE)
returns <- na.omit(xts::diff.xts(log(raw[,4]), lag=55))
hs <- purrr::map(300:nrow(returns), function(until){
  span <- xts::first(returns, until)
  h <- fArma::rsFit(xts::last(span, 21))
  h@hurst$H
})
df <- data.frame(matrix(unlist(hs), nrow=NROW(hs), byrow=T))
#qplot(x = index(df), y = df[,1], geom = "line")
qplot(df)
tail(df, n = 1)