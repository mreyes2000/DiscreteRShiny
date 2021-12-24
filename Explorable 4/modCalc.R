# modCalc.R - Modular Multiplication
library(numbers)

ModMult.makeList <- function(n) {
  Zn <- vector()
  for (i in 1:n-1) {
    if (coprime(i, n)) {
      Zn <- c(Zn, i)
    }
  }
  return(Zn)
}

ModMult.exp <- function(input, i, n) {
  holder <- 1
  for (j in 1:i+1) {
    holder <- (holder * input) %% n
  }
  return(holder)
}

ModMult.order <- function(input, len, n) {
  for (i in 1:len) {
    if (ModMult.exp(input, i, n) == 1) {
      return(i)
    }
  }
}

ModMult.vOrder <- function(v, n) {
  ordr <- vector()
  for (i in 1:length(v)) {
    ordr <- c(ordr, ModMult.order(v[i], length(v), n))
  }
  return(ordr)
}