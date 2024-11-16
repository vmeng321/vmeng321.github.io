library(tidyverse)
set.seed(500)
n = 40
B = 1000

d.bar.paired <- rep(NA, B)
##### paired data
for(i in 1:B){
  numTrts <- rep(rpois(n, lambda = 3), times=2)
 # hist(numTrts)
  e.standard <- rnorm(n*2, mean=0, sd=4)
  e.mini <- rnorm(n*2, mean=0, sd=2.3)
  is.standard <- rep(c(1,0),each=n)
  weight <- 17 + 30*is.standard +
    numTrts*2+ e.mini*(!is.standard) + e.standard*(is.standard)
  dd <- tibble(is.standard, weight, numTrts)
  dd |> group_by(is.standard) |> summarise(mean(weight), sd(weight))
  d.bar.paired[i]<- mean(weight[1:n]-weight[(n+1):(2*n)])
}



d.bar.indep <- rep(NA, B)
##### indep data
for(i in 1:B){
  numTrts <- rpois(2*n, lambda = 3)
  # hist(numTrts)
  e.standard <- rnorm(n*2, mean=0, sd=4)
  e.mini <- rnorm(n*2, mean=0, sd=2.3)
  is.standard <- rep(c(1,0),each=n)
  weight <- 17 + 30*is.standard +
    numTrts*2+ e.mini*(!is.standard) + e.standard*(is.standard)
  dd.indep <- tibble(is.standard, weight, numTrts)
  dd.indep |> group_by(is.standard) |> summarise(mean(weight), sd(weight))
  d.bar.indep[i]<- mean(weight[1:n])-mean(weight[(n+1):(2*n)])
}

sd(d.bar.indep)
sd(d.bar.paired)

