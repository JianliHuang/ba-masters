setwd('c:/data/BUAN6357/HW_6'); source('prep.txt', echo=T)

library(tidyverse)
library(broom)
library(data.table)

raw <- read.table('Concrete_Data_wj.csv',header = T,sep = ',')
raw <- as.data.table(raw)
mod <- tidy(lm(data=raw, strength~.))
m1 <- as.data.table(mod)

#classic nb
d2f <- NULL
for(i in 1:500){
  set.seed(899796546)
  t <- replicate(500, sample(1030, replace=T), simplify=F)
  t1 <- t[[i]]
  t1 <- tidy(lm(strength ~ . , data = raw[t1,]))
  d2f <- rbind(d2f, t1)
}
d2f <- as.data.table(d2f)

set.seed(544546653)
t2 <- as.vector(replicate(500, sample(1030, replace=T), simplify=T))
x <- rep(1:500, each=1030)
dt1 <- data.table(i=t2, x=x)
d3dt <- dt1[ , tidy(lm(strength ~ . , data = raw[i,])), by=x]
d3dt$x <- NULL

#balanced
set.seed(518959292)
x <- sample(rep(1:1030, 500))
y <- rep(1:500, each=1030)
d4f <- data.table()
for(i in 1: max(y)){
  k <- x[y==i]
  t3 <- tidy(lm(strength ~ . , data = raw[k,]))
  d4f <- rbind(d4f, t3)
}

set.seed(212055872)
x <- sample(rep(1:1030, 500))
y <- rep(1:500, each = 1030)
dt2 <- data.table(x=x, y=y)
d5dt <- dt2[ , tidy(lm(strength ~ . , data = raw[x,])), by = y]
d5dt$y <- NULL

source('validate.txt', echo=T)
