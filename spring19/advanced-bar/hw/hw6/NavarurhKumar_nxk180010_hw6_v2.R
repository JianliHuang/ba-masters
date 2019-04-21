library(tidyverse)
library(broom)
library(data.table)

raw <- read.table('Work/ba-masters/spring19/advanced-bar/hw/hw6/Concrete_Data_wj.csv',header = T,sep = ',')
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

#6b
mean(d3dt$estimate[which(d3dt$term == 'water')])
0.674
1.1503494
1.959964

mean(d3dt$estimate[which(d3dt$term == 'water')])
+ 1.959964*sd(d3dt$estimate[which(d3dt$term == 'water')])

mean(d2f$estimate[which(d2f$term == 'slag')])
- 1.959964*sd(d2f$estimate[which(d2f$term == 'slag')])

mean(d5dt$estimate[which(d5dt$term == 'fine_agg')])
+ 1.1503494*sd(d5dt$estimate[which(d5dt$term == 'fine_agg')])

mean(d2f$estimate[which(d2f$term == 'ash')])
+ 1.1503494*sd(d2f$estimate[which(d2f$term == 'ash')])

mean(d2f$estimate[which(d2f$term == 'days')])
- 1.959964*sd(d2f$estimate[which(d2f$term == 'days')])