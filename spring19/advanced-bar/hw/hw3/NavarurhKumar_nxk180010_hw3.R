#setwd('c:/data/BUAN6357/HW_3'); source('prep.txt', echo=T)

library(MASS)
library(tidyverse)
library(broom)

hc <- hclust(dist(Boston)^2)

hc.twss <- rep(0,25)
tb <- Boston

for (i in 1:25)
{
  t <- cutree(hc,i)
  tb <- Boston
  tb$clusters <- t
  sse <- tb %>% group_by(clusters) %>% do(wss=sum(scale(.,center = T,scale = F)^2)) %>% as.data.frame
  for(j in 1:nrow(sse))
  {
    hc.twss[i] <- hc.twss[i] + sse$wss[[j]][1]
  }
}

#source('validate.txt', echo=T)

#q1 hc.twss[3]/max(hc.twss)
#   0.1605755
#q2 hc.twss/max(hc.twss) < .1
#   6
#q3 hc.twss[3]
#   3115335.8
#q4 hc.twss[5]
#   2000643.4
#q5 diff(hc.twss)
#   
#q6 abs(diff(hc.twss/max(hc.twss)))
#   3