library(MASS)
library(tidyverse)
library(broom)

df <- Boston
hc <- hclust(dist(df))

x <- c(1:25)
hc.twss <- cutree(hc,k = x)

km <- c()
kmtwss <- c()
for (i in 1:25)
{
  temp <- kmeans(df,i)
  km <- cbind(km,temp$cluster)
  kmtwss <- rbind(kmtwss,temp$tot.withinss)
}


z <- hc.twss
z