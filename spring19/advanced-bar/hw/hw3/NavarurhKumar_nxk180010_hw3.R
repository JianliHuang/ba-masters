setwd('c:/data/BUAN6357/HW_3'); source('prep.txt', echo=T)

#libs used
library(MASS)

#df declaration and hc dendro creation
df <- Boston
hc <- hclust(dist(df))

#defining cutree limits
x <- c(1:25)

#creating cutree's for all cluster counts
hc.clusts <- as.data.frame(cutree(hc,k = x))

#temping df
dft <- df
#temp holder for wss values
temp <- matrix(0, nrow = 25, ncol = 25)

#looping to evaluate individual wss values
for (i in 1:25)
{
  dft$clust <- hc.clusts[,i]
  for (j in 1:i)
  {
    dfts <- dft[which(dft$clust == j),]
    a <- as.matrix(dist(dfts))
    b <- a^2
    temp[i,j] <- sum(b)/2
  }
}

#temp holder for total wss
hc.twss <- rep(-1,25)

#simple summing to get twss values
for(i in 1:25)
{
  hc.twss[i] <- sum(temp[i,])
}

source('validate.txt', echo=T)