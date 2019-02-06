library(MASS)
#library(tidyverse)
#library(broom)

#df declaration and hc dendro creation
df <- Boston
hc <- hclust(dist(df))

#defining cutree limits
x <- c(1:25)
#creating cutree's for all cluster counts
hc.clusts <- cutree(hc,k = x)

#function to eval the centroids based on cutree
clust.centroid <- function(i, dat, clusters) {
  ind <- (clusters == i)
  colMeans(dat[ind,])
}

#centroid list empty init
centroid.list <- list()

#looping to get cluster centres for different cutree cluster counts
for(i in 1:25)
{
  centroid.list[[i]] <- sapply(unique(hc.clusts[,i]), clust.centroid, df, hc.clusts[,i])
}


#finding the twss for diff cluster counts from kmeans
km <- c()
kmtwss <- c()
for (i in 1:25)
{
  temp <- kmeans(df,i)
  km <- cbind(km,temp$cluster)
  kmtwss <- rbind(kmtwss,temp$tot.withinss)
}
