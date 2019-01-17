######################################
## Author:   Jason Parker
## Date:     2018-10-17
## Title:    lec6.R
## Purpose:  Clustering with Boston housing data
######################################

rm(list=ls())

## Import packages
library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)

# Connect to the database to get the data
con <- SQLite() %>% dbConnect('wooldridge.db')
hprice2 <- con %>% dbReadTable('hprice2') %>% data.table
bwght <- con %>% dbReadTable('bwght') %>% data.table
con %>% dbDisconnect
summary(hprice2)
summary(scale(hprice2))
hprice2 <- hprice2 %>% select(-index,-lprice,-lnox,-lproptax)

summary(hprice2)

#######################################
## k-means Clustering
#######################################

?kmeans
set.seed(5)
runif(10)

kmeans.wss <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  return(wss)
}
eratio <- function(wss) {
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calucluate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}
plot.wss <- function(wss) {
  plot(1:NROW(wss), wss, type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}
wss <- hprice2 %>% scale %>% kmeans.wss
plot.wss(wss)
eratio(wss)

# Knowing the correct number of clusters to use, run the model
set.seed(1)
model <- kmeans(scale(hprice2),centers=2,nstart=10)
model$centers
groupsk <- model$cluster
groupsk
table(groupsk)

#######################################
## Hierarchical Clustering
#######################################

hclust.wss <- function(data,model=hclust(dist(data)),maxclu=10) {
  # Within sum of squares function for hierarchical clustering
  wss <- rep(NA,maxclu)
  for(i in 1:maxclu){
    gps <- cutree(model,i) # get groups for i clusters
    means <- data[,lapply(.SD,mean),by=gps] # get means
    demeaned <- data-means[gps,2:(ncol(data)+1)] # difference data from means
    wss[i] <- sum(demeaned^2) # sum squared distanaces
  }
  return(wss)
}

# Main hierarchical clustering approach
wss <- hprice2 %>% hclust.wss %>% plot.wss
plot.wss(wss)
eratio(wss)

# Alternative choices for distances between clusters
plot.wss(hclust.wss(hprice2,model=hclust(dist(hprice2),method="complete")))
plot.wss(hclust.wss(hprice2,model=hclust(dist(hprice2),method="average")))
plot.wss(hclust.wss(hprice2,model=hclust(dist(hprice2),method="median")))
plot.wss(hclust.wss(hprice2,model=hclust(dist(hprice2),method="ward.D")))
plot.wss(hclust.wss(hprice2,model=hclust(dist(hprice2),method="ward.D2")))
plot.wss(hclust.wss(hprice2,model=hclust(dist(hprice2),method="mcquitty")))
plot.wss(hclust.wss(hprice2,model=hclust(dist(hprice2)^2,method="centroid")))

# Model dendrogram
model <- hclust(dist(hprice2))
plot(model)
rect.hclust(model,k=7,border="red")
rect.hclust(model,k=6,border="purple")
rect.hclust(model,k=5,border="blue")
rect.hclust(model,k=4,border="green")
rect.hclust(model,k=3,border="yellow")
rect.hclust(model,k=2,border="orange")
cutree(model,k=2)  # Finding clusters from the model

#######################################
## Applying Clustering
#######################################

hprice2$kmean <- kmeans(scale(hprice2),centers=2,nstart=10)$cluster
hprice2$hier <- cutree(hclust(dist(hprice2)),k=2)
hprice2[order(kmean),lapply(.SD,mean),by=kmean] #means for kmeans
hprice2[order(hier),lapply(.SD,mean),by=hier] #means for hier
table(hprice2$hier,hprice2$kmean) # Two-way table to see relationship

# Create dummies just in case
hprice2$kmean1 <- as.numeric(hprice2$kmean==1)
hprice2$kmean2 <- as.numeric(hprice2$kmean==2)
hprice2$hier1 <- as.numeric(hprice2$hier==1)
hprice2$hier2 <- as.numeric(hprice2$hier==2)

# k-means clustering models
# Pooled:
lm(log(price)~log(nox)+rooms+stratio,data=hprice2) %>% summary
# Within:
lm(log(price)~as.factor(kmean)+log(nox)+rooms+stratio-1,data=hprice2) %>% summary

# hierarchical clustering models
# Pooled:
lm(log(price)~log(nox)+rooms+stratio,data=hprice2) %>% summary
# Within:
lm(log(price)~as.factor(hier)+log(nox)+rooms+stratio-1,data=hprice2) %>% summary