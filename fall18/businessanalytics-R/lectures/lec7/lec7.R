######################################
## Author:   Jason Parker
## Date:     2018-10-24
## Title:    lec7.R
## Purpose:  Dimension reduction with Boston housing data
##           and add some factor models with Fama French
######################################

rm(list=ls())

## Import packages
library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(tseries)

# Connect to the database to get the data
con <- SQLite() %>% dbConnect('wooldridge.db')
hprice2 <- con %>% dbReadTable('hprice2') %>% data.table
con %>% dbDisconnect

summary(hprice2)
summary(scale(hprice2))
hprice2 <- hprice2 %>% dplyr::select(-index,-lprice,-lnox,-lproptax)
xdata <- hprice2 %>% dplyr::select(-nox,-price)
summary(xdata)
cov(xdata)


#######################################
## PCA dimension reduction
#######################################
eratio <- function(wss) { # USE MINUS 1 FOR PCA
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calucluate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}
plot.pca <- function(wss) {
  plot(0:(NROW(wss)-1), wss, type="b", xlab="Number of PCs", ylab="Eigenvalues")
}
pca.select <- function(pca.model,n.max=10){
  eigen <- pca.model$sdev^2
  if(NROW(eigen)>=n.max) eigen<-eigen[1:n.max]
  plot.pca(eigen)
  return(eratio(eigen)-1)
}
parker.sul <- function(data,test.var,max.factor=NULL){
  residual.data <- data.frame(data)
  n <- ncol(residual.data)
  if(is.null(max.factor))max.factor<-max(pca.select(prcomp(residual.data,scale=TRUE)))+2
  for(c in 1:n){
    col <- residual.data[,c]
    if(is.null(ncol(test.var))){ # Single variable case
      model <- lm(col~test.var)
    }else{ # Multiple variables
      subdata <- cbind(col,test.var)
      model <- lm(col~.,data=subdata)
    }
    residual.data[,c] <- residuals(model) # Obtain residuals
  }
  return(pca.select(prcomp(residual.data,scale=TRUE),n.max=max.factor))
}
rep.kpss <- function(series,alpha=0.05,dmax=5){
  diff <- 0
  for(i in 1:dmax){
    suppressWarnings(pval <- kpss.test(series,null="Level")$p.value)
    if(pval>=alpha){
      return(c(diff,0,pval))
    }
    suppressWarnings(pval <- kpss.test(series,null="Trend")$p.value)
    if(pval>=alpha){
      return(c(diff,1,pval))
    }
    diff <- diff + 1
    series <- diff(series)
  }
  return(NULL)
}

pca <- prcomp(xdata,scale=TRUE)
screeplot(pca,type="lines") # looks like there are 2 principal components
pca.select(pca)

pca$rotation[,1:3]
pca$x[,1:3]
pca$x
pca$rotation[,1]
pca$x[,1]  # z variables ([,1] means use only z_1)
head(factors)
summary(factors)
var(factors)

hprice2$fact <- scale(pca$x[,1])
var(hprice2$fact)
# modeling with the factors
model0 <- lm(log(price)~log(nox),data=hprice2)
summary(model0) 
model1 <- lm(log(price)~fact,data=hprice2)
summary(model1) 
model2 <- lm(log(price)~log(nox)+fact,data=hprice2)
summary(model2) 
c(AIC(model0),AIC(model1),AIC(model2))
c(BIC(model0),BIC(model1),BIC(model2))

#A 1% increase in nox =>  0.15904% increase in price controlling for the first principal component

french <- fread('french.csv')
summary(french)
french$Date <- as.Date(french$Date)
french$Date
for(c in 2:50){
  series <- data.frame(french)[,c]
  print(rep.kpss(series))
}
panel <- french[,2:50]
pca <- prcomp(panel,scale=TRUE)
pca.select(pca)

french$factor <- pca$x[,1]
french$factor <- scale(french$factor)
colnames(french)
pca$rotation[,1]

modely <- lm(Fin~factor,data=french)
summary(modely)
modelx <- lm(Gold~factor,data=french)
summary(modelx)

french <- fread('french.csv')
sp500 <- fread('sp500.csv')
djia <- fread('djia.csv')
nasdaq <- fread('nasdaq.csv')
factors <- fread('factors.csv')
french <- merge(french,sp500,all=FALSE)
french <- merge(french,djia,all=FALSE)
french <- merge(french,nasdaq,all=FALSE)
french <- merge(french,factors,all=FALSE)
french$Date <- as.Date(french$Date)
panel <- french[,2:50]
french$factor <- scale(prcomp(panel,scale=TRUE)$x[,1])

ggplot(french,aes(x=Date,y=factor)) + geom_line()
ggplot(french,aes(x=Date,y=sp500)) + geom_line()
ggplot(french,aes(x=Date,y=djia)) + geom_line()
ggplot(french,aes(x=Date,y=nasdaq)) + geom_line()
ggplot(french,aes(x=Date,y=Mkt)) + geom_line()

summary(lm(Fin~factor,data=french))
summary(lm(Fin~sp500,data=french))
summary(lm(Fin~djia,data=french))
summary(lm(Fin~nasdaq,data=french))

parker.sul(panel,french$factor)
parker.sul(panel,french$sp500)
parker.sul(panel,french$djia)
parker.sul(panel,french$nasdaq)
parker.sul(panel,french[,.(djia,sp500,nasdaq)])
parker.sul(panel,french[,.(Mkt,SMB,HML)])
parker.sul(panel,french[,.(Mkt,SMB)])
parker.sul(panel,french[,.(SMB,HML)])
parker.sul(panel,french[,.(Mkt,HML)])
parker.sul(panel,french$Mkt)
parker.sul(panel,french$HML)

