##############################################
## Author:   Jason Parker
## Date:     2018-04-10
## Title:    lec8.R
## Purpose:  Stepwise regression and the EM algorithm
##############################################

## Import packages
library(data.table)
library(DBI)
library(RSQLite)
library(ddalpha)
library(tidyverse)
library(ggplot2)

## Prepare workspac
rm(list=ls(all=TRUE))
con <- dbConnect(SQLite(),'wooldridge.db')
bwght <- data.table(dbReadTable(con,'bwght'))
wage1 <- con %>% dbReadTable('wage1') %>% data.table
dbReadTable(con,'bwght_labels')
dbDisconnect(con)

summary(bwght)
table(bwght$fatheduc)
unique(bwght$fatheduc)
nrow(bwght)

summary(lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white,data=bwght))


## Try dropping fatheduc
bwght <- bwght[!is.na(motheduc)]
bwght$smokes <- as.numeric(bwght$cigs>0)
model1<- lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white, data=bwght[!is.na(fatheduc)])
summary(model1)

## Step-wise regression for variable selection using BIC (backwards induction)
model2   <- step(model1,k=log(nrow(bwght[!is.na(fatheduc)])))  #BIC
model2   <- step(model1)                        #AIC
summary(model2)

bwght$fe_imputed <- bwght$fatheduc
cntl <- cbind(bwght$cigs,bwght$motheduc,bwght$faminc,bwght$male,bwght$white)
clast <- rep(0,7)
for(i in 1:100){
  coefs <- coef(lm(log(bwght)~cntl+fe_imputed,data=bwght))
  pred <- (log(bwght$bwght)-coefs[1]-cntl%*%coefs[2:6])/coefs[7]
  pred <- ifelse(pred>18,18,pred)
  pred <- ifelse(pred<10,10,pred)
  pred <- ifelse(is.na(bwght$fatheduc),pred,bwght$fatheduc)
  bwght$fe_imputed <- pred
  if(sum((coefs-clast)^2)<1e-12)break
  clast <- coefs
}
i
model3 <- lm(log(bwght)~cigs+motheduc+fe_imputed+faminc+male+white,data=bwght)
summary(model3)

?step
model4 <- step(model3,k=log(nrow(bwght_imputed)))
model4 <- step(model3)
summary(model4)


con <- SQLite() %>% dbConnect('wooldridge.db')

con %>% dbDisconnect 

ggplot(wage1,aes(x=rep(1,nrow(wage1)),y=tenure))+geom_boxplot()

outliers <- function(x) {
  o <- order(x)
  x <- sort(x)
  bigx <- cbind(x,o)
  bnds <- quantile(x,c(0.25,0.75))+c(-1.5,1.5)*IQR(x)
  outp <- subset(bigx,x<bnds[1]|x>bnds[2])
  return(outp)
}
outliers(wage1$educ)
wage1[-outliers(wage1$educ)[,2]]

outliers <- function(x) {
  o <- order(x)
  x <- sort(x)
  z <- scale(x)
  bigx <- cbind(x,o)
  outp <- subset(bigx,abs(z)>-qnorm(1/NROW(x)))
  return(outp)
}
outliers()
-qnorm(1/NROW(wage1$educ))

xdata <- wage1[,.(educ,exper,tenure)]
wage1$mahalanobis_outliers <- as.numeric(mahalanobis(xdata,colMeans(xdata),var(xdata))>qchisq(1-1/nrow(xdata),df=ncol(xdata)))
?mahalanobis
wage1[mahalanobis_outliers==1]
wage1[mahalanobis_outliers==0]

lm(wage~educ+exper+tenure,data=wage1)
lm(wage~educ+exper+tenure,data=wage1[mahalanobis_outliers==0])
#lm(wage~educ+exper+tenure,data=wage1[mahalanobis_outliers==1]) # Don't do this; too little data

context2 <- wage1[,.(educ,exper,tenure)]
wage1$tukey <- ddalpha::depth.halfspace(as.matrix(context2),as.matrix(context2))
wage1$liu <- ddalpha::depth.simplicial(as.matrix(context2),as.matrix(context2),k=0.0001)
wage1$mahalanobis <- ddalpha::depth.Mahalanobis(as.matrix(context2),as.matrix(context2))
wage1[tukey<0.003,,][order(tukey),.(tukey,wage,educ,exper,tenure)]
wage1[liu<0.01,,][order(liu),.(liu,wage,educ,exper,tenure)]
wage1[mahalanobis<0.05,,][order(mahalanobis),.(mahalanobis,wage,educ,exper,tenure)]

summary(lm(wage~educ+exper+tenure,data=wage1))
summary(lm(wage~educ+exper+tenure,data=wage1[mahalanobis>=0.05,,]))
summary(lm(wage~educ+exper+tenure,data=wage1[tukey>=0.003,,]))
summary(lm(wage~educ+exper+tenure,data=wage1[liu>=0.01,,]))

