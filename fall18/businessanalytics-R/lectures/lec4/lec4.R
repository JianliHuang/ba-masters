######################################
## Author:   Jason Parker
## Date:     2018-06-12
## Title:    lec4.R
## Purpose:  Panel data analysis and model selection
######################################

rm(list=ls())

## Import packages
library(data.table)
library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(plm)
library(margins)

# Connect to the database to get the data
con <- SQLite() %>% dbConnect('wooldridge.db')
wagepan <- con %>% dbReadTable('wagepan') %>% data.table
wage1 <- con %>% dbReadTable('wage1') %>% data.table
mroz <- con %>% dbReadTable('mroz') %>% data.table
crime1 <- con %>% dbReadTable('crime1') %>% data.table
con %>% dbDisconnect

####################
## Model selection using wage1 data
####################

# variable name   type    format     label      variable label
# wage            float   %8.2g                 average hourly earnings
# educ            byte    %8.0g                 years of education
# exper           byte    %8.0g                 years potential experience
# tenure          byte    %8.0g                 years with current employer
summary(wage1)

# As factor demostration
wage1$occ <- wage1[,20]+2*wage1[,21]+3*wage1[,22]
wage1$occ <- as.factor(wage1$occ)
wage1$occ
# Within-groups model:
lm(log(wage)~educ+exper+tenure+occ,data=wage1) %>% summary
# Pooled model:
lm(log(wage)~educ+exper+tenure,data=wage1) %>% summary

## AIC/BIC demonstration
model1 <- lm(log(wage)~educ+exper+tenure,data=wage1)
summary(model1) 
AIC(model1)
BIC(model1)
c(AIC(model1),BIC(model1))
ic <- function(x){
  return(c(AIC(x),BIC(x)))
}
ic(model1)

# Try quadratic models to see if the BIC improves
lm(log(wage)~educ+exper+tenure+I(educ^2),data=wage1) %>% ic
lm(log(wage)~educ+exper+tenure+I(exper^2),data=wage1) %>% ic
lm(log(wage)~educ+exper+tenure+I(tenure^2),data=wage1) %>% ic

# Complete quadratic model
lm(log(wage)~educ+exper+tenure+I(educ^2)+I(exper^2)+I(tenure^2),data=wage1) %>% ic
model2 <- lm(log(wage)~educ+exper+tenure+I(educ^2)+I(exper^2)+I(tenure^2),data=wage1)

lm(log(wage)~educ+exper+tenure+I(educ^2)+I(exper^2)+I(tenure^2)+I(educ^3),data=wage1) %>% ic
lm(log(wage)~educ+exper+tenure+I(educ^2)+I(exper^2)+I(tenure^2)+I(exper^3),data=wage1) %>% ic
lm(log(wage)~educ+exper+tenure+I(educ^2)+I(exper^2)+I(tenure^2)+I(tenure^3),data=wage1) %>% ic

lm(log(wage)~educ+exper+tenure+I(educ^2)+I(exper^2)+I(tenure^2)+I(educ^3)+I(exper^3)+I(tenure^3),data=wage1) %>% ic

lm(log(wage)~(educ+exper+tenure)^2+I(educ^2)+I(exper^2)+I(tenure^2),data=wage1) %>% summary

lm(log(wage)~(educ+exper+tenure)^2+I(educ^2)+I(exper^2)+I(tenure^2),data=wage1) %>% ic
model2 %>% ic
model2 %>% coeftest(.vcov=vcovHC)
lm(log(wage)~exper+tenure+I(educ^2)+I(exper^2),data=wage1) %>% ic


lm(log(wage)~exper+tenure+I(educ^2)+I(exper^2),data=wage1) %>% ic

model3 <- lm(log(wage)~I(educ^2)+exper+I(exper^2)+tenure,data=wage1)
model3 %>% coeftest(.vcov=vcovHC)

## Plotting relationships
plot1 <- ggplot(wage1, aes(x=educ,y=log(wage))) + geom_point()
plot1 <- plot1 + geom_line(aes(y=predict(lm(log(wage)~educ,data=wage1))),color="red",size=2)
plot1 <- plot1 + geom_line(aes(y=predict(lm(log(wage)~educ+I(educ^2),data=wage1))),color="blue",size=2)
plot1 <- plot1 + geom_line(aes(y=predict(lm(log(wage)~educ+I(educ^2)+I(educ^3),data=wage1))),color="green",size=2)
plot1 <- plot1 + theme_bw()
plot1
plot2 <- ggplot(wage1, aes(x=exper,y=log(wage))) + geom_point()
plot2 <- plot2 + geom_line(aes(y=predict(lm(log(wage)~exper,data=wage1))),color="red",size=2)
plot2 <- plot2 + geom_line(aes(y=predict(lm(log(wage)~exper+I(exper^2),data=wage1))),color="blue",size=2)
plot2 <- plot2 + geom_line(aes(y=predict(lm(log(wage)~exper+I(exper^2)+I(exper^3),data=wage1))),color="green",size=2)
plot2 <- plot2 + theme_bw()
plot2
plot3 <- ggplot(wage1, aes(x=tenure,y=log(wage))) + geom_point()
plot3 <- plot3 + geom_line(aes(y=predict(lm(log(wage)~tenure,data=wage1))),color="red",size=2)
plot3 <- plot3 + geom_line(aes(y=predict(lm(log(wage)~tenure+I(tenure^2),data=wage1))),color="blue",size=2)
plot3 <- plot3 + geom_line(aes(y=predict(lm(log(wage)~tenure+I(tenure^2)+I(tenure^3),data=wage1))),color="green",size=2)
plot3 <- plot3 + theme_bw()
plot3

# Step-wise selection
modelx <- lm(log(wage)~educ+exper+tenure+I(educ^2)+I(exper^2)+I(tenure^2)+I(educ^3)+I(exper^3)+I(tenure^3),data=wage1)
summary(step(modelx)) # AIC
summary(step(modelx,k=log(nrow(wage1)))) #BIC even if says AIC, it's a lie
step(modelx,k=log(nrow(wage1))) %>% ic
model3 %>% ic

# Polynomial models
lm(log(wage)~poly(educ,degree=2),data=wage1) %>% summary
lm(log(wage)~poly(educ,degree=2)+poly(exper,degree=2),data=wage1) %>% summary
lm(log(wage)~polym(educ,exper,degree=2),data=wage1) %>% summary
# Doesn't work because polym is one variable
# lm(log(wage)~polym(educ,exper,tenure,degree=3),data=wage1) %>% step %>% summary

# # Poly function with variable selection (insane)
# polym_jp <- function(...,data,degree=1) {
#   attach(data)
#   polly <- polym(...,degree=degree) %>% data.table
#   detach(data)
#   return(polly)
# }
# xdata <- polym_jp(educ,exper,tenure,data=wage1,degree=3)
# str <- "log(wage)~"
# for(i in 1:19) str <- paste(str,"xdata$\"",colnames(xdata)[i],"\"+",sep='')
# str <- substr(str,1,nchar(str)-1)
# print(str)
# lm(str,data=wage1) %>% step(k=log(nrow(wage1))) %>% summary


####################
## Panel data analysis with the wage pan data.
####################


### NOTE YOU NEED THIS NEXT LINE TO DECLARE THE DATA IS PANEL
wagepan <- wagepan %>% plm.data(indexes=c('nr','year'))
class(wagepan)


summary(wagepan)
head(wagepan)

# Model %change in wage 
plm(lwage~educ+exper,model="pooling",data=wagepan) %>% summary
plm(lwage~educ+exper,model="within",data=wagepan) %>% summary
plm(lwage~educ+exper,model="within",effect="individual",data=wagepan) %>% summary
plm(lwage~educ+exper,model="within",effect="time",data=wagepan) %>% summary
plm(lwage~educ+exper,model="within",effect="twoways",data=wagepan) %>% summary
plm(lwage~educ+exper+as.factor(year),model="within",data=wagepan) %>% summary

# White correction for heteroskedasticity
# plm(lwage~educ+exper,model="pooling",data=wagepan) %>% summary(method="white1")
# plm(lwage~educ+exper,model="within",data=wagepan) %>% summary(method="white1")
# plm(lwage~educ+exper,model="within",effect="individual",data=wagepan) %>% summary(method="white1")
# plm(lwage~educ+exper,model="within",effect="time",data=wagepan) %>% summary(method="white1")
# plm(lwage~educ+exper,model="within",effect="twoways",data=wagepan) %>% summary(method="white1")

# HAC estimator. Note: n must be >> T
# heteroskedasticity and autocorrelation consistent estimator
plm(lwage~educ+exper,model="pooling",data=wagepan) %>% summary(method="arellano")
plm(lwage~educ+exper,model="within",data=wagepan) %>% summary(method="arellano")
plm(lwage~educ+exper,model="within",effect="individual",data=wagepan) %>% summary(method="arellano")
plm(lwage~educ+exper,model="within",effect="time",data=wagepan) %>% summary(method="arellano")
# plm(lwage~educ+exper,model="within",effect="twoways",data=wagepan) %>% summary(method="arellano")

plm(lwage~educ+exper,model="within",data=wagepan) %>% summary
model <- plm(lwage~educ+exper,model="within",data=wagepan)
summary(model,vcov=vcovHC(model,method="arellano"))

wagepandt[,var(educ),by=nr]


####################
## Logistic models with mroz
####################

# variable name   type    format     label      variable label
# inlf            byte    %9.0g                 =1 if in lab frce, 1975
# hours           int     %9.0g                 hours worked, 1975
# kidslt6         byte    %9.0g                 # kids < 6 years
# kidsge6         byte    %9.0g                 # kids 6-18
# age             byte    %9.0g                 woman's age in yrs
# educ            byte    %9.0g                 years of schooling
# wage            float   %9.0g                 est. wage from earn, hrs
# repwage         float   %9.0g                 rep. wage at interview in 1976
# hushrs          int     %9.0g                 hours worked by husband, 1975
# husage          byte    %9.0g                 husband's age
# huseduc         byte    %9.0g                 husband's years of schooling
# huswage         float   %9.0g                 husband's hourly wage, 1975
# faminc          float   %9.0g                 family income, 1975
# mtr             float   %9.0g                 fed. marg. tax rte facing woman
# motheduc        byte    %9.0g                 mother's years of schooling
# fatheduc        byte    %9.0g                 father's years of schooling
# unem            float   %9.0g                 unem. rate in county of resid.
# city            byte    %9.0g                 =1 if live in SMSA
# exper           byte    %9.0g                 actual labor mkt exper
summary(mroz)

# Linear probability model
model1 <- lm(inlf~educ+exper+I(exper^2)+age+kidslt6+kidsge6,data=mroz)
summary(model1)
coeftest(model1,vcov.=vcovHC)

newwomen <- data.table(V1=c(16,20,11),V2=c(5,15,3),
                       V3=c(30,40,30),V4=c(0,0,3),V5=c(0,2,0))
colnames(newwomen) <- c('educ','exper','age','kidslt6','kidsge6')
head(newwomen)
predict(model1,newwomen)
# First woman has an 81% chance of being in the labor force
# Second woman has a 108% chance of being in the labor force ??? 
# Third woman has a -22% chance of being in the labor force ???
model2 <- glm(inlf~educ+exper+I(exper^2)+age+kidslt6+kidsge6,family=binomial,data=mroz)
# model2 <- logit(inlf~educ+exper+I(exper^2)+age+kidslt6+kidsge6,data=mroz)
summary(model2)

coeftest(model1)
coeftest(model2)
coeftest(model1,vcov.=vcovHC)
coeftest(model2,vcov.=vcovHC)

AIC(model2)
BIC(model2)

predict(model2,newwomen) # Don't use this for glms
predict(model2,newwomen,type="response")
# First woman has an 85% chance of being in the labor force
# Second woman has a 96% chance of being in the labor force
# Third woman has a 2% chance of being in the labor force

?margins
margins(model2)
# Every year of education makes you 3.3% more likely to be in the labor force controlling for ...
# Every year of experience makes you 2.6% more likely to be in the labor force controlling for ...
# Every child less than 6 makes you 25.7% less likely to be in the labor force controlling for ...

# The following are for calculating margins at particular points (We don't normally do this)
margins(model2,data=newwomen[1])
margins(model2,data=newwomen[2])
margins(model2,data=newwomen[3])

ggplot(mroz,aes(x=exper,y=inlf)) + geom_point() +
  geom_line(aes(y=predict(lm(inlf~exper,data=mroz))),color="red",size=2)+
  geom_line(aes(y=predict(glm(inlf~exper,family=binomial(link="logit"),data=mroz),type="response")),color="blue",size=2)

####################
## Poisson models with crime1
####################

# variable name   type    format     label      variable label
# narr86          byte    %9.0g                 # times arrested, 1986
# nfarr86         byte    %9.0g                 # felony arrests, 1986
# nparr86         byte    %9.0g                 # property crme arr., 1986
# pcnv            float   %9.0g                 proportion of prior convictions
# avgsen          float   %9.0g                 avg sentence length, mos.
# tottime         float   %9.0g                 time in prison since 18 (mos.)
# ptime86         byte    %9.0g                 mos. in prison during 1986
# qemp86          float   %9.0g                 # quarters employed, 1986
# inc86           float   %9.0g                 legal income, 1986, $100s
# durat           float   %9.0g                 recent unemp duration
# black           byte    %9.0g                 =1 if black
# hispan          byte    %9.0g                 =1 if Hispanic
# born60          byte    %9.0g                 =1 if born in 1960
summary(crime1)

model3 <- lm(narr86~tottime+qemp86+inc86+black+hispan,data=crime1)
summary(model3)

model4 <- glm(narr86~inc86+black+hispan,family=poisson,data=crime1)
summary(model4)
BIC(model4)
AIC(model4)
# A $1000 increase in income in '86 => 8.5% decrease in the number of arrests on average 
margins(model4)
# A $1000 increase in income in '86 => 0.03 decrease in the number of arrests

coeftest(model3)
coeftest(model4)
coeftest(model3,vcov.=vcovHC)
coeftest(model4,vcov.=vcovHC)

