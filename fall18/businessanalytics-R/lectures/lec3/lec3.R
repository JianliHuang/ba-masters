######

###############################################################
## Correct WAGE1 data for heteroskedasticity   <-- Messes up significance testing
###############################################################

rm(list=ls())
setwd("/home/navarurh/documents/subjects/businessanalytics-R/lec3/")
## Import packages
library(data.table)
library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools
library(tseries) # Time series package
library(DBI)
library(RSQLite)
library(tidyverse)
library(broom)

## Data import and validation
con <- SQLite() %>% dbConnect('wooldridge.db')
wage1 <- con %>% dbReadTable('wage1') %>% data.table
minwage232 <- con %>% dbReadTable('minwage') %>% data.table
minwage232 <- minwage232 %>% select(index,emp232,wage232,unem,cpi,minwage)
con %>% dbDisconnect
# variable name   type    format     label      variable label
# wage            float   %8.2g                 average hourly earnings
# educ            byte    %8.0g                 years of education
# exper           byte    %8.0g                 years potential experience
# tenure          byte    %8.0g                 years with current employer

# Model %change in wage using a quadratic model of education
model2 <- lm(log(wage)~educ+I(educ^2)+tenure,data=wage1)

# summary(model2) # these t-tests are wrong! [[OLS]]
coef(model2)
coeftest(model2) # [[OLS]] Old school t test for significance (like summary)
coeftest(model2,vcov.=vcovHC) #[[White]] White-corrected t test for significance

ols_se <- tidy(coeftest(model2))[,3]
white_se <- tidy(coeftest(model2,vcov.=vcovHC))[,3]
diff <- ols_se - white_se
vol_hetero <- sum(diff^2)/sum(ols_se^2)
vol_hetero

model1 <- lm(log(wage)~tenure,data=wage1)
model2 <- lm(log(wage)~educ+I(educ^2)+tenure,data=wage1)

anova(model1,model2) # ChiSq-test of the joint hypothesis that educ has no effect on 
# NOTE: THE F TEST CANNOT CORRECT FOR HETEROSKEDASTICITY, INSTEAD WE USE WLS

# Weighted Least Squares
wage1 %>% ggplot(aes(x=educ,y=residuals(model2)^2)) + geom_point()
wage1 <- wage1[educ!=0]
wage1$wei <- wage1$educ^(-1/2)
5^(-1/2)
16^(-1/2)
model3 <- lm(log(wage)~tenure,weights=wei,data=wage1)
model4 <- lm(log(wage)~educ+I(educ^2)+tenure,weights=wei,data=wage1)
coeftest(model4)
vcov(model4)
vcovHC(model4) # white corrected variance covariance matrix of betas
coeftest(model4,vcov.=vcovHC)# Heteroskedasticity has been reduced

ols_se <- tidy(coeftest(model4))[,3]
white_se <- tidy(coeftest(model4,vcov.=vcovHC))[,3]
diff <- ols_se - white_se
vol_hetero <- sum(diff^2)/sum(ols_se^2)
vol_hetero

anova(model3,model4) # Now we can ChiSq-test

###############################################################
## Explore minwage232 time series data 
###############################################################

# variable name   type    format     label      variable label
# emp232          float   %9.0g                 employment, sector 232, 1000s
# wage232         float   %9.0g                 hourly wage, sector 232, $
# unem            float   %9.0g                 civilian unemployment rate, %
# cpi             float   %9.0g                 Consumer Price Index (urban), 1982-1984 = 100
# minwage         float   %8.0g                 Federal minimum wage, $/hour
summary(minwage232)
head(minwage232)
minwage232$emp <- minwage232$emp232
minwage232$wage <- minwage232$wage232

ts.plot(minwage232$cpi) # Data is monthly from 1949 to 2000
ts.plot(minwage232$emp)
ts.plot(minwage232$wage)

model1 <- lm(log(emp)~log(wage)+index,data=minwage232)
summary(model1)

kpss.test(log(minwage232$emp),null="Level")
kpss.test(log(minwage232$emp),null="Trend")
kpss.test(diff(log(minwage232$emp)),null="Level")
kpss.test(diff(log(minwage232$emp)),null="Trend")
# After first-differencing, log employment is trend stationary

# employment series is TREND stationary after first-differencing
?kpss.test
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
rep.kpss(log(minwage232$emp))
rep.kpss(log(minwage232$wage))


ts.plot(diff(log(minwage232$emp)))
ts.plot(diff(log(minwage232$wage)))
# summary(lm(diff(log(wage))~index[2:612],data=minwage232))
# summary(lm(diff(log(emp))~index[2:612],data=minwage232))
# 
# kpss.test(diff(log(minwage232$emp)),null="Level")
# kpss.test(diff(log(minwage232$emp)),null="Trend") #good
# kpss.test(diff(log(minwage232$wage)),null="Level") #good
# # kpss.test(diff(log(minwage232$wage)),null="Trend")
# #employment is trend stationary after first-differencing
# #wages are level stationary after first-differencing
# # Disprove cointegration hypothesis
# kpss.test(residuals(model1),null="Level")
# kpss.test(residuals(model1),null="Trend")

## Without time # wrong b/c emp is trend stationary
# model2 <- lm(diff(log(emp))~diff(log(wage)),data=minwage232)
# summary(model2)
# coeftest(model2,vcov=NeweyWest(model2,lag=10))

## With time
model3 <- lm(diff(log(emp))~diff(log(wage))+index[2:612],data=minwage232)
summary(model3) # [[OLS]] wrong t-stats (significant tests)
coeftest(model3)
coeftest(model3,vcov=NeweyWest(model3,lag=10))
# Wages are not related to employment in this industry.

## With time
model4 <- lm(diff(log(emp))~index[2:612],data=minwage232)
coef(model4)
coeftest(model4,vcov=NeweyWest(model4,lag=10))

##############################################
##### The end of linear modeling and time series
##############################################

acf(diff(log(minwage232$wage)))
pacf(diff(log(minwage232$wage)))

## Arima for wage
maxa <- 13
outp <- matrix(0L,(maxa+1)^2,3)
lwage <- minwage232 %>% select(wage) %>% log
ndx <- 1
for(i in 0:maxa){
  print(paste(as.character(round(i*(maxa+1)/(maxa+1)^2*100,digits=2)),'%...',sep=''))
  for(j in 0:maxa) {
    tryCatch({aic<-lwage%>%arima(c(i,1,j))%>%AIC},error=function(err){aic<-9999.99})
    outp[ndx,1:3] <- c(i,j,aic)
    ndx <- ndx + 1
  }
} #tells us p = 12, q = 12 is optimal
rm(lwage,ndx) 
outp <- data.table(outp)
colnames(outp) <- c('p','q','AIC')
outp[order(AIC)]


model <- arima(log(minwage232$wage),c(13,1,12))


library(forecast)
model5 <- arima(log(minwage232$wage),c(13,1,12))
steps <- 60
future <- forecast(model5,h=steps)
plot(future)
plot(future,xlim=c(612-steps,612+steps),ylim=c(1.5,2.5))

library(TSA)
periodogram(log(minwage232$wage))
periodogram(diff(log(minwage232$wage)))
1/0.08
1/0.16
1/0.25
1/0.33

## sArima for wage
maxa <- 5
outp <- matrix(0L,(maxa+1)^2,3)
lwage <- minwage232 %>% select(wage) %>% log
ndx <- 1
for(i in 0:maxa){
  print(paste(as.character(round(i*(maxa+1)/(maxa+1)^2*100,digits=2)),'%...',sep=''))
  for(j in 0:maxa) {
    tryCatch({aic<-lwage%>%arima(c(i,1,j),seasonal=list(order=c(1,0,1),period=12))%>%AIC},error=function(err){aic<-9999.99})
    outp[ndx,1:3] <- c(i,j,aic)
    ndx <- ndx + 1
  }
} #tells us p = 0, q = 0 is optimal
rm(lwage,ndx) 
outp <- data.table(outp)
colnames(outp) <- c('p','q','AIC')
outp[order(AIC)]

model6 <- stats::arima(log(minwage232$wage),c(0,1,2),seasonal=list(order=c(1,0,1),period=12))
model6
steps <- 60
future <- forecast(model6,h=steps)
plot(future)
plot(future,xlim=c(612-steps,612+steps),ylim=c(1.5,2.5))

library(vars)
diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
x <- minwage232 %>% dplyr::select(wage,emp) %>% log %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
model7 <- VAR(x,p=2,type="both")
summary(model7)
# Employment Granger-causes change in the wage