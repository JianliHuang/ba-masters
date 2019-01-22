# Problem Set 3
# Name - Navarurh Kumar
# NetID - NXK180010
# Class - BA with R Friday 4-7PM


#home directory definition
setwd("/home/navarurh/documents/assignments/businessanalytics-R/assgn-3/")

#packages used for the assignment
library(data.table)
library(DBI)
library(RSQLite)
library(sandwich)
library(lmtest)
library(plyr)
library(tseries)
library(broom)
library(reshape)
library(vars)

#sql connection function to ease life
datafetch <- function(table_name){
  con <- dbConnect(SQLite(),'wooldridge.db')
  read_table <- data.table(dbReadTable(con,table_name))
  dbDisconnect(con)
  return(read_table)
}

####start of assignment


########################
#     Question 3.1     #
########################

#Q3.1.i.    The null hypothesis is H0: b13 = 0 (since we dont have an outfielder variable setting catcher to zero sets the required null)
#           We get an estimated b13 value of 0.2535 with se(b13) = 0.131 and a t-val of 1.93 and p-val of 0.054 (reject at 5.5% if that has a meaning) 
#           hence we have cannot reject the null at 5% (imp - but just barely)
#           b13 being binary the change from 0(outfielder) to 1(catcher) gives a %diff of (e^b13-1)*100 or 28.86%
#           so there is a huge difference in salaries of catchers and outfielders
#           which is contrary to our rejection of the null at alpha = 0.05
#           this is mostly a consequence of p-val(0.054) being really really close to alpha(0.05), we could have rejected the null as it turns out
#Q3.1.ii.   we have to test the null that H0:b9=0b10=b11=b12=b13=0
#           anova gives an f-stat of 1.777 and a p-val of 0.1168 (can only reject at 12%)
#           so we cannot reject the null at a 10% level
#Q3.1.iii.  the results seem consistent because at a summary glance we see that thrdbase and shrtstop variables are low significance based on t-stat
#           and catcher is a highly significant variable compared to these two

mlb <- datafetch('mlb1')
mlb_lab <- datafetch('mlb1_labels')

model1 <- lm (data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
summary(model1)
(exp(model1$coefficients[14])-1)*100
model2 <- lm (data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar)
summary(model2)
anova(model1,model2)


########################
#     Question 3.2     #
########################

#Q3.2.i.    expecting a postive relation with sat
#           and a negative relation with hsperc[smaller the better per definition], hsize[feels like small classes would do better]
#           female and athlete could go either way, no assumptions on gender or whether athletes are good at studies
#Q3.2.ii.   delta colgpa(athlete - non-athlete) = 0.169
#           super significant with a t-stat of 3.998 and a rejection at 5% with p-val 6.5e-5
#Q3.2.iii.  delta colgpa(athlete - non-athlete) = 0.005 and a highly insignificant var in the equation
#           this shows controlling for SAT scores athletes perform better whereas not controlling for SATs we see no diff between athletes and non-athletes
#Q3.2.iv.   creating variables for female-athlete which is 0-1 for females and athletes non-athletes (doing the same for males as well for controlling)
#           our null hypothesis is H0:beta(fathl) = 0
#           we see fathl has a beta of 0.2511 with a tstat at 2.981 and p-val of 0.0029
#           this is a very significant var in the regression, we can reject the null at the 5% alpha
#Q3.2.v.    we see that when we add the sat*female interaction we get a t-val of 0.4 and a p-val of 0.69
#           these are highly insignificant and hence we can say that gender has no effect on SATs

gpa <- datafetch('gpa2')
gpa_lab <- datafetch('gpa2_labels')

model3 <- lm(data = gpa, colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + athlete)
summary(model3)

model4 <- lm(data = gpa, colgpa ~ hsize + I(hsize^2) + hsperc + female + athlete)
summary(model4)
anova(model3,model4)

gpa2 <- gpa
gpa2$fathl <- gpa2$female * gpa2$athlete
gpa2$mathl <- 0
gpa2$mathl[which(gpa2$female == 0 & gpa2$athlete == 1)] <- 1
model5 <- lm(data = gpa2, colgpa ~ hsize + I(hsize^2) + hsperc + sat + fathl + mathl)
summary(model5)

model6 <- lm(data = gpa, colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + athlete + I(sat*female))
summary(model6)


########################
#     Question 3.3     #
########################

#Q3.3.i.    if there is discrimination then b1 for (white) should have a high positive value
#Q3.3.ii.   we get a coeff of 0.2 with t-val at 10.1 and 2e-16 for p-val
#           approve = 0.7 + 0.2*white
#           this is highly significant and says that there is a flat 70% chance of approval with a 20% increased chance if white
#Q3.3.iii.  we have beta for white as 0.12 and an intercept of 0.937
#           t-val for white is 6.53 and a p-val of 8.4e-11, all highly significant
#           there is still a big bias towards white in this model
#Q3.3.iv.   the interaction term white*obrat has really high significance with t-val 3.53 and p-val of 0.0004
#Q3.3.v.    adjusting the obrat var to make it obrat-32 we get the following result for white
#           beta = 0.113 se = 0.02 so we have a alpha 5% level in the following range
#           0.113 +- 1.96(0.02)
#           or a 95% confidence interval of 0.0738 and 0.1522
#           doesnt include zero so there is always an advantage to being white when applying for a loan as per this data and model

loan <- datafetch('loanapp')
loan_lab <- datafetch('loanapp_labels')

model7 <- lm(data = loan, approve ~ white)
summary(model7)

model8 <- lm(data = loan, approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr)
summary(model8)

model9 <- lm(data = loan, approve ~ I(white*obrat) + white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr)
summary(model9)

loan2 <- loan
loan2$obrat <- loan2$obrat - 32
model10 <- lm(data = loan2, approve ~ I(white*obrat) + white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr)
summary(model10)


########################
#     Question 3.4     #
########################

#Q3.4.i.              se(normal lm)     se(robust)
#           lotsize   0.00064           0.00125
#           sqrft     0.0132            0.0177
#           bdrms     9.01              8.478
#           lotsize loses all significance when adjusting for heteroskedasticity
#           sqrft also loses significance but not by much (still very significant)
#           bdrms gains significance but too low to be of any consequence (p is still > 0.1)
#Q3.4.ii.                 se(normal lm)			se(robust)
#           log(lotsize)  0.03828				    0.041473
#           log(sqrft)    0.09287				    0.103829
#           bdrms         0.02753				    0.030601
#           looking at the model and the se and se(robust) are quite close with the same significance attached to variables in both
#           both show log(lotsize) and log(sqrft) as significant and bdrms as insignificant
#Q3.4.iii.  this shows that taking log transform of the dependent helps remove(almost if not altogether) heteroskedasticity in the data

hprice <- datafetch('hprice1')
hprice_lab <- datafetch('hprice1_labels')

model11 <- lm(data = hprice, price ~ lotsize + sqrft + bdrms)
summary(model11)
coeftest(model11, vcov = vcovHC(model11, type="HC1"))


model12 <- lm(data = hprice, log(price) ~ log(lotsize) + log(sqrft) + bdrms)
summary(model12)
coeftest(model12, vcov = vcovHC(model12, type="HC1"))

########################
#     Question 3.5     #
########################

#Q3.5.i.    the residuals are in the variable resid
#           the simple ols reg model is as follows
#           colGPA = 1.356 + 0.413*hsGPA + 0.0133*ACT - 0.071*skipped + 0.124*PC
#Q3.5.ii.   running the white test on residuals from prev and the pred values (preds variable) we get
#           r-sq = 0.036 fstat = 3.58 and p-val = 0.03, significant
#           so at a 5% level there is heteroskedasticity in the model we made as per White's test
#Q3.5.iii.  min(predict.lm(model14) > 0) shows 1 hence all values predicted are greater than zero
#           the weighted ols is as follows
#           colGPA = 1.402 + 0.403*hsGPA + 0.013*ACT - 0.076*skipped + 0.126*PC
#Q3.5.iv.                 se			    se(robust)
#           hsGPA         0.083362		0.086334
#           ACT           0.009827		0.010414
#           skipped       0.022173		0.021209
#           PC            0.056339		0.059025
#           there isnt a big change in standard errors from the previous part
#           this goes to show that WLS regression does a good job at taking care of heteroskedasticity in the data
#           both have the same significant variables

gpa1 <- datafetch('gpa1')
gpa1_lab <- datafetch('gpa1_labels')

model13 <- lm(data = gpa1, colGPA ~ hsGPA + ACT + skipped + PC)
summary(model13)

resid <- model13$residuals
preds <- predict.lm(model13)
model14 <- lm(resid^2 ~ preds + I(preds^2))
summary(model14)

min(predict.lm(model14) > 0)
model15 <- lm(data = gpa1, colGPA ~ hsGPA + ACT + skipped + PC, weights = 1/predict.lm(model14))
summary(model15)
coeftest(model15, vcov = vcovHC(model15, type="HC1"))



########################
#     Question 3.6     #
########################

#Q3.6.1     NA
#Q3.6.2     NA
#Q3.6.3     NA
#Q3.6.4     line 248-252
#Q3.6.5     a normal ols gives the following relation
#           btc_price = -41590 + 10.95*sp500 - 2.993*gold -56.18*texas_oil + 22760*usd_eu_conv
#           gives negative predictions alongwith horrible predictions for recent btc prices
#           plot(model16$residuals) is all over the place and pretty much follows the btc price trend in terms of shape
#Q3.6.6     each of the 5 series are level-stationary after taking 1st differences
#Q3.6.7     the ols with diffs at level1 for all variables gives the following
#           diff(btc_price) = 4.024 + 0.932*diff(sp500) + 0.588*diff(gold) - 0.37*diff(texas_oil) - 502.5*diff(usd_eu_conv)
#Q3.6.8     line 301-311
#Q3.6.9     line 314-315
#Q3.6.10    running the arima model for different AR and MA values gives a min at AR = 7 and MA = 5 with an AIC of -1198.845
#Q3.6.11    line 338-343
#Q3.6.12    the periodogram shows too many spikes, data might have seasonality but it is not visible to the naked eye
#Q3.6.13    the periodogram has not really changed from the original, no new insights into seasonality
#Q3.6.14    the model with p=1 has the lowest AIC hence we choose that for our VAR model
#           we see the following granger causalities for different dependents
#           dep = btc_price   - no causalities
#           dep = sp500       - gold price at 1st diff level has some significance
#           dep = gold        - texas_oil and usd_eu_conv have an effect on the gold prices 
#                               [understandable as currency strength is interralted with country's gold reserves]
#           dep = texas_oil   - no causalities
#           dep = usd_eu_conv - no causalities
#Q3.6.15    The predictions for bitcoin prices from the two models look fairly similar
#           The var model shows a super steady almost horizontal trend - line365-366
#           The arima model also shows a similar trend for the next 30 days as per line 326

btcf <- read.csv('navarurh_kumar_btc_data.csv',header = T,stringsAsFactors = F)

#3.6.4
ts.plot(btcf$btc_price)
ts.plot(btcf$sp500)
ts.plot(btcf$gold)
ts.plot(btcf$texas_oil)
ts.plot(btcf$usd_eu_conv)

#3.6.5
model16 <- lm(data = btcf, btc_price ~ sp500 + gold + texas_oil + usd_eu_conv)
summary(model16)
predict.lm(model16)[1]
predict.lm(model16)[1146]
plot(model16$residuals)

#3.6.6
kpss.test(btcf$btc_price,null="Level")
kpss.test(btcf$btc_price,null="Trend")
kpss.test(diff(btcf$btc_price),null="Level")

kpss.test(btcf$sp500,null="Level")
kpss.test(btcf$sp500,null="Trend")
kpss.test(diff(btcf$sp500),null="Level")

kpss.test(btcf$gold,null="Level")
kpss.test(btcf$gold,null="Trend")
kpss.test(diff(btcf$gold),null="Level")

kpss.test(btcf$texas_oil,null="Level")
kpss.test(btcf$texas_oil,null="Trend")
kpss.test(diff(btcf$texas_oil),null="Level")

kpss.test(btcf$usd_eu_conv,null="Level")
kpss.test(btcf$usd_eu_conv,null="Trend")
kpss.test(diff(btcf$usd_eu_conv),null="Level")

ts.plot(diff(btcf$btc_price))
ts.plot(diff(btcf$sp500))
ts.plot(diff(btcf$gold))
ts.plot(diff(btcf$texas_oil))
ts.plot(diff(btcf$usd_eu_conv))

#3.6.7
model17 <- lm(data = btcf, diff(btc_price) ~ diff(sp500) + diff(gold) + diff(texas_oil) + diff(usd_eu_conv)) 
summary(model17)

#3.6.8
btcf <- btcf[which(btcf$date >= '2017-01-01'),]
ts.plot(btcf$btc_price)
ts.plot(btcf$sp500)
ts.plot(btcf$gold)
ts.plot(btcf$texas_oil)
ts.plot(btcf$usd_eu_conv)
ts.plot(diff(btcf$btc_price))
ts.plot(diff(btcf$sp500))
ts.plot(diff(btcf$gold))
ts.plot(diff(btcf$texas_oil))
ts.plot(diff(btcf$usd_eu_conv))

#3.6.9
acf(diff(btcf$btc_price))
pacf(diff(btcf$btc_price))

acf(diff(log(btcf$btc_price)))
pacf(diff(log(btcf$btc_price)))

#3.6.10
arima_pq <- 8
outp <- matrix(0L,(arima_pq+1)^2,3)
pos <- 1
for(i in 0:arima_pq){
  for(j in 0:arima_pq) {
    tryCatch({aic <- arima(log(btcf$btc_price),c(i,1,j))$aic},error=function(err){aic<-9999.99})
    outp[pos,1:3] <- c(i,j,aic)
    pos <- pos + 1
  }
}
outp <- data.frame(outp)
colnames(outp) <- c('p','q','AIC')

#3.6.11
library(forecast)
model18 <- arima(log(btcf$btc_price),c(7,1,5))
steps <- 30
future <- forecast(model18,h=steps)
plot(future)
plot(future,xlim=c(417-steps,417+steps),ylim=c(7.5,10.5))

#3.6.12
library(TSA)
periodogram(log(btcf$btc_price))
periodogram(diff(log(btcf$btc_price)))

#3.6.13
btcf$date <- as.Date(btcf$date)
btcf$dow <- weekdays(btcf$date)
btcfw <- btcf[c(2,7)]
btcfw$dummy <- 1
btcfw <- reshape(btcfw, idvar = "btc_price", timevar = "dow", direction = "wide")
btcfw[is.na(btcfw)] <- 0
colnames(btcfw)[2:6] <- c('tu','we','th','fr','mo')
model19 <- lm(data = btcfw, diff(log(btc_price)) ~ tu[2:416] + we[2:416] + th[2:416] + fr[2:416] + mo[2:416])
summary(model19)
periodogram(model19$residuals)

#3.6.14
diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
btc_var <- btcf %>% dplyr::select(btc_price, sp500, gold, texas_oil, usd_eu_conv) %>% log %>% diff_jp
VAR(btc_var,p=1,type="both") %>% AIC
VAR(btc_var,p=2,type="both") %>% AIC
VAR(btc_var,p=3,type="both") %>% AIC
VAR(btc_var,p=64,type="both") %>% AIC
model20 <- VAR(btc_var,p=1,type="both")
summary(model20)
causality(model20,cause = 'sp500')$Granger
causality(model20,cause = 'gold')$Granger
causality(model20,cause = 'texas_oil')$Granger
causality(model20,cause = 'usd_eu_conv')$Granger

#3.6.15

prd <- predict(model20, n.ahead = 30, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")
plot(prd, "single",xlim=c(417-30,417+30),ylim=c(-0.015,0.015))