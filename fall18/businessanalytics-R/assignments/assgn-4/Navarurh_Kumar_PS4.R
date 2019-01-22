# Problem Set 4
# Name - Navarurh Kumar
# NetID - NXK180010
# Class - BA with R Friday 4-7PM


#home directory definition
setwd("/home/navarurh/documents/assignments/businessanalytics-R/assgn-4/")

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
library(plm)
library(margins)

#sql connection function to ease life
datafetch <- function(table_name){
  con <- dbConnect(SQLite(),'wooldridge.db')
  read_table <- data.table(dbReadTable(con,table_name))
  dbDisconnect(con)
  return(read_table)
}

#function for aic bic output
ic <- function(x){
  return(c(AIC(x),BIC(x)))
}


####start of assignment


########################
#     Question 4.1     #
########################

#Q4.1 the best fit model in this case is 
#     lm(data = hprice, price ~ bdrms + lotsize + sqrft + I(bdrms^2) + I(lotsize^2) + bdrms:lotsize + bdrms:sqrft))

     

hprice <- datafetch('hprice1')
hprice_lab <- datafetch('hprice1_labels')

model1 <- lm(data = hprice, log(price) ~ bdrms + lotsize + sqrft)
model1_ <- lm(data = hprice, price ~ bdrms + lotsize + sqrft)
summary(model1)
summary(model1_)

model2 <- lm(data = hprice, log(price) ~ bdrms + lotsize + I(sqrft^2))
model2_ <- lm(data = hprice, price ~ bdrms + lotsize + I(sqrft^2))
summary(model2)
summary(model2_)

model3 <- lm(data = hprice, price ~ bdrms + I(lotsize^2) + sqrft)
summary(model3)
model4 <- lm(data = hprice, price ~ I(bdrms^2) + lotsize + sqrft)
summary(model4)
model5 <- lm(data = hprice, price ~ bdrms + lotsize + sqrft + I(bdrms^2) + I(lotsize^2) + I(sqrft^2))
summary(model5)
model6 <- lm(data = hprice, price ~ (bdrms + lotsize + sqrft)^2 + I(bdrms^2) + I(lotsize^2) + I(sqrft^2))
summary(model6)
model7 <- lm(data = hprice, price ~ bdrms + lotsize + sqrft + I(bdrms^2) + I(lotsize^2) + I(sqrft^2) + I(bdrms^3))
summary(model7)

ic(model1_) # 975.7549 988.1416
ic(model2_) # 971.9324 984.3191
ic(model3) # 982.9836 995.3702
ic(model4) # 974.8586 987.2452
ic(model5) # 954.9752 974.7939
ic(model6) # 937.2350 964.4858 - lowest
ic(model7) # 956.7202 979.0162

best <- step(model6)
best

bestfitmodel <- lm(data = hprice, price ~ bdrms + lotsize + sqrft + I(bdrms^2) + I(lotsize^2) + bdrms:lotsize + bdrms:sqrft)
summary(bestfitmodel)

########################
#     Question 4.2     #
########################

#Q4.2 the best fit model in this case is
#     lm(data = gpa, colgpa ~ sat + tothrs + verbmath + hsperc + I(hsperc^2) + sat:tothrs + sat:hsperc + tothrs:hsperc + verbmath:hsperc)

gpa <- datafetch('gpa2')

model1 <- lm(data = gpa, colgpa ~ sat + tothrs + verbmath + hsperc)
summary(model1)
model2 <- lm(data = gpa, colgpa ~ I(sat^2) + tothrs + verbmath + hsperc)
summary(model2)
model3 <- lm(data = gpa, colgpa ~ sat + I(tothrs^2) + verbmath + hsperc)
summary(model3)
model4 <- lm(data = gpa, colgpa ~ sat + tothrs + I(verbmath^2) + hsperc)
summary(model4)
model5 <- lm(data = gpa, colgpa ~ sat + tothrs + verbmath + I(hsperc^2))
summary(model5)
model6 <- lm(data = gpa, colgpa ~ I(sat^2) + I(tothrs^2) + I(verbmath^2) + I(hsperc^2))
summary(model6)
model7 <- lm(data = gpa, colgpa ~ (sat + tothrs + verbmath + hsperc)^2 + I(sat^2) + I(tothrs^2) + I(verbmath^2) + I(hsperc^2))
summary(model7)
model8 <- lm(data = gpa, colgpa ~ I(sat^2) + I(tothrs^2) + I(verbmath^2) + I(hsperc^2) + I(sat^3))
summary(model8)

ic(model1) # 6913.909 6951.875
ic(model2) # 6895.831 6933.797
ic(model3) # 6919.732 6957.699
ic(model4) # 6913.915 6951.881
ic(model5) # 7134.979 7172.945
ic(model6) # 7117.609 7155.576
ic(model7) # 6729.287 6830.530 - lowest
ic(model8) # 7109.695 7153.989

best <- step(model7)
best

bestfitmodel <- lm(data = gpa, colgpa ~ sat + tothrs + verbmath + hsperc + I(hsperc^2) + sat:tothrs + sat:hsperc + tothrs:hsperc + verbmath:hsperc)
summary(bestfitmodel)

########################
#     Question 4.3     #
########################

#Q4.3 the best fit model in this case is
#     lm(data = mlb), log(salary) ~ years + gamesyr + bavg + hrunsyr + 
#                     rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + 
#                     thrdbase + shrtstop + catcher + I(years^2) + I(gamesyr^2) + 
#                     I(allstar^2) + years:gamesyr + years:hrunsyr + years:rbisyr + 
#                     years:runsyr + years:catcher + gamesyr:hrunsyr + gamesyr:rbisyr + 
#                     gamesyr:runsyr + gamesyr:frstbase + gamesyr:scndbase + gamesyr:shrtstop + 
#                     bavg:rbisyr + hrunsyr:runsyr + hrunsyr:fldperc + hrunsyr:thrdbase + 
#                     hrunsyr:shrtstop + rbisyr:runsyr + rbisyr:scndbase + rbisyr:shrtstop + 
#                     runsyr:allstar + runsyr:thrdbase + fldperc:allstar + fldperc:frstbase + 
#                     fldperc:shrtstop + fldperc:catcher + allstar:frstbase + allstar:scndbase)

mlb <- datafetch('mlb1')

model1 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
model2 <- lm(data = mlb, log(salary) ~ I(years^2) + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
model3 <- lm(data = mlb, log(salary) ~ years + I(gamesyr^2) + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
model4 <- lm(data = mlb, log(salary) ~ years + gamesyr + I(bavg^2) + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
model5 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + I(hrunsyr^2) + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
model6 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + I(rbisyr^2) + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
model7 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + I(runsyr^2) + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
model8 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + I(fldperc^2) + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)
model9 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + I(allstar^2) + frstbase + scndbase + thrdbase + shrtstop + catcher)
model10 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + I(frstbase^2) + scndbase + thrdbase + shrtstop + catcher)
model11 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + I(scndbase^2) + thrdbase + shrtstop + catcher)
model12 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + I(thrdbase^2) + shrtstop + catcher)
model13 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + I(shrtstop^2) + catcher)
model14 <- lm(data = mlb, log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + I(catcher^2))
model15 <- lm(data = mlb, log(salary) ~ I(years^2) + I(gamesyr^2) + I(bavg^2) + I(hrunsyr^2) + I(rbisyr^2) + I(runsyr^2) + I(fldperc^2) + I(allstar^2) + I(frstbase^2) + I(scndbase^2) + I(thrdbase^2) + I(shrtstop^2) + I(catcher^2))
model16 <- lm(data = mlb, log(salary) ~ I(years^2) + I(gamesyr^2) + I(bavg^2) + I(hrunsyr^2) + I(rbisyr^2) + I(runsyr^2) + I(fldperc^2) + I(allstar^2) + I(frstbase^2) + I(scndbase^2) + I(thrdbase^2) + I(shrtstop^2) + I(catcher^2) + I(years^3))
model17 <- lm(data = mlb, log(salary) ~ I(years^2) + I(gamesyr^2) + I(bavg^2) + I(hrunsyr^2) + I(rbisyr^2) + I(runsyr^2) + I(fldperc^2) + I(allstar^2) + I(frstbase^2) + I(scndbase^2) + I(thrdbase^2) + I(shrtstop^2) + I(catcher^2) + I(gamesyr^3))
model18 <- lm(data = mlb, log(salary) ~ (years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher)^2 + I(years^2) + I(gamesyr^2) + I(bavg^2) + I(hrunsyr^2) + I(rbisyr^2) + I(runsyr^2) + I(fldperc^2) + I(allstar^2) + I(frstbase^2) + I(scndbase^2) + I(thrdbase^2) + I(shrtstop^2) + I(catcher^2))

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
summary(model16)
summary(model17)
summary(model18)

ic(model1) # 774.9215 832.9185
ic(model2) # 795.6623 853.6593
ic(model3) # 783.4971 841.4941
ic(model4) # 775.0302 833.0272
ic(model5) # 776.4031 834.4001
ic(model6) # 772.3854 830.3824
ic(model7) # 781.6443 839.6413
ic(model8) # 774.9158 832.9129
ic(model9) # 778.4923 836.4894
ic(model10) # 774.9215 832.9185
ic(model11) # 774.9215 832.9185
ic(model12) # 774.9215 832.9185
ic(model13) # 774.9215 832.9185
ic(model14) # 774.9215 832.9185
ic(model15) # 844.2513 902.2483
ic(model16) # 782.3599 844.2234
ic(model17) # 794.0655 855.9290
ic(model18) #  703.484 1055.333 - lowest

best <- step(model18)
best

bestfitmodel <- lm(data = mlb,  log(salary) ~ years + gamesyr + bavg + hrunsyr +
                                rbisyr + runsyr + fldperc + allstar + frstbase + scndbase +
                                thrdbase + shrtstop + catcher + I(years^2) + I(gamesyr^2) +
                                I(allstar^2) + years:gamesyr + years:hrunsyr + years:rbisyr +
                                years:runsyr + years:catcher + gamesyr:hrunsyr + gamesyr:rbisyr +
                                gamesyr:runsyr + gamesyr:frstbase + gamesyr:scndbase + gamesyr:shrtstop +
                                bavg:rbisyr + hrunsyr:runsyr + hrunsyr:fldperc + hrunsyr:thrdbase +
                                hrunsyr:shrtstop + rbisyr:runsyr + rbisyr:scndbase + rbisyr:shrtstop +
                                runsyr:allstar + runsyr:thrdbase + fldperc:allstar + fldperc:frstbase +
                                fldperc:shrtstop + fldperc:catcher + allstar:frstbase + allstar:scndbase)

summary(bestfitmodel)

########################
#     Question 4.4     #
########################

#Q4.4.i.    log(rent) = -.5688 + 0.2622*y90 + 0.0407*log(pop) + 0.5714*log(avginc) + 0.005*pctstu
#           y90 coeff shows that for a change in the decade the rent has increased
#           by 26% keeping all the other factors constant
#           as for b-pctstu we see a very significant p-val of 2.4e-6 and we can interpret that
#           a 1%-point increase in pctstu will lead to a 0.5% increase in the rent
#Q4.4.ii.   The equation in part i does not have the ai term in the model. As a result if we include
#           ai in the error term then there is a very obvious correlation between the error terms across
#           the two time periods, invalidating the error terms from the first model
#Q4.4.iii.  clrent = 0.3855 + 0.0722*clpop + 0.3099*clavginc + 0.0112*cpctstu
#           In this model a 1%-point increase in pctstu will cause a 1.1% increase in the rent
#           cpctstu is not as significant as the last model but still fairly significant with a p-val of 0.008726
#Q4.4.iv.   log(rent) = 0.3855*y90 + 0.0722*log(pop) + 0.3099*log(avginc) + 0.0112*pctstu
#           We do get the same coefficients and standard erros when we run the previous model with fixed effects
#           the intercept in the previous becomes the estimate for the beta for y90 which is a flag variable

rent <- datafetch("rental")
rent_l <- datafetch("rental_labels")
rent <- pdata.frame(rent, index = c('city','year'))

model1 <- plm(data = rent, log(rent) ~ y90 + log(pop) + log(avginc) + pctstu, model = 'pooling')
summary(model1)

model2 <- plm(data = rent, clrent ~ y90 + clpop + clavginc + cpctstu, model='pooling')
summary(model2)

model3 <- plm(data = rent, log(rent) ~ y90 + log(pop) + log(avginc) + pctstu, model='within')
summary(model3)

########################
#     Question 4.5     #
########################

#Q4.5.i.    Past executions reduce murder rates so b1 should definitely be negative
#           Also good employment means lower crimes so we can say that b2 should be positive
#Q4.5.ii.   we get the following pooled ols
#           mrdrte = -5.278 - 2.067*d93 + 0.128*exec + 2.529*unem
#           there is no deterrent effect as the coeffs for both exec and unem are positive
#Q4.5.iii.  the first differenced model is
#           cmrdrte = 0.413 - 0.1038*cexec - 0.0666*cunem
#           here we see a deterrent effect coming in from both exec and cunem
#           so for an increase in 10 executions we will see a 1.04 point reduction in murder-rate, or 1 reduced murderer per 100k people
#           seems like a good decrease because a murder is quite rare anyways
#Q4.5.iv.   The heteroskedasticity robust error for delta(exec) is 0.0165, a non-robust error with a significant t-val of -6.296
#Q4.5.v.    Texas had 34 executions in '93 while the next highest was Virginia with 11
#Q4.5.vi.   The estimated deterrent effect is smaller here because Texas was introducing a lot of variability in d(exec)
#           The standard error on d(exec) has increased a lot by removing Texas
#Q4.5.vii.  we get the following model for fixed effects including all the data
#           mrdrte = 1.556*d90 + 1.733*d93 - 0.1383*exec + 0.2213*unem
#           exec still shows a deterrent effect which is higher in magnitude
#           so for an increase in 10 executions we will see a 1.38 point reduction in murder-rate, or 1.38 reduced murderer per 100k people
#           but the significance for exec has fallen by a lot and is now at the 0.04 level

murder <- datafetch("murder")
murder_l <- datafetch("murder_labels")
murder <- pdata.frame(murder,index = c("state","year"))

mur9093 <- murder[which(murder$year %in% c(90,93)),]
model1 <- plm(data = mur9093, mrdrte ~ d93 + exec + unem, model='pooling')
summary(model1)

mur93 <- murder[which(murder$d93 == 1),]
model2 <- plm(data = mur93, cmrdrte ~ cexec + cunem, model='pooling')
summary(model2)

summary(model2,vcov=vcovHC(model2,method='arellano'))

state_counts <- aggregate(mur93$exec, by = list(mur93$state), FUN = 'sum')

murntx <- murder[which(murder$state != 'TX' & murder$year == 93),]
model3 <- plm(data= murntx, cmrdrte ~ cexec + cunem, model='pooling')
summary(model3)
summary(model3,vcov=vcovHC(model3,method='arellano'))

model4 <- plm(data = murder, mrdrte ~ d90 + d93 + exec + unem, model='within')
summary(model4)
summary(model4,vcov=vcovHC(model4,method='arellano'))

########################
#     Question 4.6     #
########################

#Q4.6.i.    based on the model a 0.1 change in concen will increase the fare by 3.6%
#Q4.6.ii.           2.5 %       97.5 %
#           concen  0.3011861   0.4190546
#           robust 95% interval will be at 
#           concen  0.2423      0.4778      
#Q4.6.iii.  log(fare) = 0.36*concen - 0.902*log(dist) + 0.103*log(dist)^2
#           say log(dist) = x and we keep concen constant
#           log(fare) = 0.103x^2 - 0.902x
#           d/dx-ing both sides
#           we see the inflection point at x = 0.902/(2*0.103)
#           or log(dist) = 4.3786 or dist = 79.729 but the min dist we have is 95 so the inflection point lies outside the range of our data
#Q4.6.iv.   we see the FE beta estimate of concen as 0.1688
#           log(fare) = 0.0228*y98 + 0.0364*y99 + 0.0978*y00 + 0.1688*concen
#Q4.6.v.    we are missing average passengers per day and average one way fare from the data given to us in the model created
#           these arent correlated to concen
#           but there could be many other factors that could be correlated to concen which are not present in the data and could comprise ai
#           some of them could be population, income, education, etc. on the routes that a airline company operates
#           city operators will always have higher spend and market share than rural operators
#Q4.6.vi.   yeah higher concentration does increase airfare and i would prefer the Fixed Effects model because it allows for correl between
#           concen and other time related factors

airf <- datafetch('airfare')
airf_l <- datafetch('airfare_labels')
airf <- pdata.frame(airf,index = c('id','year'))

model1 <- plm(data = airf, log(fare) ~ y98 + y99 + y00 + concen + log(dist) + I(log(dist)^2), model = 'pooling')
summary(model1)
confint(model1,"concen",level=0.95)
0.3011-1.96*0.03
0.4190+1.96*0.03

model2 <- plm(data = airf, lfare ~ y98 + y99 + y00 + concen + ldist + ldistsq, model = "within")
summary(model2)

cor.test(airf$concen,airf$passen)
cor.test(airf$concen,airf$fare)

########################
#     Question 4.7     #
########################

#Q4.7.i.  The marginal effect is 0.1443 which is similar to the linear model coeff for white which is 0.2006
#         so as per the linear model 20.06% higher chance whereas the logit says 14.43% higher chance of a loan approval if white
#Q4.7.ii. The average marginal effect for white is 0.08282
#         this shows that there is still a postive bias for white when it comes to loan approval at 8.28%
#         its fairly reduced but still quite high a value and very statistically significant

loan <- datafetch('loanapp')
loan_l <- datafetch('loanapp_labels')

model1 <- glm(data = loan, approve ~ white, family = binomial)
summary(model1)
margins(model1)

model2 <- lm(data = loan, approve ~ white)
summary(model2)

model3 <- glm(data = loan, approve ~  white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, family = binomial)
summary(model3)
margins(model3)

########################
#     Question 4.8     #
########################

#Q4.8.i.    89.82% of the sample is employed and 9.92% are alcohol abusers
#Q4.8.ii.   employ = 0.901 - .028*abuse
#           a person abusing alcohol has a 2.8% lesser chance of being employed, the relationship is significant at the 0.01 level
#           the relationship makes sense as alcoholics are expected to not have employment
#           heteroskedasticity-robust standard errors are
#                         Estimate      Std. Error    t value     Pr(>|t|)    
#           (Intercept)   0.9009946     0.0031755     283.7299    < 2e-16 ***
#           abuse         -0.0283046    0.0111529     -2.5379     0.01117 *
#Q4.8.iii.  the logit model shows that an alcohol abuser has a 2.6% lesser chance of being employed
#           the result is significant at the 0.001 level
#Q4.8.iv.   both probit and logit predict employ = 0.9 at abuse = 0 and employ = 0.8727 at abuse = 1
#           both logit and probit use cdfs to convert the dep var into 0-1 range
#           in this case since we have only one indep var we see the exact output for logit and probit
#Q4.8.v.    the coefficient for abuse is 0.0203 at the 0.01 level
#           so a person abusing alcohol has a 2% lesser chance of being employed
#Q4.8.vi.   the average marginal effect of abuse is now that if a person abuses alcohol they have a 1.94% lower chance of being employed
#           it is statistically significant at the 0.01 level and is very close in value and significance to the linear model
#Q4.8.vii.  including them as controls is not really obvious as health metrics directly tie in with being an alcohol abuser, heavy correlation
#Q4.8.viii. Since mothalc and fathalc are not correlated with abuse we can use those as instrumental variables for abuse
#           a linear model for employ with all 3 doesnt change the significance level of abuse and barely changes its beta-estimate

alc <- datafetch('alcohol')
alc_l <- datafetch('alcohol_labels')

mean(alc$employ)
mean(alc$abuse)

model1 <- lm(data = alc, employ ~ abuse)
summary(model1)
coeftest(model1,vcovHC)

model2 <- glm(data = alc, employ ~ abuse, family = binomial)
summary(model2)
margins(model2)
model3 <- glm(data = alc, employ ~ abuse, family = binomial(link = "probit"))
summary(model3)
margins(model3)

predict(model2,data.frame(abuse = c(0,1)),type = "response")
predict(model3,data.frame(abuse = c(0,1)),type = "response")

model4 <- lm(data = alc, employ ~ abuse + age + agesq + educ + educsq + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3)
summary(model4)

model5 <- glm(data = alc, employ ~ abuse + age + agesq + educ + educsq + married + famsize + white + northeast + midwest + south + centcity + outercity + qrt1 + qrt2 + qrt3, family = binomial)
summary(model5)
margins(model5)

cor.test(alc$abuse,alc$fathalc)$estimate
cor.test(alc$abuse,alc$mothalc)$estimate
model6 <- lm(data = alc, employ ~ abuse + mothalc + fathalc)
summary(model6)

########################
#     Question 4.9     #
########################

#Q4.9.i.    The coefficient on y82 is -.1926 with an average marginal effect of -0.5283
#           This means we see a 19.26% decrease in number of kids from 1972 to 1982
#Q4.9.ii.   Keeping all else constant we see that a black woman will likely have 43.3% more children than a non black woman
#Q4.9.iii.  We get a correlation of 0.3476971 between kids and kids-predicted by poisson
#           The r-square for the linear model is 0.1295
#           The r-square based on sqrt(correlation-value) for poisson model is 0.12089
#           Thus the r-square is higher for the linear model

fert <- datafetch('fertil1')
fert_l <- datafetch('fertil1_labels')

model1 <- glm(data = fert, kids ~ educ + age + agesq + black + east + northcen + west + farm + othrural + town + smcity
                                  + y74 + y76 + y78 + y80 + y82 + y84, family=poisson)
summary(model1)
margins(model1)

(exp(0.3603475) - 1)*100

model2 <- lm(data = fert, kids ~ educ + age + agesq + black + east + northcen + west + farm + othrural + town + smcity
              + y74 + y76 + y78 + y80 + y82 + y84)
summary(model2)

fert$pred_poisson <- predict(model1,type="response")
fert$pred_linear <- predict(model2)

corval <- cor.test(fert$kids,fert$pred_poisson)$estimate
corval
corval^2