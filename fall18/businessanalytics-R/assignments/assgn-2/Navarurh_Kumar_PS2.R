# Problem Set 2
# Name - Navarurh Kumar
# NetID - NXK180010
# Class - BA with R Friday 4-7PM


#home directory definition
setwd("/home/navarurh/documents/assignments/businessanalytics-R/assgn-2/")

#packages used for the assignment
library(data.table)
library(DBI)
library(RSQLite)

#sql connection function to ease life
datafetch <- function(table_name){
  con <- dbConnect(SQLite(),'wooldridge.db')
  read_table <- data.table(dbReadTable(con,table_name))
  dbDisconnect(con)
  return(read_table)
}

####start of assignment

########################
#     Question 2.1     #
########################

#Q2.1.i.    a 1% increase in expendA will lead to a β1/100 percent point change in voteA
#Q2.1.ii.   H0: β1 + β2 = 0
#Q2.1.iii.  voteA = 45.08788 + 6.08136*log(expendA) - 6.61563*log(expendB) + 0.15201*prtystrA
#           log(expendA) is a very significant factor in the estimation of voteA
#           log(expendB) is again very significant hence B's expenditures also have an impact on voteA
#           we cannot test the hypothesis as we do not have the standard error for the value (β1 + β2)
#Q2.1.iv.   writing θ = β1 + β2 or β1 = θ - β2 and introducing it into the original equation gives us
#           voteA = β0 + θ*log(expendA) + β2*[log(expendB) – log(expendA)] + β3*prtystrA
#           the linear model gives us a tstatistic value of -1.002 hence we can reject the null hypothesis

vote <- datafetch("vote1")
model1 <- lm(data = vote, voteA ~ log(expendA) + log(expendB) + prtystrA)
summary(model1)
model2 <- lm(data = vote, voteA ~ log(expendA) + I(log(expendA) - log(expendB)) + prtystrA)
summary(model2)

########################
#     Question 2.2     #
########################

#Q2.2.i.   the equation in ch3prb4 is as follows
#           log(salary) = β0 + β1*LSAT + β2*GPA + β3*log(libvol) + β4*log(cost) + β5*rank
#           null hypothesis that rank has no effect is - H0: β5 = 0
#Q2.2.ii.   GPA (t=2.749) is way more significant that LSAT(t=1.171)
#           a joint significance wont hold much value as GPA is way more significant than the LSAT
#           also the fstastic is 9.95 and p value is 0.0001 with 2 and 130df
#Q2.2.iii.  clsize and faculty have a poor joint significance
#           fstat for joint significance is 0.9484 and p value is 0.39
#           they will be significant only at a very high significance level
#Q2.2.iv.   "studfac" ratio from the data set feels like it could have an impact
#           also factors relating to the faculty itself like faculty ranking, faculty quality, etc 
#           could also have an impact on the salary

law <- datafetch("lawsch85")
law1 <- law[which(!is.na(law$LSAT) & !is.na(law$GPA)),]
model1 <- lm(data = law1, log(salary) ~ LSAT + GPA + log(libvol) + log(cost) + rank)
summary(model1)
model2 <- lm(data = law1, log(salary) ~ log(libvol) + log(cost) + rank)
summary(model2)
anova(model1,model2)
law2 <- law[which(!is.na(law$clsize) & !is.na(law$faculty)),]
model3 <- lm(data = law2, log(salary) ~ LSAT + GPA + log(libvol) + log(cost) + rank + clsize + faculty)
summary(model3)
model4 <- lm(data = law2, log(salary) ~ LSAT + GPA + log(libvol) + log(cost) + rank)
summary(model4)
anova(model3,model4)

########################
#     Question 2.3     #
########################

#Q2.3.i.    θ1 = 150*0.0003794 + 0.02888 = 0.08579 or a 150sqft bedroom adds 8.58% to the house price
#Q2.3.ii.   β2 = θ1 - 150*β1 or
#           log(price) = β0 + β1*sqrft + (θ1 - 150*β1)*bdrms
#Q2.3.iii.  θ1 = 0.0858 se(θ1) = 0.02677
#           confint = estimate +- 1.96 stderror
#           or for θ1 it is 0.0333 and 0.1383 or 3.3% to 13.83%

hprice <- datafetch("hprice1")
model1 <- lm(data = hprice, log(price) ~ sqrft + bdrms)
summary(model1)
model2 <- lm(data = hprice, log(price) ~ I(sqrft - 150*bdrms) + bdrms)
summary(model2)
0.0858+1.96*(0.02677)
0.0858-1.96*0.02677
confint(model2)

########################
#     Question 2.4     #
########################

#Q2.4.i.    H0: b2 = b3
#Q2.4.ii.   say b2 - b3 = t1
#           so  b2 = t1 + b3
#           log(wage) = b0 + b1*educ + (t1+b3)*exper + b3*tenure
#           log(wage) = b0 + b1*educ + t1*exper + b3(tenure+exper)
#           t1 estimate = 0.00195
#           t1 stderror = 0.00474
#           95% CI is -0.0073 to 0.0112 or the value is super close to zero
#           since the 95% interval is practically zero we fail to reject the null hypothesis

wage <- datafetch("wage2")
model1 <- lm(data = wage, log(wage) ~ educ + exper + I(tenure+exper))
summary(model1)
0.00195-1.96*0.00474
0.00195+1.96*0.00474
confint(model1)

########################
#     Question 2.5     #
########################

#Q2.5.i.    there are 2017 single person households in the dataset
#Q2.5.ii.   nettfa = -43.0398 + 0.799*inc + 0.843*age
#           net financial wealth has a positive relation to both income and age, not surprising
#Q2.5.iii.  slope is meaningless as no one exists with age = 0 and income = 0
#Q2.5.iv.   h0: b2=1 and h1: b2<1
#           being a one sided hypothesis the pvalue is 0.044 hence we can reject the null against the one sided alternative
#Q2.5.v.    the coeff is 0.8207 against the 0.79932 in the previous question
#           it is not a big difference and this is so because the correlation between age and inc is very low(0.04)
#           so a linear model with just inc and one with inc+age will not have a big differnce in the estimate for inc

k401 <- datafetch("401ksubs")
k401 <- k401[which(fsize == 1),]
nrow(k401)
model1 <- lm(data = k401, nettfa ~ inc + age)
summary(model1)
(0.843-1)/0.092
pt(-1.7065,df=length(k401$age)-1)
model2 <- lm(data = k401, nettfa ~ inc)
summary(model2)
cor(k401$age,k401$inc)

########################
#     Question 2.6     #
########################

#Q2.6.i.    log(price) = 8.258 + 0.317*log(dist)
#           distance should have a postive relation to price as living close to an incinerator is not normal
#Q2.6.ii.   the coeff for log(dist) is now 0.028
#           this fall in coeff makes sense as we are now controlling for far more important factors like area, rooms, bathrooms etc
#           the effect of incinerator distance becomes miniscule compared to these more important features for a house
#Q2.6.iii.  adding log(intst)^2 had a huge change on the significance of log(dist)
#           so we can conclude that even though log(dist) is mostly insignificant there is a non linear correlation 
#           between dist and intst
#Q2.6.iv.   the estimate is only -0.1 and tvalue is -1.105 and a p-val of 0.27 so it is not very significant

kiel <- datafetch("kielmc")
model1 <- lm(data = kiel, log(price) ~ log(dist))
summary(model1)
model2 <- lm(data = kiel, log(price) ~ log(dist) + log(intst) + log(area) + log(land) + rooms + baths + age)
summary(model2)
model3 <- lm(data = kiel, log(price) ~ log(dist) + log(intst) + log(area) + log(land) + rooms + baths + age + I(log(intst)^2))
summary(model3)
model4 <- lm(data = kiel, log(price) ~ log(dist) + log(intst) + log(area) + log(land) + rooms + baths + age + I(log(intst)^2) + I(log(dist)^2))
summary(model4)

########################
#     Question 2.7     #
########################

#Q2.7.i.    log(wage) = 0.126 + 0.09062*educ + 0.04097*exper - 0.00071*exper^2
#Q2.7.ii.   the p-val for exper^2 is 1.63e-9 which is super low so exper^2 is super significant at the 1% or even lower level (99% CI)
#Q2.7.iii.  delta(exper) = 1 in both cases
#           %delta(wage) = 100(0.04097 - 2*0.00071*exper)
#           for the fifth it is 3.52%
#           for the twentieth it is 1.39%
#Q2.7.iv.   based on the approximation in the previous question
#           turnaround will be at exper = 0.04097/(2*0.00071) 
#           or experience greater than 28.85 years will start having a negative impact on wage
#           121 people or 23% of people have 29 or more years of experience, a significant number

wage <- datafetch("wage1")
model1 <- lm(data = wage, log(wage) ~ educ + exper + I(exper^2))
summary(model1)
100*(0.04097 - 2*0.00071*4)
100*(0.04097 - 2*0.00071*19)
0.04097/(2*0.00071)
nrow(wage[which(exper>28.85),])
121/526*100

########################
#     Question 2.8     #
########################

#Q2.8.i.    log(wage) = b0 + b1*educ + b2*exper + b3*educ*exper
#           log(wage) = b0 +educ*(b1+b3*exper) + b2*exper
#           keeping exper const delta(log(wage)) on delta(educ) = 1 will be b1+b3*exper
#Q2.8.ii.   H0: b3 = 0
#           what makes more sense is that people with more experience AND more education will earn more money 
#           or that b3 > 0 makes more sense as a hypothesis
#Q2.8.iii.  the t value for the combo variable is 2.095
#           the pvalue is 0.018 
#           so we can reject b3 = 0 against b3 > 0 at a 1.8% confidence level
#Q2.8.iv.   t1 = b1 + 10*b3 or b1 = t1 - 10*b3
#           log(wage) = b0 + t1*educ - 10*b3*educ + b2*exper + b3*(exper*educ)
#           log(wage) = b0 + t1*educ + b2*exper + b3*educ(exper-10)
#           t1 estimate = 0.0761 se(t1) = 0.0066
#           95% CI for t1 is 0.0631 to 0.0891

wage <- datafetch("wage2")
model1 <- lm(data = wage, log(wage) ~ educ + exper + I(educ*exper))
summary(model1)
pt(-2.095,df=nrow(wage)-1)
model2 <- lm(data = wage, log(wage) ~ educ + exper + I(educ*(exper-10)))
summary(model2)
confint(model2)

########################
#     Question 2.9     #
########################

#Q2.9.i.    sat = 997.981 + 19.814*hsize - 2.131*hsize^2
#           t value is -3.881 hence hsize^2 is very significant
#Q2.9.ii.   maximizing for sat we get
#           19.814 - 2.131*2*hsize = 0
#           or hsize = 4.648
#           so the optimal class size is 465 students
#           this is too big and the rsquare being 0.00765 shows that the model is a bad one hence the size optimization is off
#Q2.9.iii.  since giving the SAT is a pre-condition here we are not considering high school students who have not given the SAT
#           a more representative sample would include both SAT takers and non-takers
#Q2.9.iv.   log(sat) = 6.896 + 0.0196*hsize - 0.00208*hsize^2
#           so optimal hsize will be
#           0.0196 + 0.00208*2*hsize = 0
#           or hsize = 4.711
#           the optimal class size is 471 which is very very close to the value from the previous model

gpa <- datafetch("gpa2")
model1 <- lm(data = gpa, sat ~ hsize + I(hsize^2))
summary(model1)
19.814/2/2.131
model2 <- lm(data = gpa, log(sat) ~ hsize + I(hsize^2))
summary(model2)
0.0196/2/0.00208

########################
#     Question 2.10    #
########################

#Q2.10.i.   log(price) = -1.297 + 0.16797*log(lotsize) + 0.70023*log(sqrft) + 0.03696*bdrms rsquare = 0.643
#Q2.10.ii.  log(price) = 5.9929
#           so based on this price = a0*e^5.9929
#           a0 = sum-over-i-to-n(e^(residual-i-to-n))/n
#           a0 = sum(exp(model1$residuals))/nrow(hprice) = 1.016348
#           predicted price = 1.016348*exp(5.9929) = 407.1232
#           hence the predicted price for a 20000 lotsize 2500 squarefoot 4 bedroom house is $417,123.20
#Q2.10.iii. price = -0.2177 + 0.002068*lotsize + 0.1228*sqrft + 1.385*bdrms rsquare = 0.672
#Q2.10.iv.  the error in prediction for the second model has a greater standard deviation than the first (53 over 58.8)
#           as a result we can comfortably say that the first model is a better predictor and fits the analysis better
#           Model 1 is a better predictor as compared to Model 2
#           based on calculated fields err1 and err2 in hprice

hprice <- datafetch("hprice1")
model1 <- lm(data = hprice, log(price) ~ log(lotsize) + log(sqrft) + bdrms)
summary(model1)
pred <- data.frame(lotsize = 20000, sqrft = 2500, bdrms = 4)
predict(model1,pred)
sum(exp(model1$residuals))/nrow(hprice)
1.016348*exp(5.9929)
model2 <- lm(data = hprice, price ~ lotsize + sqrft + bdrms)
summary(model2)

hprice$pred1 <- 1.016348*exp(predict(model1))
hprice$pred2 <- predict(model2)

hprice$err1 <- hprice$price - hprice$pred1
hprice$err2 <- hprice$price - hprice$pred2

mean(hprice$err1)   #0.2448
sd(hprice$err1)     #53.00776

mean(hprice$err2)   #-4.4e-14
sd(hprice$err2)     #58.79282

