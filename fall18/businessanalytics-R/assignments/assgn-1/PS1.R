#Problem Set 1
#Name - Navarurh Kumar
#NetID - NXK180010
#Class - BA with R Friday 4-7PM


#home directory definition
setwd("C:\\Users\\navar\\OneDrive\\Documents\\UTD\\assignments\\businessanalytics-R\\assgn-1")
#packages used for the assignment
library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)


#sql connection function to ease life
datafetch <- function(table_name){
  con <- SQLite() %>% dbConnect('wooldridge.db')
  con %>% dbListTables
  read_table <- con %>% dbReadTable(table_name) %>% data.table
  con %>% dbDisconnect
  return(read_table)
}



####start of assignment

#importing dataset 1
wage <- datafetch('wage1')
wage_lab <- datafetch('wage1_labels')
wage_lab

summary(wage)

#1.1
summary(wage$educ)
#1.2
mean(wage$wage)
#1.4
mean(wage$wage)*(218.056/56.9)
#1.5
table(wage$female)

#1.1 - the mean years of education is 12.56  years pointing to the fact that most people stopped after getting a high school education
#       the minimum years of education is 0 and the highest is 18
#1.2 - the mean wage is $5.90/hr which is pretty low by current standards
#1.3 - The CPI for 1976 was at 56.9 and for 2010 was at 218.056
#1.4 - The average hourly wage for 2010 as per CPI values is $22.64484 and yeah this is way more reasonable by even current day standards
#1.5 - The dataset has 274 men and 252 women




#importing dataset 2
meap <- datafetch('meap01')
meap_lab <- datafetch('meap01_labels')
meap_lab

summary(meap)

#2.1
summary(meap$math4)
#2.2
cnt_school <- nrow(meap)
cnt_school_perfect_math4 <- nrow(meap[which(meap$math4 == 100),])
cnt_school_perfect_math4/cnt_school*100
#2.3
nrow(meap[which(meap$math4 == 50),])
#2.4
mean(meap$math4) #71.91%
mean(meap$read4) #60.06%
#2.5
cor(meap$read4,meap$math4) #0.8427281
#2.6
summary(meap$exppp) #min is 1207, max is 11960
mean(meap$exppp) #5194.865
sd(meap$exppp) #1091.89
#2.7
(6000-5500)/5500*100 #9.1%
(log(6000) - log(5500))*100 #8.7%

#2.1  min= 0 max= 100 ; the lower limit is perfect math-wise but is real-world weird; 
#     it shows there are schools where no one passed/has a satisfactory performance in 4th grade maths :?
#2.2  1823 total schools in the sample, 
#     38 schools have a perfect pass rate in math4
#     2.084% of schools have a perfect pass rate for math4
#2.3  17 schools have an exact 50% pass rate in math4
#2.4  Math has a 71.9% mean pass rate compared to 60.1% for reading; we could argue a case for reading to be a harder course to pass in 4th grade
#2.5  A high correlation of 0.843 or 84.3% argues that schools with a good reading pass rate will also boast a good math pass rate
#2.6  An sd of $1091.89 across a mean value of $5194.87 shows a high variation in per pupil spending across the schools
#2.7  The actual increase is 9.1% and the log approximation is 8.7%, fair estimation




#importing dataset 3
k401 <- datafetch('401k')
k401_lab <- datafetch('401k_labels')
k401_lab

summary(k401)

#3.1
mean(k401$prate) #87.36%
mean(k401$mrate) #0.73%
#3.2
mod1 <- lm(data=k401,prate~mrate)
summary(mod1)
plot1 <- ggplot(data=k401, aes(x=mrate,y=prate)) +geom_point()
plot1
#3.3
summary(mod1)
plot1
#3.4
predict(mod1,data.frame(mrate = c(3.5))) #103.5892
#3.5

#3.1  the average participation and match rates are 87.36% and 0.73% respectively
#3.2  the linreg model for prate vs mrate is - prate = 83.0755 + 5.8611*mrate
#     the intercept is 83.0755[value for prate with mrate zero] and the slope is 5.8611[and this is the factor for mrate's contribution
#     to prate as it increases/decreases, a very high slope]
#     the r-squared is at 0.0747 showing that the relation between prate and mrate is definitely not a linear one as is evident from plot1
#3.3  following is the interpretation for the slope and intercept [same as above]
#     the intercept is 83.0755[value for prate with mrate zero] and the slope is 5.8611[and this is the factor for mrate's contribution
#     to prate as it increases/decreases, a very high slope]
#3.4  A predicted value of 103.59% for participation rate makes no sense as participation cannot be more than 100%
#3.5  Based on mod1 and plot1 we can see that the dependence of prate solely on mrate could be more of a logit model but for a linear model
#     predicting prate based only on mrate is a bad idea; as also made apparent by a super low rsquare of 0.0747
#     The rate at which match-rate affects participation-rate [5.8611] seems like a very high value 




#importing dataset 4
ceosal <- datafetch('ceosal2')
ceosal_lab <- datafetch('ceosal2_labels')
ceosal_lab

summary(ceosal)

#4.1
mean(ceosal$salary) #865.86 thousand dollars
mean(ceosal$comten) #22.5 years
#4.2
nrow(ceosal[which(ceosal$ceoten == 0),])
max(ceosal$ceoten)
#4.3
mod2 <- lm(data=ceosal,log(salary)~ceoten)
summary(mod2)

plot2 <- ggplot(data=ceosal, aes(x=ceoten,y=salary)) +geom_point()
plot2

#4.1  The average salary for a ceo is 865,864.4 dollars and the average tenure is 22.5years
#4.2  5 out of 177 are in their first year as CEO's and the max tenure of a CEO is at 58 years with a 37year period as the CEO out of that  
#4.3  log(salary) = 6.5 + 0.0097*ceoten
#     if the ceo tenure increases by a year then the salary will see a 0.97% bump




#importing dataset 5
wage2 <- datafetch('wage2')
wage2_lab <- datafetch('wage2_labels')
wage2_lab

summary(wage2)

#5.1
mean(wage2$wage)  #957.9455
mean(wage2$IQ)    #101.2824
sd(wage2$IQ)      #15.05264
#5.2
mod3 <- lm(data=wage2,wage~IQ)
plot3 <-  ggplot(wage2, aes(x = IQ, y = wage)) + 
          geom_point() +
          stat_smooth(method = "lm", col = "red")
summary(mod3)
plot3
#5.3
mod4 <- lm(data=wage2,log(wage)~IQ)
plot4 <-  ggplot(wage2, aes(x = IQ, y = log(wage))) + 
          geom_point() +
          stat_smooth(method = "lm", col = "red")
summary(mod4)
plot4


#5.1  The average wage is $957.95 and average IQ is 101 points
#     The sample sdev for IQ is at 15.05 so it sample is a fairly great representative of the population
#5.2  The linear model is as follows
#     wage = 116.9916 + 8.3031*IQ + e
#     so for a 15 point increase in IQ the wage will change by 15*8.3031 = 124.5465 or $124.55
#     the rsquared for this model is 0.095 which is fairly low pointing to the fact that wage is dependent on far more factors than just IQ
#5.3  log(wage) = 5.887 + 0.0088*(IQ) + e
#     if IQ increases by 15 then we see a 0.0088*15*100 = 13.2% change in predicted wage




#importing dataset 6
meap93 <- datafetch('meap93')
meap93_lab <- datafetch('meap93_labels')
meap93_lab

summary(meap93)

#6.1
mod5 <- lm(data=meap93,math10~expend)
summary(mod5)
plot5 <-  ggplot(meap93, aes(x = expend, y = math10)) + 
          geom_point() +
          stat_smooth(method = "lm", col = "red")
plot5
#6.2
#6.3
mod6 <- lm(data=meap93,math10~log(expend))
summary(mod6)
#6.4
#6.5
max(meap93$math10)
max(fitted.values(mod6))

#6.1  As per a linear model each dollar spent leads to a 0.0025% increase in math pass rate
#     but we can only have a max 100% pass rate so a diminishing rate of return flattening out at 100% sounds more reasonable than a
#     linear relationship
#6.2  as per math10 = b0 + b1(log(expend)) + u
#     a 10% increase in expend means 
#     math10_new = b0 + b1(log(1.1*expend)) + u
# or  math10_new = b0 + b1*(log(1.1) + log(expend)) + u
# so  delta_math10 = math10_new - math_10
# or  delta_math10 = b1*log(1.1)
# or  delta_math10 = 0.095*b1 or 0.1*b1 (approximately)
# so  a 10% increase in expend is a b1/10 point change in math10

#6.3  math10 = -69.341 + 11.164*(log(expend)) + u
#     the sample size is 408 and the rsquared stands at 0.02966
#     the model is not a good one and diminishing return model would make more sense here
#6.4  For a 10% increase in expend the math10 percentage point increase is only 11.164*log(1.1) = 1.06 point increase which is quite low
#6.5  The max math10 value for the dataset is 66.7% and max fitted as per model is 30.15% so we aren't even close to touching 100$
#     for this dataset and model





#importing dataset 7
hprice <- datafetch('hprice1')
hprice_lab <- datafetch('hprice1_labels')
hprice_lab

summary(hprice)

#7.1
mod7 <- lm(data=hprice,price~sqrft+bdrms)
summary(mod7)
#7.4
mod8 <- lm(data=hprice,log(price)~sqrft+bdrms)
summary(mod8)
#7.5
pred_data <- data.frame(sqrft = 2438, bdrms = 4)
predict(mod7,pred_data)
#7.6

#7.1  price = -19.315 + 0.12844*(sqrft) + 15.19819*(bdrms) + u
#7.2  holding square footage constant, the expected increase in price for a one bedroom increase is $15198.19
#7.3  the addition of 140sqft bedroom (single) will lead to a price increase of $33179.79
#7.4  a one unit increase in bedrooms has a 2.8% effect on price
#     whereas a one unit increase in square footage only has a 0.038% increase in price
#7.5  as per the OLS line the value of 2438sqft 4 bedroom house should be $354,605.20
#7.6  the first house has a 2438 square footage and 4 bedrooms
#     we predicted a price of $354605.20 and the listed price was $300,000
#     hence this data point has a residual of $54,605.20, so the buyer underpaid getting a great deal on the house
#     but this assessment is under the assumption of a linear relation between price vs squarefootage and number of bedrooms




#importing dataset 8
ceosal2 <- datafetch('ceosal2')
ceosal2_lab <- datafetch('ceosal2_labels')
ceosal2_lab

summary(ceosal2)

#8.1
mod9 <- lm(data=ceosal2,log(salary)~ log(sales) + log(mktval))
summary(mod9)
#8.2
mod10 <- lm(data=ceosal2,log(salary)~ log(sales) + log(mktval) + log(profits))
summary(ceosal2$profits) #holds negative values that the model will ignore
mod10 <- lm(data=ceosal2,log(salary)~ log(sales) + log(mktval) + profits)
summary(mod10)
#8.3
mod11 <- lm(data=ceosal2,log(salary)~ log(sales) + log(mktval) + profits + ceoten)
summary(mod11)
#8.4
cor(ceosal2$profits,log(ceosal2$mktval))

#8.1  log(salary) = 4.62092 + 0.16213*(log(sales)) + 0.10671*(log(mktval))
#8.2  since profits has negative values we cannot compute a log and hence cant create a constant variability model
#     the r-squared here is 0.2993 so these variables definitely do not give the full picture
#8.3  log(salary) = 4.558 + 0.1622*log(sales) + 0.1018*log(mktval) + 2.9*10^(-5)*profits + 0.01168*ceoten
#     so a one year increase in ceotenure keeping all else constant will have a 1.168% increase in salary
#8.4  the correlation between profits and log(mktval) is 0.777
#     these variables are highly correlated
#     rsquare for mod9 was 0.2991 and after including profits in mod10 it was 0.2993 showing the little effect due to correlation
#     the estimators we are using




#importing dataset 9
attend <- datafetch('attend')
attend_lab <- datafetch('attend_labels')
attend_lab

summary(attend)

#9.1
summary(attend$atndrte)
summary(attend$priGPA)
summary(attend$ACT)
#9.2
mod12 <- lm(data=attend,atndrte ~ priGPA + ACT)
summary(mod12)
#9.3
#9.4
pred_data <- data.frame(priGPA = 3.65, ACT = 20)
predict(mod12,pred_data)
nrow(attend[which(priGPA == 3.65 & ACT == 20),]) #1
attend$atndrte[which(attend$priGPA == 3.65 & attend$ACT == 20)] #87.5%
#9.5
pred_data <- data.frame(priGPA = 3.1, ACT = 21)
predict(mod12,pred_data) #93.16%
pred_data <- data.frame(priGPA = 2.1, ACT = 26)
predict(mod12,pred_data) #67.32%

#9.1            min     average     max
#     atndrte   6.25    81.71       100
#     priGPA    0.857   2.587       3.930
#     ACT       13      22.51       32
#9.2  atndrte = 75.7 + 17.261*priGPA - 1.717*ACT
#     the intercept (zero priGPA and zero ACT) says the predict attendance rate would be 75.7%
#     i mean if i did not attend any class and gave no tests i would have  a 0 gpa and 0 ACT score but also zero attendance
#     so the intercept is not very useful in and by itself
#9.3  priGPA has a coeff(slope) of +17.261 meaning one point gpa increase means an attendance increase of approx 17%
#     ACT has a -1.717 slope so a one point improvement in ACT score points to a decrease in attendance rate
#     the negative relation between ACT and attendance is very werid logically, it basically says a good ACT will mean the student has a
#     lower attendance rate
#9.4  the predicted attendance is 104.37% which makes no sense for a percentage
#     there is a record with the given priGPA(3.65) and ACT(20) values and the actual atndrte is 87.5% for that record
#9.5  The predicted difference is 25.84%




#importing dataset 10
htv <- datafetch('htv')
htv_lab <- datafetch('htv_labels')
htv_lab

summary(htv)

#10.1
range(htv$educ) 
nrow(htv[which(htv$educ == 12),])/nrow(htv)*100
mean(htv$educ)
mean(htv$motheduc)
mean(htv$fatheduc)
#10.2
mod13 <- lm(data=htv, educ ~ fatheduc + motheduc)
summary(mod13)
#10.3
mod14 <- lm(data=htv, educ ~ fatheduc + motheduc + abil)
summary(mod14)
#10.4
mod15 <- lm(data=htv, educ ~ fatheduc + motheduc + abil + I(abil^2))
summary(mod15)
#10.5
nrow(htv[which(htv$abil < -3.967094),]) #15
#10.6

#calculating the constant
8.240226+0.108939*mean(htv$fatheduc)+0.190126*mean(htv$motheduc) #11.91157

#plotting
plot7 <- ggplot(htv,aes(x=abil, y=educ)) 
plot7 <- plot7 + geom_point(shape=16,color = '#00D7F9', size = 1)
plot7 <- plot7 + geom_line(data = htv,aes(x=abil,y = 11.91157 + 0.401462*abil + 0.050599*I(abil^2)),color='#FF5757',size = 1.5)
plot7 <- plot7 + theme(panel.background = element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
                       panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray"), 
                       panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray"))
plot7 <- plot7 + xlab('Ability') + ylab('Education')
plot7



#10.1   The range for education is 6years and 20years
#       out of the 1230 men 41.63% of them have completed exactly 12 years of education
#       the average years of education is higher for the people compared to both their mothers and fathers
#10.2   educ = 6.96435 + 0.19029*fatheduc + 0.30420*motheduc
#       the rsquared is at 0.2493 hence the education of parents accounts for 24.93% of the sample's variation
#       motheduc having a higher coefficient compared to fatheduc makes sense as kids are mostly with their
#       mothers at home and any home tutoring is dependent on the education level of the mother
#10.3   educ = 6.96435 + 0.11109*fatheduc + 0.18913*motheduc + 0.50248*abil
#       the rsquared for this model stands at 0.4275, a significant increase
#       ability alongwith parent's education is a better explanation for educ
#10.4   educ = 8.240226 + 0.108939*fatheduc + 0.190126*motheduc + 0.401462*abil + 0.050599*abil^2
#       keeping motheduc and fatheduc constant and trying to find the minima for educ compared to abil we have
#       d(educ)/d(abil) = 0.401462 + 2*0.050599*abil
#       minimizing educ
#       0.101198*abil = -0.401462
#       abil for min educ = -3.967094
#10.5   15 out of 1230 or 1.22% of men have an ability rating of lower than -3.97 in the database
#       this shows that the model is actually relevant as the low point for education can only be zero
#       in relation if too many men in the dataset had an ability of lower than -3.97 then according to the minimization equation
#       with the minima at -3.97, the curve would move back upwards meaning people with say a possible ability rating of -10 or -7
#       even would end up having a better education than those with less negative abilities as per the quadratic equation
#       it is basically a relic of the minima existing at the -3.97 point
#10.6   plot7 variable shows the plot with a scatter between education vs abiity and the best fit line as calculated after
#       using the equation from model mod15 and setting father's and mother's education to their average values
#       the following variable holds the plot
plot7


