rm(list=ls(all=TRUE))

library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich)
library(margins)
library(partykit)
library(plm)

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
plot.pca <- function(wss) {
  plot(0:(NROW(wss)-1), wss, type="b", xlab="Number of PCs", ylab="Eigenvalues")
}
pca.select <- function(pca.model,n.max=10){
  eigen <- pca.model$sdev^2
  if(NROW(eigen)>=n.max) eigen<-eigen[1:n.max]
  plot.pca(eigen)
  return(eratio(eigen)-1)
}
pc.test <- function(data,test.var,max.factor=NULL){
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

## Prepare workspace
con <- dbConnect(SQLite(),'wooldridge.db')
con %>% dbListTables
card <- con %>% dbReadTable('card') %>% data.table
jtrain <- con %>% dbReadTable('jtrain') %>% data.table
dbReadTable(con,'card_labels')
dbDisconnect(con)


summary(card)
head(card,10)

card$married <- NULL
cardsm <- card[complete.cases(card)]
#complete cases picks up only rows whwre all variables are present. 
#In general omits all NA values.

summary(lm(wage~educ+age+enroll+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=card))
# Q: For 949 of the data points, IQ is missing. How was this handled in the above model?
# A: Those data points were dropped by R. 
#Qns on missing data: Observations deleted due to missingness warnings at the end of regression output. 

# Q: Is it reasonable to drop data points with missing IQ values?
# A: It seems fairly reasonable because IQ was probably missing due to the cost of testing. This is probably not seriously correlated with the other variables.
#So there might not be too much bias cux of this. It might not be correlated with the other things. 

# Q: Interpret the estimated coefficient on library card. 
# A: If you had a library card when you were 14, you have (on average) a $0.06 increase in your wage in '76 than a person who didn't.
BIC(lm(wage~educ+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=cardsm))

# Q: Which model is the best of the ones listed on page X?
# A: (One with lowest BIC) put model here
step(lm(wage~educ+age+enroll+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=cardsm),k=log(nrow(cardsm))) %>% summary

summary(lm(wage~educ+age+enroll+black+smsa+south+KWW+libcrd14,data=card))
coeftest(lm(wage~educ+age+enroll+black+smsa+south+KWW+libcrd14,data=card),vcovHC)
# Q: What is the result of the test that library cards have a significant impact on the wage? 
# A: According to both the OLS and the vcovHC models, library cards are insignficant. 

summary(lm(log(wage)~educ+age+black+smsa+south+KWW+libcrd14,data=card))
# Q: Interpret the estimated coefficient on south in the above equation.
# A: Someone who lives in the south has a 12% lower wage than someone who does not live in the south (on average)


card$college <- as.numeric(card$educ>=16)
cardsm$college <- as.numeric(cardsm$educ>=16)

summary(card$college)
# Linear probability model
#When u have a binary outcome variable we should use a glm but betas might be wrong. 
#ALWAYS USE GLM FOR BINARY OUTCOME VARIABLE
#When using glm we have to use margins to read/ interpret the coefficients. 

summary(lm(college~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=card))
summary(step(lm(college~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=cardsm)))
summary(step(lm(college~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=cardsm),k=log(nrow(cardsm))))
# Q: Interpret the estimated coefficient on father's education.
# A: For every 1 year increase in fatheduc, a person is 2% more likely to complete a college degree
summary(glm(college~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=card,family=binomial))
# Q: How does a 1 year increase in motheduc affect a person's likelihood of finishing college?
margins(glm(college~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=card,family=binomial))
# A: A 1 year increase in motheduc ==> 1.8% increase in the prob of finishing college
summary(step(glm(college~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=card,family=binomial)))
margins(step(glm(college~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=card,family=binomial)))
coeftest(step(glm(college~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=card,family=binomial)),vcovHC)
# A: A 1 year inc. in motheduc ==> 1.8% increase in the probability of completing college

summary(glm(educ~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+KWW+IQ+libcrd14,data=card,family=poisson))
# Q: Interpret the estimated coefficent beta_1.
# A: Every 1 year increase in mother's education ==> 0.65630% increase in the years of education a person will attain (on average)

#Treat it like a natural log model. Just multiply by 100 and interpret the coefficient. 


set.seed(500)
model <- ctree(as.factor(college)~motheduc+fatheduc+age+momdad14+sinmom14+step14+black+smsa+south+libcrd14,data=card)
model
#without as.factor the box plots are not really informative. So use as.factor. 

plot(model)
# Q: What does the model say will happen to a person where mothed = fathed = 12 who had a library card whose mother and father were together when they were 14?
# A: There is a 40% chance that such a person would complete college.
#Father has 12 yrs, (small left, right bigger no's), mother > 11 -> motheduc = 12 -> momdad14 >0 -> fatheduc > 12 -> 0.4 or 40%. 

cardsmiq <- card[complete.cases(card[,.(KWW,IQ)])] 
#Took complete cases for KWW and IQ. 
# Kmeans does not handle missing values very well.

wssa <- cardsmiq %>% select(KWW,IQ) %>% scale %>% kmeans.wss
wssb <- cardsmiq %>% select(KWW,IQ) %>% kmeans.wss
plot.wss(wssa)
eratio(wssa)
# Q: Which wss is better, wssa or wssb?
# A: wssa has scaling so it is better
#Scaling is akways vetter for kmeans. 
# Q: How many clusters should we use in this case?
# A: The elbow is at 2 and the eratio says 2
model <- kmeans(cardsmiq %>% select(KWW,IQ) %>% scale,centers=2,nstart=10)
model$centers
sd(cardsmiq$KWW)
sd(cardsmiq$IQ)

cardsmiq[,.(mean(KWW),mean(IQ)),by=model$cluster]
# Q: What are the cluster centers?
# A: Lower group: KWW=29.17,IQ=90.2; Higher group: KWW=39.96,IQ=110.8
#Model centers * sd(variable) + mean(variable)
#cardsmiq[,.(mean(KWW),mean(IQ)),by=model$cluster] or u can use this. 
model$cluster
cardsmiq$grp <- model$cluster

# Pooling:
# Pooling: all groups are treated as the same
summary(lm(log(wage)~educ+age+black+smsa+south+KWW,data=cardsmiq))
# Within:
# Within:diff intercept for each one of the groups 
summary(lm(log(wage)~as.factor(grp)+educ+age+black+smsa+south+KWW,data=cardsmiq))
summary(lm(log(wage)~as.factor(grp)-1+educ+age+black+smsa+south+KWW,data=cardsmiq))

# Within and pooled look basically the same.
#they have very minute difference. They are basically the same.

wss <- cardsmiq %>% select(KWW,IQ) %>% hclust.wss
plot.wss(wss)
eratio(wss)

model <- cardsmiq %>% select(KWW,IQ) %>% dist %>% hclust()
plot(model)
cardsmiq$hgrp <- cutree(model,4)

# Pooling model
summary(lm(log(wage)~educ+age+black+smsa+south,data=cardsmiq))
# Within groups
summary(lm(log(wage)~as.factor(hgrp)+educ+age+black+smsa+south,data=cardsmiq))
#interpretting education: Why is it different in these 2 models? 
#pooling : 0.0221
#fixed: 0.0195 
#formula or hint: 1. Who are my groups or individuals? Here, they are 4 groups based on IQ and KWW score.
#2. Do the within groups interpreation: Almost, they will make sense easier than pooling. 
# For a given (IQ and KWW) group a one year increase in education will relate to a 2.3% increase in the wage.
#3.Do the pooled interpretation. 
# There is a positive relation between educ and wage. Good educ will lead to better income. 
# The correlation between educ and KWW,IQ makes educ look more important than it is. 3.0%
#whatever the difference is its cuz of some correlation between the x variable and the clustering variable.
#Between the groups, theres is soem positive relationship that makes beta in pooling higher 
#within - fixed effect
#between - pooled ols - fixed 
#overall - pooledols. 

# Q: Explain why south has a different effect in the within groups model than in the pooling model.
# A: When you are stuck in one group (low iq or high iq), living in the south has a smaller effect 
#than when the model allows for movement between groups (where its higher).

# Q: Why does education have a different effect in the pooled vs within models?
# A: Groups are based on KWW and IQ (intelligence)
# A: For a given level of intelligence, getting an extra year of education gets you a 2.3% increase in your expected wage
# A: But Across different levels of IQ, getting an extra year of education is correlated with a 3.0% increase 
# in the wage because education is correlated with intelligence.
# Q: Explain why south has a different effect in the within groups model than in the pooling model.
# A: When you are stuck in one group (low iq or high iq), living in the south has a smaller effect than when the model allows for movement between groups.


cardsm %>% select(educ,motheduc,fatheduc,KWW,IQ) %>% cor
pca <- cardsm %>% select(educ,motheduc,fatheduc,KWW,IQ) %>% prcomp(scale=TRUE)
pca.select(pca)
pc.test(cardsm %>% select(educ,motheduc,fatheduc,KWW,IQ),cardsm$educ)
tmp <- cardsm %>% select(educ,motheduc,fatheduc,KWW,IQ) %>% scale %>% rowMeans
pc.test(cardsm %>% select(educ,motheduc,fatheduc,KWW,IQ),tmp)

#Works best if there are no categorical or binary variables. 
#shoudl scale pca before doing it. 

# Q: How many pcs should we use?
# A: 1
#nothing abour rotation in the test 
#gives u all the rotations 
pca$rotation
cardsm$pc <- pca$x[,1]

summary(lm(log(wage)~age+black+smsa+south+pc,data=cardsm))
# Q: What is the effect of living in the south on the wage?
# A: Controlling for age,black,urban/rural, and the principal component from education/IQ, living in the south has a 7.6% negative effect on the wage.

summary(jtrain)
head(jtrain,10)
jtrainpdf <- jtrain %>% pdata.frame(index=c('fcode','year'))
summary(jtrainpdf)

summary(plm(log(scrap)~as.factor(year)+grant+lag(grant),data=jtrainpdf,model="pooling"))
#If u get a grant this higher the scrap rate is higher.
#doesnt make sense
summary(plm(log(scrap)~as.factor(year)+grant+lag(grant),data=jtrainpdf,model="within"))
#if u get a grant, the scrap rate goes down. 
#intercept is usually gone. 

#1. Who are my groups/ indiv ? the different firms. labeled b fcode. 
#2. Withingroups: for a given firm, getting a grant 6% reduction in scrp this year and 14% decrease in scrap next year. (SENSE)
#3. Pooling, Between firms there is a positive correlation b/w gettinga grant and high scrap rate. 

#lag(grant) - did they get the grant last year ? 

# Q: What is the difference between pooled vs within?
# A: Individuals are the firms
# A: For a given firm, getting a grant is associated with a 6% decrease in the scrap rate this year and a 14% decrease in the scrap rate next year.
# A: Between firms, there is a positive correlation between getting grants and high scrap rates.
#This is because only bad firms took grants in the first place.

#scenario: firm not doiung well, ssome one offers a grant, takes ur workers and teach production makes u to reduce scrap. Firms with low scrap 
#wouldnt have taken this as they already have low scrao rate.