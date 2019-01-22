rm(list=ls())

library(data.table)
library(dplyr)
library(partykit)
library(ggplot2)
library(plyr)

auto.kmeans <- function(data,maxclu=10,seed=1,nstart=10) { 
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}

auto.hclust <- function(data,model=hclust(dist(data)),maxclu=10) {
  wss <- rep(NA,maxclu)
  r <- ncol(data)+1
  for(i in 1:maxclu) {
    groups <- cutree(model,i)
    means <- data[order(groups),lapply(.SD,mean),by=groups]
    means <- means[,2:r]
    demeaned <- data - means[groups]
    wss[i] <- sum(rowSums(demeaned^2))
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}

# Q5.1 a
# The data generation process creates variables using the normal distribution generator function rnorn for given mean and standard deviation
# The rnorm generates z and w as normal distributions with n = 500, mean = 0 sdev = 1
# The variable x is calculated as 5*z + 50/80/30 in the 3 groups
# The variable y is calculated as -100/-80/-120*z+ 1100/1200/1000 + 50*w in the three groups
# y is then rounded to the nearest 10s in all three cases/groups
# And finally y is bound to a min value of 200 and a max value of 1600 in all 3 cases/groups
# x is named as "income" and y as "sat", a running "id" column is also present in each group, along with a "group" identifier as 1,2 or 3
# Finally all 3 groups are merged into "dtable" and the group variable is changed into a factor
# A new id column "iid" generated [same as "id" for all practical purposes]

n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge(dt1    ,dt2, all=TRUE)
dtable <- merge(dtable ,dt3, all=TRUE)
dtable$group <- as.factor(dtable$group)
dtable$iid <- 1:1500

# Q5.1 b
# What we are observing here is the Simpson paradox. [we are assuming no knowledge of the data generation we did previously]
# The data when taken as a whole shows that income has a postive effect on SAT as shown by the pooled OLS model1
# Whereas when we run the model within groups and for individual groups we see a telling change in the sign for income's effect on 
# SAT showing there is a negative effect of income increase on SAT within the differnt groups

model1 <- lm(data = dtable, sat ~ income)
model2 <- lm(data = dtable, sat ~ income + group)
model3 <- lm(data = dtable[group == 1], sat ~ income)
model4 <- lm(data = dtable[group == 2], sat ~ income)
model5 <- lm(data = dtable[group == 3], sat ~ income)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

# Q5.1 c
# model1 - 17 nodes
# From the first model we see that there is one partition with 1001 records with way too much variance in the data
# This model does not really explain the separation all too well of SAT based on income
# model2 - 4 nodes
# The second model is again quite obvious, tree partitioning on groups will just show nodes based on the three groups
# model3 - 53 nodes
# This model also first separates on groups as the obvious partitioning choice before going deeper into income partitions

model1 <- ctree(data = dtable, sat ~ income)
model2 <- ctree(data = dtable, sat ~ group)
model3 <- ctree(data = dtable, sat ~ income + group)

plot(model1)
plot(model2)
plot(model3)


# Q5.1 d
# The best model to fit the generated data is 
# model3 <- glmtree(data = dtable, sat ~ income + group)

model1 <- glmtree(data = dtable, sat ~ income)
model2 <- glmtree(data = dtable, sat ~ group)
model3 <- glmtree(data = dtable, sat ~ income + group)
model4 <- glmtree(data = dtable, sat ~ income + group + income|group)

sqrt(mean((dtable$sat-predict(model1,dtable))^2))
sqrt(mean((dtable$sat-predict(model2,dtable))^2))
sqrt(mean((dtable$sat-predict(model3,dtable))^2))
sqrt(mean((dtable$sat-predict(model4,dtable))^2))

# Q5.1 e
# The auto.kmeans functions shows elbow at 3 with the max diff from previous number of clusters in the wihtin sum of squares
# For the three clusters we see that the cluster centres and the means of the three groups we defined in data generation are
# very very close.
# From the model we also see that there are only 3 records that are assigned a cluster different from the groups we defined
# in the data generation process

auto.kmeans(dtable)
set.seed(1)
model1 <- kmeans(dtable, centers = 3, nstart = 10)
model1$centers
sum(model1$cluster == dtable$group)
mean(model1$cluster == dtable$group)
mean(dtable$sat[dtable$group==1])
mean(dtable$sat[dtable$group==2])
mean(dtable$sat[dtable$group==3])
plot(dtable$sat,dtable$income)
x <- dtable
x$clust_kmeans <- model1$cluster
qplot(group, income, data = x, colour = as.factor(clust_kmeans))

# Q5.1 f
# kmeans with 3 clusters is able to predict the groups correctly 1496/1500 times
# 498 correct preds for cluster1's groups
# 499 correct preds for cluster2's groups
# 499 correct preds for cluster3's groups
# kmeans gets it wrong a total of 4 times
# heirarchichal clustering gives the following results
# 431 correct preds for cluster1's groups
# 498 correct preds for cluster2's groups
# 462 correct preds for cluster3's groups
# heirarchical gets it wrong 109 times

clust1_kmeans_correct <- sum(x[which(x$group==1)]$group == x[which(x$group==1)]$clust_kmeans)
clust2_kmeans_correct <- sum(x[which(x$group==2)]$group == x[which(x$group==2)]$clust_kmeans)
clust3_kmeans_correct <- sum(x[which(x$group==3)]$group == x[which(x$group==3)]$clust_kmeans)

dtable$group <- as.integer(dtable$group)
auto.hclust(dtable)
model1 <- hclust(dist(dtable))
plot(model1)
clust_heir <- cutree(model1,k=3)
x$clust_heir <- clust_heir 
qplot(group, income, data = x, colour = as.factor(clust_heir))

clust1_heir_correct <- sum(x[which(x$group==1)]$group == x[which(x$group==1)]$clust_heir)
clust2_heir_correct <- sum(x[which(x$group==2)]$group == x[which(x$group==2)]$clust_heir)
clust3_heir_correct <- sum(x[which(x$group==3)]$group == x[which(x$group==3)]$clust_heir)

# Q5.1 g
# The pooled model will be same always because we do not consider the data for different groups
# for the within groups models we see the following
# The original model shows a -20 beta on income
# The model within groups for clust_kmeans shows a -16 beta, the few wrong measurements throw off the beta by a little but not a lot
# The model within groups for clust_heir shows a -2 beta, the increased number of
# wrong measurements throw off the beta by a lot in this case
# We can say the kmeans model adequately represents the equations presented by the data generation process
# this is so because the common factor z in the normal generation is -5 times and 100 times in the two variables sat and income
# This shows that the beta on income compared to sat should be about -20 as shown by model2_1

model1 <- lm(data = x, sat ~ income) # pooled model remains the same in both cases

model2_1 <- lm(data = x, sat ~ income + group) # within for groupings
model2_2 <- lm(data = x, sat ~ income + as.factor(clust_kmeans)) # within for kmeans clusters
model2_3 <- lm(data = x, sat ~ income + as.factor(clust_heir)) # within for heirarchichal clusters

summary(model1)
summary(model2_1)
summary(model2_2)
summary(model2_3)

qplot(income, sat, data = x, colour = as.factor(group))
qplot(income, sat, data = x, colour = as.factor(clust_heir))
qplot(income, sat, data = x, colour = as.factor(clust_kmeans))

# Q5.1 h
# > model1$centers
# income
# 1 30.15980
# 2 79.76588
# 3 49.75284
# We see that the data generation process and the clustering process show that the 1st cluster is group 3 in our data
# 2nd cluster and group are the same and the 3rd cluster is actually representative of group 1 in our data
# The predictions are correct the following times for each group
# for group 1 489 times correct out of 500
# for group 2 500 times correct out of 500
# for group 3 484 times correct out of 500
# The kmeans estimation using only the income variable is correct 1473 times out of 1500

data_inc <- dtable[,3]
auto.kmeans(data_inc)
set.seed(1)
model1 <- kmeans(data_inc, centers = 3, nstart = 10)
model1$centers
data_inc$clust_kmeans <- model1$cluster
data_inc$group <- dtable$group
sum(data_inc$group == 3 & data_inc$clust_kmeans == 1)
sum(data_inc$group == 1 & data_inc$clust_kmeans == 3)
sum(data_inc$group == 2 & data_inc$clust_kmeans == 2)

# Q5.1 i
# > plyr::count(data_incsat,c("group","clust_kmeans"))
# group clust_kmeans freq
# 1            2  371
# 1            3  129
# 2            1  500
# 3            2  170
# 3            3  330
# from the above we can see that 1st cluster is group 2 in our data, 2nd cluster is group 1 and 3rd cluster is group 3
# we see a worse performance as compared to the previous model in part h
# but a better comparison is with the model using only sat and income without scaling
# the scaled model performs way better in that case as compared to an unscaled one
# The results are as follows
# for group 1 371 times correct out of 500
# for group 2 500 times correct out of 500
# for group 3 330 times correct out of 500
# The kmeans estimation using only the income variable is correct 1201 times out of 1500
# The results for this model with unscaled sat and income data is as follows
# for group 1 256 times correct out of 500
# for group 2 343 times correct out of 500
# for group 3 267 times correct out of 500
# which is way worse than the scaled model for sat and income


data_incsat <- as.data.frame(scale(dtable[,c(2,3)]))
auto.kmeans(data_incsat)
model1 <- kmeans(data_incsat, centers = 3, nstart = 10)
model1$centers
data_incsat$group <- dtable$group
data_incsat$clust_kmeans <- model1$cluster
plyr::count(data_incsat,c("group","clust_kmeans"))