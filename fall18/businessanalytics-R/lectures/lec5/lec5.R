######################################
## Author:   Jason Parker
## Date:     2018-07-09
## Title:    lec5.R
## Purpose:  Classification, Recursive partitioning, and Naive Bayes
######################################

rm(list=ls())

## Import packages
library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(e1071) # Titanic data
library(party) # Recursive partitioning
library(partykit) # Pruning tree from party
library(bnlearn) # Bayesian networks + naive Bayes

# Connect to the database to get the data
data("Titanic")
titan <- data.table(Titanic)
rm(Titanic)
con <- SQLite() %>% dbConnect('wooldridge.db')
bwght <- con %>% dbReadTable('bwght') %>% data.table
wage1 <- con %>% dbReadTable('wage1') %>% data.table
con %>% dbDisconnect

#######################################
## Classification with the titanic data
#######################################

# De-sparse the titanic data
View(titan)
nrow(titan)
seq_len(nrow(titan))

titan$N
?rep.int
titan$N
repr <- rep.int(seq(1,nrow(titan),1), titan$N)
repr
titan <- titan[repr,]
titan <- titan[,!"N"]
View(titan)

# Convert strings to factors
titan$Survived <- as.factor(titan$Survived)
titan$Class <- as.factor(titan$Class)
titan$Sex <- as.factor(titan$Sex)
titan$Age <- as.factor(titan$Age)

# Using logit for classification
glmodel <- glm(Survived~.,data=titan,family=binomial())
glmodel %>% summary
pred1 <- glmodel %>% predict(type="response")
pred1 <- as.numeric(pred1 > 0.5)
mean(pred1 == as.numeric(titan$Survived=="Yes"))
table(pred1,titan$Survived) 

# Recursvive partitioning trees and classification
tree <- ctree(Survived~Class+Sex+Age,data=titan)
tree %>% plot
nodeprune(tree,c(3,10)) %>% plot
pred2 <- tree %>% predict
mean(pred2 == titan$Survived)
table(pred2,titan$Survived)

# Naive bayes and classification
bayes <- naive.bayes(titan,"Survived")
pred3 <- bayes %>% predict(titan)
mean(pred3 == titan$Survived)
table(pred3,titan$Survived)

table(pred3,pred1)

#######################################
## Recursive partitioning tree for bwght data
#######################################

bwght$smokes <- as.numeric(bwght$cigs>0)
bwght$smokes <- as.factor(bwght$smokes)
tree <- ctree(bwght~cigs+faminc+male+white,data=bwght)
tree <- ctree(cigs~faminc+white,data=bwght)
tree <- ctree(smokes~faminc+white,data=bwght)
plot(tree)

tree <- ctree(wage~educ+exper+tenure,data=wage1)
plot(tree)
nodeprune(tree,c(3,6)) %>% plot

#######################################
## glm+trees 
#######################################

glmtree(wage~educ+exper+tenure,data=wage1)
glmtree(wage~exper+tenure|educ,data=wage1)
glmtree(wage~tenure|educ+exper,data=wage1)
glmtree(wage~educ+exper+tenure|nonwhite+female+married,data=wage1)

glmtree(bwght~cigs+faminc+male+white,data=bwght)
glmtree(bwght~cigs|faminc+male+white,data=bwght)
tree <- glmtree(bwght~cigs|faminc+male+white,data=bwght)
(bwght$bwght-predict(tree,bwght))^2 %>% mean %>% sqrt
glmtree(bwght~faminc+male+white|cigs,data=bwght)
tree <- glmtree(bwght~faminc+male+white|cigs,data=bwght)
(bwght$bwght-predict(tree,bwght))^2 %>% mean %>% sqrt

tree<- glmtree(wage~educ+exper+tenure,data=wage1)
(wage1$wage-predict(tree,wage1))^2 %>% mean %>% sqrt
tree<- glmtree(wage~exper+tenure|educ,data=wage1)
(wage1$wage-predict(tree,wage1))^2 %>% mean %>% sqrt
tree<- glmtree(wage~tenure|educ+exper,data=wage1)
(wage1$wage-predict(tree,wage1))^2 %>% mean %>% sqrt
tree<- glmtree(wage~exper|educ+tenure,data=wage1)
(wage1$wage-predict(tree,wage1))^2 %>% mean %>% sqrt
tree<- glmtree(wage~educ+exper+tenure|educ+exper+tenure,data=wage1)
tree
(wage1$wage-predict(tree,wage1))^2 %>% mean %>% sqrt

tree <- glmtree(wage~educ+exper+tenure|nonwhite+female+married,data=wage1)
tree <- glmtree(wage~educ+exper+tenure+nonwhite+female+married|educ+exper+tenure+nonwhite+female+married,data=wage1)
(wage1$wage-predict(tree,wage1))^2 %>% mean %>% sqrt
tree
