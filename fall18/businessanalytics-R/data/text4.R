setwd("C:/Users/jap090020/Dropbox/UTDallas/buan6356/post")
rm(list=ls())
library(data.table)
library(dplyr)
library(tm)
library(SnowballC)
library(lsa)

df <- fread('SMSSpamCollection',header=FALSE)
head(df)
colnames(df) <- c('spam','data')
nrow(df)
df$spam <- as.numeric(df$spam=="spam")

corp <- Corpus(VectorSource(df$data))
inspect(corp)
corp <- corp %>% tm_map(stripWhitespace)
corp <- corp %>% tm_map(removePunctuation)
corp <- corp %>% tm_map(removeNumbers)
swords <- c(stopwords('english'),'im','ur','u','i','k','ok','okay','yeah','you','Ã¼','Ãœ','ltgt','r','b')
corp <- corp %>% tm_map(removeWords,swords)
inspect(corp)
corp <- corp %>% tm_map(stripWhitespace)
inspect(corp)
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

tfidf <- weightTfIdf(tdm)
tfidf
lsa.tfidf <- lsa(tfidf,dim=20)
df2 <- data.table(as.matrix(lsa.tfidf$dk))
df2$spam <- df$spam

head(df2)
model <- glm(spam~.,data=df2,family=binomial())
prob <- predict(model,type="response")
for(i in seq(0.1,0.9,0.1)){
  pred <- as.numeric(prob>i)
  tbl <- table(pred,df$spam)
  p1 <- tbl[2,1]/sum(tbl[,1])
  p2 <- tbl[1,2]/sum(tbl[,2])
  #print(tbl)
  print(c(i,p1,p2))
}

model <- glm(spam~.,data=df2,family=binomial())
prob <- predict(model,type="response")
pred <- as.numeric(prob>0.5)
table(pred,df$spam)

## DONT TYPE THIS UNNECESSARY
dfnew <- data.table(t(as.matrix(tfidf)))
head(dfnew)
dfnew[1,1]
nrow(dfnew)
ncol(dfnew)
