setwd('c:/data/BUAN6357/HW_5'); source('prep.txt', echo=T)
library(data.table)
raw <- read.table('Concrete_Data_wj.csv',header = T,sep = ',')
raw$idx <- as.numeric(rownames(raw))
raw <- as.data.table(raw)

basem_pred <- mean(raw$strength)

# set.seed(654432970)
# bs <- sample(1:1030)
# str <- as.list(raw$strength)
# resid_base <- raw$strength[c(bs)] - basem_pred
resid_base <- raw$strength - basem_pred
idx_base <- raw$idx

base_m <- data.table(cbind(resid_base,idx_base))

#simple model
set.seed(654432970)
x <- 1:1030
test_idx <- sample(x,103)
test <- raw[raw$idx %in% test_idx,]
train <- raw[!(raw$idx %in% test_idx),]

model <- lm(data = train, strength ~ .-idx)
resid_simple <- model$residuals
idx_simple <- train$idx

simple_m <- data.table(cbind(resid_simple,idx_simple))

test$prediction <- predict(model, test)
idx_cv <- test$idx
resid_cv <- test$strength - test$prediction
simple_cv <- data.table(cbind(resid_cv,idx_cv))

#jackknife
jk_m <- NULL
jk_cv <- NULL

for(i in 1:1030)
{
  m <- lm(strength~.-idx,data = raw[raw$idx!=i,])
  resid_jk <- m$residuals
  idx_jk <- raw$idx[raw$idx!=i]
  iter_jk <- i
  temp <- data.table(cbind(resid_jk,idx_jk,iter_jk))
  jk_m <- rbind(jk_m,temp)
  pred <- predict(m,raw[raw$idx==i,])
  resid_jk <- raw$strength[raw$idx==i] - pred
  idx_jk <- i
  temp2 <- data.table(cbind(resid_jk,idx_jk,iter_jk))
  jk_cv <- rbind(jk_cv,temp2)
}


#k fold cv models
set.seed(654432970)
idx <- sample(1:1030)
k <- rep(1:10,each=103)
xx <- data.table(cbind(idx,k))

k_m <- NULL
k_cv <- NULL

for(i in 1:10)
{
  train <- raw[raw$idx %in% xx$idx[xx$k != i],]
  test <- raw[raw$idx %in% xx$idx[xx$k == i],]
  
  m <- lm(strength~.-idx,data = train)
  
  resid_k <- m$residuals
  idx_k <- train$idx
  grp_k <- i
  temp <- data.table(cbind(resid_k,idx_k,grp_k))
  k_m <- rbind(k_m,temp)
  
  test$predict <- predict(m,test)
  resid_cv <- test$strength - test$predict
  idx_cv <- test$idx
  grp_cv <- i
  temp2 <- data.table(cbind(resid_cv,idx_cv,grp_cv))
  k_cv <- rbind(k_cv,temp2)
}

# is.data.table(raw)
# is.data.table(base_m)
# is.data.table(simple_m)
# is.data.table(simple_cv)
# is.data.table(jk_m)
# is.data.table(jk_cv)
# is.data.table(k_m)
# is.data.table(k_cv)

source('validate.txt', echo=T)