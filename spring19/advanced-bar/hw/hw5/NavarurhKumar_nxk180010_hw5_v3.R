#setwd('c:/data/BUAN6357/HW_5'); source('prep.txt', echo=T)

library(data.table)
raw <- read.table('~/documents/ba-masters/spring19/advanced-bar/hw/hw5/Concrete_Data_wj.csv',header = T,sep = ',')
raw$idx <- as.numeric(rownames(raw))
raw <- as.data.table(raw)

# set.seed(654432970)
# bs <- sample(1:1030)
# str <- as.list(raw$strength)
# resid_base <- raw$strength[c(bs)] - basem_pred
model <- lm(data=raw,strength~.-idx)
resid_base <- model$residuals
idx_base <- raw$idx

base_m <- data.table(cbind(resid_base,idx_base))

#simple model
set.seed(654432970)
x <- 1:1030
test_idx <- sample(x,103)
test_idx <- as.data.table(test_idx)
colnames(test_idx) <- 'idx'
test <- setDT(raw)[test_idx, on="idx"]
train_idx <- raw$idx[-test$idx]
train_idx <- as.data.table(train_idx)
colnames(train_idx) <- 'idx'
train <- setDT(raw)[train_idx, on="idx"]

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
  test <- setDT(raw)[xx[xx$k==i,], on="idx"]
  
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
#source('validate.txt', echo=T)

#hw5b

mbm <- mean(base_m$resid_base)
sdbm <- sd(base_m$resid_base)
lb5 <- mbm - 0.674*sdbm
ub5 <- mbm + 0.674*sdbm
lb75 <- mbm - 1.1503494*sdbm
ub75 <- mbm + 1.1503494*sdbm
lb95 <- mbm - 1.959964*sdbm
ub95 <- mbm + 1.959964*sdbm
ubk5 <- mean(k_m$resid_k) + 0.674*sd(k_m$resid_k)
ubs5 <- mean(simple_m$resid_simple) + 0.674*sd(simple_m$resid_simple)
ubs5np <- mean(simple_cv$resid_cv) + 0.674*sd(simple_cv$resid_cv)
ubs95np <- mean(simple_cv$resid_cv) + 1.959964*sd(simple_cv$resid_cv)

k_cv$flag75 <- 0
k_cv$flag75[which(k_cv$resid_cv <= ub75 & k_cv$resid_cv >= lb75)] <- 1
sum(k_cv$flag75)/nrow(k_cv)

simple_cv$flag95 <- 0
simple_cv$flag95[which(simple_cv$resid_cv <= ub95 & simple_cv$resid_cv >= lb95)] <- 1
sum(simple_cv$flag95)/nrow(simple_cv)

simple_cv$flag75 <- 0
simple_cv$flag75[which(simple_cv$resid_cv <= ub75 & simple_cv$resid_cv >= lb75)] <- 1
sum(simple_cv$flag75)/nrow(simple_cv)

jk_cv$flag5 <- 0
jk_cv$flag5[which(jk_cv$resid_jk <= ub5 & jk_cv$resid_jk >= lb5)] <- 1
sum(jk_cv$flag5)/nrow(jk_cv)

jk_cv$flag95 <- 0
jk_cv$flag95[which(jk_cv$resid_jk <= ub95 & jk_cv$resid_jk >= lb95)] <- 1
sum(jk_cv$flag95)/nrow(jk_cv)
