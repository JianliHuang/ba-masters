setwd('c:/data/BUAN6357/HW_4'); source('prep.txt', echo=T)

require(partykit)       # for ctree()
# constants
cols      <- 7
byRows    <- 1
byCols    <- 2
seed      <- 1          # not a great choice
## parameters
nx        <- c(25,50,100)    # number of replications per digit (small for fast test)
p         <- 0.9   # probability a segment works correctly
# to help reading of code
classes   <- c(0,1,2,3,4,5,6,7,8,9)
minDigit  <- min(classes)
maxDigit  <- max(classes)
numDigits <- length(classes)
# generate simulated data observations
#set.seed(seed)
#generating final ans vars as numeric vectors
holdPClogit <- vector(mode="numeric", length=0)
holdMedBRlogit <-vector(mode="numeric", length=0)
hold75BRlogit <- vector(mode="numeric", length=0)
holdPCtree10 <- vector(mode="numeric", length=0)
holdMedBRtree10 <- vector(mode="numeric", length=0)
hold75BRtree10 <- vector(mode="numeric", length=0)
holdPCtree1 <- vector(mode="numeric", length=0)
holdMedBRtree1 <- vector(mode="numeric", length=0)
hold75BRtree1 <- vector(mode="numeric", length=0)

smpl <- list()

#generating the full sample set; to be subsetted later
for (n in c(1:3))
{
  set.seed(seed)
  t1 <- rep(classes, nx[n])
  t2 <- c(1,1,1,0,1,1,1,
          0,0,1,0,0,1,0,
          1,0,1,1,1,0,1,
          1,0,1,1,0,1,1,
          0,1,1,1,0,1,0,
          1,1,0,1,0,1,1,
          0,1,0,1,1,1,1,
          1,0,1,0,0,1,0,
          1,1,1,1,1,1,1,
          1,1,1,1,0,1,0)
  # segment patterns for each digit
  t3 <- rep(t2, nx[n])                 # noiseless data
  t4 <- rbinom(length(t3), 1, 1-p) # probability of failure event (noise)
  # flip the bits to add noise [ t4 == 1 designates failure event ]
  t5 <- ifelse(t4 == 1, 1-t3, t3)
  # reshape
  t5                  <- matrix(data=t5, nrow=length(classes)*nx[n], ncol=cols, byrow=T)
  dim(t1)             <- c(length(t1), 1)
  t6                  <- cbind(t1, t5)
  simDigits           <- as.data.frame(t6)
  colnames(simDigits) <- c("digit", "s1", "s2", "s3", "s4", "s5", "s6", "s7")
  smpl[[n]] <- simDigits
}


#running the scenarios for the different sample sizes based on the list of dfs smpl
for (x in c(1:3))
{
td <- smpl[[x]]
print(nrow(td))

#logit
fitted.logit  <- matrix(rep(NA,nrow(td)*numDigits), nrow=nrow(td) )
digits        <- td$digit   # save for later use (and re-use)
td$digit      <- NULL       # strip this out for convenience in model spec
# fit: get predictions from individual models
for ( i in 1:length(classes) ) {
  d                 <- classes[i]
  td$y              <- digits*0  # initialize
  td$y[digits == d] <- 1         # indicator for -each- digit
  m                 <- glm(y ~ ., data=td, family=binomial())
  fitted.logit[,i]  <- m$fitted.values
}
# classify
index       <- apply(fitted.logit, byRows, which.max) # location
class.logit <- classes[index]
scale.logit <- apply(fitted.logit, byRows, sum)
p.logit     <- apply(fitted.logit, byRows, max)/scale.logit
risk.logit  <- 1-p.logit                              # Bayes Risk
hits.logit <- table(class.logit, digits)
pc.logit   <- sum(diag(hits.logit))/sum(hits.logit) # percent correct             
# cleanup
td$y        <- NULL


# 10x tree: each v. other
fitted.tree10 <- matrix(rep(NA,nrow(td)*numDigits), nrow=nrow(td) )
# fit: get predictions from individual models
for ( i in 1:length(classes) ) {
  d                  <- classes[i]
  td$y               <- digits*0  # initialize
  td$y[digits == d]  <- 1         # indicator for -each- digit
  m                  <- ctree(y ~ ., data=td)
  fitted.tree10[,i]  <- predict(m)
}
# cleanup
td$y         <- NULL
# classify
index        <- apply(fitted.tree10, byRows, which.max) # location
class.tree10 <- classes[index]
scale.tree10 <- apply(fitted.tree10, byRows, sum)
p.tree10     <- apply(fitted.tree10, byRows, max)/scale.tree10
risk.tree10  <- 1-p.tree10                               # Bayes Risk
hits.tree10 <- table(class.tree10, digits)
pc.tree10   <- sum(diag(hits.tree10))/sum(hits.tree10) # percent correct             


# 1 tree: multinomial
td$fDigits    <- as.factor(digits)  # triggers classification
m             <- ctree(fDigits~.,data=td)
fitted.tree1  <- predict(m)         
p.tree1       <- predict(m,type="prob")  # individual class probabilities 
# find min Bayes Risk classification based on probabilities
mbr.tree1     <- apply(p.tree1, byRows, which.max)
risk.tree1    <- 1-apply(p.tree1, byRows, max)
hits.tree1   <- table(fitted.tree1, digits)
pc.tree1     <- sum(diag(hits.tree1))/sum(hits.tree1) # percent correct             


#hw4 deliverables
holdPClogit <- c(holdPClogit, pc.logit)
holdMedBRlogit <- c(holdMedBRlogit, median(risk.logit))
hold75BRlogit <- c(hold75BRlogit, as.numeric(quantile(risk.logit, .75)))
holdPCtree10 <- c(holdPCtree10, pc.tree10)
holdMedBRtree10 <- c(holdMedBRtree10, median(risk.tree10))
hold75BRtree10 <- c(hold75BRtree10, as.numeric(quantile(risk.tree10, .75)))
holdPCtree1 <- c(holdPCtree1, pc.tree1)
holdMedBRtree1 <- c(holdMedBRtree1, median(risk.tree1))
hold75BRtree1 <- c(hold75BRtree1, as.numeric(quantile(risk.tree1, .75)))
}

source('validate.txt', echo=T)
