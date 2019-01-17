######################################
## Author:   Jason Parker
## Date:     2018-08-27
## Title:    lec2.R
## Purpose:  Programming intro with statistics review
##             - Control structures
##             - Statistics concepts/review
##             - Synthetic data + Monte Carlo
##             - Endogeneity
##             - Time series intro
######################################

## Drop everything
rm(list=ls(all=TRUE))
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(broom)

## Set a default sample size
n <- 200

######################################
## Synthetic data
######################################

set.seed(Sys.time())
set.seed(75080)

## Uniform distribution
runif(1)
runif(10)
z <- runif(n)
z <- runif(200)
z
mean(z)
min(z)
max(z)
var(z)
?runif

z <- runif(10,min=1,max=3)
z
c(min(z),max(z))
var(runif(10*n))
1/12  # Variance of uniform distribution from wikipedia


## Normal distribution
?rnorm
rnorm(10)

u <- runif(1)
c(u,qnorm(u))

u <- runif(100)
c(u,qnorm(u))
table(u)

?rnorm
rnorm(10,mean=5,sd=0.5)
x <- rnorm(1000*n,mean=5,sd=0.5)
c(mean(x),sqrt(var(x)))





######################################
## Control Structures
######################################

## User-defined function (runs as block)
std <- function(input_std){
  varx <- var(input_std)
  stdx <- sqrt(varx)
  return(stdx)
}
c(mean(x),std(x))

## If/else statements
p <- 0#1
if (p == 1) {
  x <- rnorm(n)
} else {
  x <- runif(n)
}
mean(x)

## Loops
x <- runif(n)
meanx <- 0
for(i in 1:n) {
  meanx <- meanx + x[i]
}
meanx <- meanx/n
meanx

## Loop time delay demonstration
n = 100000000
x <- runif(n)
t0 <- Sys.time()
meanx <- 0
for(i in 1:n) {
  meanx <- meanx + x[i]
}
meanx <- meanx/n
Sys.time() - t0
t0 <- Sys.time()
mx <- mean(x)
Sys.time() - t0
c(meanx,mx)  # same answer, but takes an order of magnitude longer


mx <- 0
for(i in 1:1000){
  x <- rnorm(200,mean=5,sd=0.5)
  mx[i] <- mean(x)
}
mx

######################################
## Monte Carlo
######################################


## Monte Carlo simulation x1
simn  <- 1000
n     <- 200
mx    <- rep(0,simn)
for (isim in 1:simn) {
  x         <- rnorm(n,mean=5,sd=0.5)
  mx[isim]  <- mean(x)
}
mx
hist(mx) # mean around 5, small stdev
mean(mx) 
sd(mx) # calculate stdev
var(mx)

## Monte Carlo simulation x5
simn  <- 1000
outp  <- matrix(data=0,nrow=5,ncol=4)
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  mx          <- rep(0,simn)
  for (isim in 1:simn) {
    x         <- rnorm(n,mean=5,sd=0.5)
    mx[isim]  <- mean(x)
  }
  outp[ns,]   <- c(n,mean(mx),sd(mx),var(mx))
  #plot histogram (distribution of sample means)
  #  set limits around 4.5 to 5.5 for consistent estimation
  hist(mx,xlim=c(4.5,5.5))
  #pause
  invisible(readline(prompt="Press [enter] to continue"))
}
outp ## Note that the standard deviation is converging to 0 as n goes to infinity
##########################################################################################################################################
#sample mean converges around the population's mean as the sample size increases
##########################################################################################################################################
##############################################
## Distribution of the sample mean for normal data
##############################################

outp  <- matrix(data=0,nrow=5*10*11,ncol=5)
rowcount <- 1
for (ns in 1:5) {  #change n
  for (ms in -5:5) { #change popn mean
    for (sds in 1:10) { #change popn sd
      pmean <- ms #popn mean =  -5,-4,...,5
      psd <- sds/10 #popn sd = 0.1,0.2,...,1.0
      if (ns == 1) {n <- 10}
      if (ns == 2) {n <- 25}
      if (ns == 3) {n <- 50}
      if (ns == 4) {n <- 100}
      if (ns == 5) {n <- 250}
      mx              <- rep(0,simn)
      for (isim in 1:simn) {
        x             <- rnorm(n,mean=pmean,sd=psd)
        mx[isim]      <- mean(x)
      }
      #save information about the distribution of the sample mean
      outp[rowcount,] <- c(n,pmean,psd,mean(mx),var(mx))
      rowcount        <- rowcount + 1
    }
  }
}
head(outp)
nrow(outp)
context     <- data.frame(outp)
colnames(context) <- c('sample','pmean','psd','m_xbar','var_xbar')
head(context)

model1 <- lm(m_xbar~sample+pmean+psd, data=context)
summary(model1)
model1 <- lm(m_xbar~pmean, data=context)
summary(model1) #R-squared close to 1 means the model is a good fit
model2 <- lm(var_xbar~     sample+pmean+psd,    data=context)
summary(model2) 
model2 <- lm(log(var_xbar)~log(sample)+log(psd),data=context)
summary(model2) #R-squared close to 1 means the model is a good fit

#m_xbar        = pmean
#log(var_xbar) = -1*log(n) + 2*log(psd)
#log(var_xbar) = log(pop_var/n)
#var_xbar      = pop_var/n



##################
## Distribution of sample mean with uniform data
#################


outp  <- matrix(data=0,nrow=5*10*11,ncol=5)
rowcount <- 1
for (ns in 1:5) {  #change n
  for (ms in -5:5) { #change popn mean
    for (sds in 1:10) { #change popn sd
      pmean <- ms #popn mean =  -5,-4,...,5
      psd <- sds/10 #popn sd = 0.1,0.2,...,1.0
      amin <- pmean - sqrt(3)*psd
      bmax <- pmean + sqrt(3)*psd
      if (ns == 1) {n <- 10}
      if (ns == 2) {n <- 25}
      if (ns == 3) {n <- 50}
      if (ns == 4) {n <- 100}
      if (ns == 5) {n <- 250}
      mx              <- rep(0,simn)
      for (isim in 1:simn) {
        x             <- runif(n,min=amin,max=bmax)
        mx[isim]      <- mean(x)
      }
      #save information about the distribution of the sample mean
      outp[rowcount,] <- c(n,pmean,psd,mean(mx),var(mx))
      rowcount        <- rowcount + 1
    }
  }
}
head(outp)
nrow(outp)
context     <- data.frame(outp)
colnames(context) <- c('sample','pmean','psd','m_xbar','var_xbar')
head(context)

model1 <- lm(m_xbar~       pmean,               data=context)
summary(model1) #R-squared close to 1 means the model is a good fit
model2 <- lm(log(var_xbar)~log(sample)+log(psd),data=context)
model2 <- lm(log(var_xbar)~I(2*log(sample))+log(psd),data=context)
summary(model2) #R-squared close to 1 means the model is a good fit

#m_xbar        = pmean
#log(var_xbar) = -1*log(n) + 2*log(psd)
#log(var_xbar) = log(pop_var/n)
#var_xbar      = pop_var/n

##############################################
## Central limit theorem
##############################################

simn <- 100000

simn = 2000
plot(density(rnorm(simn)),xlim=c(-2.5,2.5))

for (ns in 1:5) {
  if (ns == 1) {n <- 1}
  if (ns == 2) {n <- 2}
  if (ns == 3) {n <- 3}
  if (ns == 4) {n <- 5}
  if (ns == 5) {n <- 10}
  mx_spec         <- rep(0,simn)
  for (isim in 1:simn) {
    x             <- runif(n,min=-1,max=1)
    #mx_spec is the standardized sample mean (mean=0,sd=1)
    #i.e., just the mean times some stuff because the popn mean
    #  is zero
    mx_spec[isim] <- mean(x)*sqrt(n)*sqrt(3) 
  }
  plot(density(mx_spec),xlim=c(-2.5,2.5))
  invisible(readline(prompt="Press [enter] to continue"))
} # Sample mean follows a normal distribution as n => infty

##############################################
## Hypothesis testing
##############################################

simn <- 1000
outp <- matrix(data=0,nrow=5,ncol=2)
mu   <- 0  #Ho:mu==0; Ha:mu!=0
mu   <- 0.5 #Ho:mu==0.5; Ha:mu!=0.5
for (ns in 1:5) {
  if (ns == 1) {n <- 3}
  if (ns == 2) {n <- 5}
  if (ns == 3) {n <- 10}
  if (ns == 4) {n <- 25}
  if (ns == 5) {n <- 50}
  rejections      <- 0
  for (isim in 1:simn) {
    x             <- runif(n,min=-1,max=1)
    mx            <- mean(x)
    sterr         <- sqrt(var(x)/n) #estimate for st. dev. of mx
    #t_stat is just mx_spec from above
    t_stat        <- (mx-mu)/sterr
    #use the normal distribution b/c it works as n->infty
    if(abs(t_stat)>1.96) {rejections <- rejections + 1}
  }
  outp[ns,]       <- c(n,rejections/simn)
}
outp 
# When null is true, rejection rate => 0.05 as5n => infty
#   We call this rejection rate the "size" of the test
# When null is false, rejection rate => 1 as n => infty
#   We call this rejection rate the "power" of the test

simn  <- 1000 #repetitions

##############################################
## OLS without endogeneity
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2


outp <- matrix(data=0,simn*5,3)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients
    model         <- tidy(lm(y~x))
    outp[rowcount,2]   <- model[2,2]
    outp[rowcount,3]   <- (model[2,2]-2)/model[2,3]
    outp[rowcount,1]   <- n
    rowcount <- rowcount + 1
  }
}
outp <- data.frame(outp)
colnames(outp) <- c('sample','no_endo','no_endo_t')
outp$sample <- as.factor(outp$sample)

plot_no_endo <- ggplot(outp,aes(no_endo,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_no_endo

plot_no_endo_t <- ggplot(outp,aes(no_endo_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_no_endo_t

##############################################
## OLS with an omitted variable (uncorrelated with x)
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
# new vars
b2    <- 1
zmean <- 3
zsd   <- 1.5
outp$omit_uncorr <- rep(0,simn*5)
outp$omit_uncorr_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    z             <- rnorm(n,mean=zmean,sd=zsd) #z independent of x so no problem
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + b2*z + e
    
    # estimate the ols coefficients WITHOUT INCLUDING z
    model         <- tidy(lm(y~x))
    # model         <- tidy(lm(y~x+z)) 
    outp$omit_uncorr[rowcount]   <- model[2,2]
    outp$omit_uncorr_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_omit_uncorr <- ggplot(outp,aes(omit_uncorr,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_omit_uncorr
plot_omit_uncorr_t <- ggplot(outp,aes(omit_uncorr_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_omit_uncorr_t

##############################################
## OLS with an omitted variable (CORRELATED with x)
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
b2    <- 1
sig   <- 1.2
xmean <- 4
xsd   <- 2
zmean <- 3
zsd   <- 1.5
xzsd  <- 1.25
outp$omit_corr <- rep(0,simn*5)
outp$omit_corr_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    w             <- rnorm(n,mean=0,sd=xzsd) #covariance between x and z
    x             <-  w + rnorm(n,mean=xmean,sd=sqrt(xsd*xsd-xzsd))
    z             <-  w + rnorm(n,mean=zmean,sd=sqrt(zsd*zsd-xzsd))
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + b2*z + e
    
    # estimate the ols coefficients WITHOUT INCLUDING z
    model         <- tidy(lm(y~x))
    # model         <- tidy(lm(y~x+z)) 
    outp$omit_corr[rowcount]   <- model[2,2]
    outp$omit_corr_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_omit_corr <- ggplot(outp,aes(omit_corr,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_omit_corr
plot_omit_corr_t <- ggplot(outp,aes(omit_corr_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_omit_corr_t

##############################################
## OLS with reverse regression
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
outp$reverse <- rep(0,simn*5)
outp$reverse_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients BACKWARDS
    model         <- tidy(lm(x~y))
    outp$reverse[rowcount]   <- 1/model[2,2]
    outp$reverse_t[rowcount] <- (model[2,2]-0.5)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_reverse <- ggplot(outp,aes(reverse,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_reverse
plot_reverse_t <- ggplot(outp,aes(reverse_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_reverse_t

##############################################
## OLS with measurement error
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
# new vars
exsd  <- 1
eysd  <- 2
outp$meas_error <- rep(0,simn*5)
outp$meas_error_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + e
    
    # we don't observe x and y, we only have data on xstar and ystar
    xstar         <- x + rnorm(n,mean=0,sd=exsd)
    ystar         <- y + rnorm(n,mean=0,sd=eysd)
    
    # estimate the ols coefficients using the measured (with error) x and y
    model         <- tidy(lm(ystar~xstar))
    outp$meas_error[rowcount]   <- model[2,2]
    outp$meas_error_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_meas_error <- ggplot(outp,aes(meas_error,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_meas_error
plot_meas_error_t <- ggplot(outp,aes(meas_error_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_meas_error_t


##############################################
## Measurement error in bwght
##############################################

outp$bwght <- rep(0,simn*5)
outp$bwght_t <- rep(0,simn*5)
outp$ssize <- rep(0,simn*5)
rowcount <- 1

for (ns in 1:5) {
  if (ns == 1) {n <- 100}
  if (ns == 2) {n <- 250}
  if (ns == 3) {n <- 500}
  if (ns == 4) {n <- 1000}
  if (ns == 5) {n <- 2500}
  for (isim in 1:simn) {
    # create random data
    smokes        <- as.numeric(runif(n)>0.9)
    cigs          <- smokes*runif(n,min=0,max=9)^2
    bwght         <- cigs*-0.3867889+rnorm(n,mean=120,sd=20)
    cigs_star     <- cigs*runif(n,min=0.5,max=1)
    
    # estimate the ols coefficients using the measured (with error) x and y
    model         <- tidy(lm(bwght~cigs_star))
    outp$bwght[rowcount]   <- model[2,2]
    outp$bwght_t[rowcount] <- (model[2,2])/model[2,3]
    outp$ssize[rowcount]   <- n
    rowcount = rowcount + 1
  }
}
outp$ssize <- as.factor(outp$ssize)


plot_bwght <- ggplot(outp,aes(bwght,fill = ssize)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(-1,0)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_bwght
plot_bwght_t <- ggplot(outp,aes(bwght_t,fill = ssize)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_bwght_t
mean(outp$bwght_t)
var(outp$bwght_t)


mean(outp$bwght)
factor <- mean(outp$bwght)
factor <- (factor+0.5)/-0.5
factor <- factor + 1
-0.5/factor


##############################################
## Heteroskedasticity
##############################################

simn <- 200

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2

outp$hetero <- rep(0,simn*5)
outp$hetero_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig*(abs(x-xmean)/xsd))
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients
    model         <- tidy(lm(y~x))
    outp$hetero[rowcount]   <- model[2,2]
    outp$hetero_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount <- rowcount + 1
  }
}

plot_hetero <- ggplot(outp,aes(hetero,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_hetero

plot_hetero_t <- ggplot(outp,aes(hetero_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_hetero_t
mean(abs(outp$hetero_t)>=1.96)
##############################################
## OLS with Time series
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
## New parameters
rho   <- 0.5
rhox  <- 0.5

outp$ts1 <- rep(0,simn*5)
outp$ts1_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- arima.sim(n = n, list(order = c(1,0,0), ar = c(rhox), sd = xsd)) + xmean
    e             <- arima.sim(n = n, list(order = c(1,0,0), ar = c(rho), sd = sig))
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients using the measured (with error) x and y
    model         <- tidy(lm(y~x))
    outp$ts1[rowcount]   <- model[2,2]
    outp$ts1_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_ts1 <- ggplot(outp,aes(ts1,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_ts1
plot_ts1_t <- ggplot(outp,aes(ts1_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_ts1_t

##############################################
## OLS with Unit root
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
## New parameters
rho   <- 0.5
rhox  <- 0.5

outp$ts2 <- rep(0,simn*5)
outp$ts2_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- arima.sim(n = n, list(order = c(1,1,0), ar = c(rhox), sd = xsd)) + xmean
    e             <- arima.sim(n = n, list(order = c(1,1,0), ar = c(rho), sd = sig))
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients using the measured (with error) x and y
    model         <- tidy(lm(y~x))
    outp$ts2[rowcount]   <- model[2,2]
    outp$ts2_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_ts2 <- ggplot(outp,aes(ts2,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_ts2
plot_ts2_t <- ggplot(outp,aes(ts2_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_ts2_t

#############
## Plots
#############

plot_no_endo
plot_omit_uncorr
plot_omit_corr
plot_reverse
plot_meas_error

plot_no_endo_t
plot_omit_uncorr_t
plot_omit_corr_t
plot_reverse_t
plot_meas_error_t

plot_bwght
plot_bwght_t

plot_hetero
plot_no_endo_t
plot_hetero_t

plot_no_endo
plot_ts1
plot_ts2

plot_no_endo_t
plot_ts1_t
plot_ts2_t
