#setwd('c:/data/BUAN6357/HW_6'); source('prep.txt', echo=T)
setwd('documents/ba-masters/spring19/advanced-bar/hw/hw6/')

library(tidyverse)
library(data.table)
library(broom)

raw <- read.table('Concrete_Data_wj.csv',header = T,sep = ',')

mod <- lm(data=raw, strength~.)
m1 <- tidy(mod)


#source('validate.txt', echo=T)