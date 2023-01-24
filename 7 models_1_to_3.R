###File paths have been removed and file names changed

##load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(pROC)
library(Hmisc)
library(data.table)


##Loop over 3 datasets to run models 1 through 3
##Load in 3 datasets into a list format
dataset_names <- c("pheno.UC.merge_norels", "pheno.IBD.merge_norels", "pheno.CD.merge_norels")
data_list <- lapply(dataset_names, fread)

for(i in 1:length(data_list)) {

dat <- data_list[[i]]

a <- glm(ALL_GI ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(ALL_GI ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(ALL_GI ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

a <- glm(LSCP ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(LSCP ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(LSCP ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

a <- glm(DIAR ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(DIAR ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(DIAR ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

a <- glm(DEFEC ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(DEFEC ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(DEFEC ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

a <- glm(GAS ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(GAS ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(GAS ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

a <- glm(MEAL ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(MEAL ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(MEAL ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

a <- glm(PAIN ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(PAIN ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(PAIN ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

a <- glm(LOOSE ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(LOOSE ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(LOOSE ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

a <- glm(CONST ~ ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
b <- glm(CONST ~ ASDPOP + std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
c <- glm(CONST ~ ASDPOP + std.pt0.001 + std.pt0.001:ASDPOP + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")

exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))
summary(c)

}


