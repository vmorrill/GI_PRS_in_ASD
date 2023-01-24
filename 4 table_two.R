###File paths have been removed and file names changed

##load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(pROC)
library(Hmisc)
library(data.table)

##Loop over 3 datasets to make table 2
##Load in 3 datasets into a list format
dataset_names <- c("pheno.UC.merge_norels", "pheno.IBD.merge_norels", "pheno.CD.merge_norels")
data_list <- lapply(dataset_names, fread)

for(i in 1:length(data_list)) {

dat <- data_list[[i]]

#european ancestry subset
EUR <- dat[dat$G.EUR==1,]
#non european ancestry subset
nEUR <- dat[dat$G.EUR==0,]


a <- glm(ASDPOP ~ std.pt0.001, data=dat, family = "binomial")
b <- glm(ASDPOP ~ std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat, family = "binomial")
exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))

a <- glm(ASDPOP ~ std.pt0.001, data=EUR, family = "binomial")
b <- glm(ASDPOP ~ std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=EUR, family = "binomial")
exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))

a <- glm(ASDPOP ~ std.pt0.001, data=nEUR, family = "binomial")
b <- glm(ASDPOP ~ std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=nEUR, family = "binomial")
exp(a$coefficients)
exp(confint(a))
exp(b$coefficients)
exp(confint(b))

}


