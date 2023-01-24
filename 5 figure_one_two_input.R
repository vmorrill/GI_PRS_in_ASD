###File paths have been removed and file names changed

##load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(pROC)
library(Hmisc)
library(data.table)

##Loop over 3 datasets
##Load in 3 datasets into a list format
dataset_names <- c("pheno.UC.merge_norels", "pheno.IBD.merge_norels", "pheno.CD.merge_norels")
data_list <- lapply(dataset_names, fread)

##Figure 1: CASES only##
for(i in 1:length(data_list)) {

dat <- data_list[[i]]
dat_case <- dat %>% filter(ASD=="CASE")

#Create data frame
GI_names <- names(dat[4:13])
effect <- rep(NA, 10)
upper <- rep(NA, 10)
lower <- rep(NA, 10)
output_case <- data.frame(GI_names, effect, upper, lower)

#Run a logistic model for each GI symptom 1 at a time

for (i in 4:13) {
   
    logistic <- glm(dat_case[,i] ~ std.pt0.001 + PC1 + PC2 + PC3 +PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=dat_case, family = "binomial")
    paste=i-3
    output_case$effect[paste] = exp(coef(summary(logistic))[2,1]) #extracts PRS covariate and expoentiates
    output_case$upper[paste] = exp(confint(logistic)[2,2]) #extracts upper bound of confidence interval and expoentiates
    output_case$lower[paste] = exp(confint(logistic)[2,1]) #extracts lower bound of confidence interval and expoentiates
      
}

#View output
output_case

}

##Figure 2: CONTROLS only##
for(i in 1:length(data_list)) {

dat <- data_list[[i]]
dat_case <- dat %>% filter(ASD=="CONTROL")

#Create data frame
GI_names <- names(dat[4:13])
effect <- rep(NA, 10)
upper <- rep(NA, 10)
lower <- rep(NA, 10)
output_control<- data.frame(GI_names, effect, upper, lower)

#Run a logistic model for each GI symptom 1 at a time

for (i in 4:13) {
    logistic <- glm(dat_control[,i] ~ std.pt0.001 + PC1 + PC2 + PC3 +PC4 , data=dat_control, family = "binomial")
    paste=i-3
    output_control$effect[paste] = exp(coef(summary(logistic))[2,1]) #extracts PRS covariate and expoentiates
    output_control$upper[paste] = exp(confint(logistic)[2,2]) #extracts upper bound of confidence interval and expoentiates
    output_control$lower[paste] = exp(confint(logistic)[2,1]) #extracts lower bound of confidence interval and expoentiates
}

#View output
output_control

}



