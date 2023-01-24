###File paths have been removed and file names changed

##load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(pROC)
library(Hmisc)
library(data.table)

##Loop over 3 datasets for making table 1
##Load in 3 datasets into a list format
dataset_names <- c("pheno.UC.merge_norels", "pheno.IBD.merge_norels", "pheno.CD.merge_norels")
data_list <- lapply(dataset_names, fread)

for(i in 1:length(data_list)) {

dat <- data_list[[i]]

#Mean and SD of PRS across ASD groups
dat %>%
    group_by(ASDPOP) %>%
    summarize(
    n(),
    mean.PRS=mean(std.pt0.001, na.rm=TRUE),
    sd.PRS= sd(std.pt0.001, na.rm=TRUE)
)

#t test for PRS across ASD groups
t.test(dat$std.pt0.001 ~ dat$ASDPOP)

#n and frequency of GI symptoms across ASD groups

dat %>%
    group_by(ASDPOP) %>%
    summarize(n= n(),
    n.ALL_GI= sum(as.numeric(as.character(ALL_GI)), na.rm=TRUE),
    p.ALL_GI= sum(as.numeric(as.character(ALL_GI)), na.rm=TRUE)/n(),

    n.VOM= sum(as.numeric(as.character(VOM)), na.rm=TRUE),
    p.VOM= sum(as.numeric(as.character(VOM)), na.rm=TRUE)/n(),

    n.LSCP= sum(as.numeric(as.character(LSCP)), na.rm=TRUE),
    p.LSCP= sum(as.numeric(as.character(LSCP)), na.rm=TRUE)/n(),

    n.DEFEC= sum(as.numeric(as.character(DEFEC)), na.rm=TRUE),
    p.DEFEC= sum(as.numeric(as.character(DEFEC)), na.rm=TRUE)/n(),

    n.GAS= sum(as.numeric(as.character(GAS)), na.rm=TRUE),
    p.GAS= sum(as.numeric(as.character(GAS)), na.rm=TRUE)/n(),

    n.MEAL= sum(as.numeric(as.character(MEAL)), na.rm=TRUE),
    p.MEAL= sum(as.numeric(as.character(MEAL)), na.rm=TRUE)/n(),

    n.PAIN= sum(as.numeric(as.character(PAIN)), na.rm=TRUE),
    p.PAIN= sum(as.numeric(as.character(PAIN)), na.rm=TRUE)/n(),

    n.LOOSE= sum(as.numeric(as.character(LOOSE)), na.rm=TRUE),
    p.LOOSE= sum(as.numeric(as.character(LOOSE)), na.rm=TRUE)/n(),

    n.DIAR= sum(as.numeric(as.character(DIAR)), na.rm=TRUE),
    p.DIAR= sum(as.numeric(as.character(DIAR)), na.rm=TRUE)/n(),

    n.CONST= sum(as.numeric(as.character(CONST)), na.rm=TRUE),
    p.CONST= sum(as.numeric(as.character(CONST)), na.rm=TRUE)/n()

)

dat %>%
group_by(ASDPOP) %>%
summarize(n= n(),
n.G.EUR= sum(as.numeric(as.character(G.EUR)), na.rm=TRUE),
p.G.EUR= sum(as.numeric(as.character(G.EUR)), na.rm=TRUE)/n(),
n.GND= sum(as.numeric(as.character(PV_GND)), na.rm=TRUE),
p.GND= sum(as.numeric(as.character(PV_GND)), na.rm=TRUE)/n()
)

#Diference in proportions for GI symptoms
prop.test(x = c(187, 81), n = c(564, 715))  #ALL_GI
prop.test(x = c(33, 6), n = c(187, 81))  #VOM
prop.test(x = c(50, 12), n = c(187, 81))  #DIAR
prop.test(x = c(76, 24), n = c(187, 81))  #LOOSE
prop.test(x = c(132, 54), n = c(187, 81))  #CONST
prop.test(x = c(71, 17), n = c(187, 81))  #LSCP
prop.test(x = c(30, 13), n = c(187, 81))  #MEAL
prop.test(x = c(42, 29), n = c(187, 81))  #DEFEC
prop.test(x = c(72, 25), n = c(187, 81)) #PAIN
prop.test(x = c(86, 25), n = c(187, 81))  #GAS

#Difference in proportions for demographics
prop.test(x = c(303, 501), n = c(564, 715)) #G.EUR
prop.test(x = c(453, 391), n = c(564, 715))  #PV_GND

}

