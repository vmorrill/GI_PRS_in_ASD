###File paths have been removed and file names changed

##load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(pROC)
library(Hmisc)
library(data.table)

##Load in 3 datasets into a list format
dataset_names <- c("pheno.UC.merge", "pheno.IBD.merge", "pheno.CD.merge")
data_list <- lapply(dataset_names, fread)

##Loop over 3 datasets for data manipulation

for(i in 1:length(data_list)) {

    dat <- data_list[[i]]

    ##Remove relatives
    remove <- fread("toRemove.csv")
    remove <- remove %>% rename(PERSONID=ID, FamilyID=Family_ID)
    dim(dat) #1505
    dim(remove) #97
    removed <- inner_join(dat,remove) #35 removed for relatedness issue, 2 for sex mismatch

    ##Recode variables
    
    #gender
    dat$PV_GND[dat$PV_GND==2] <- 0 #specify female as 0
    dat$PV_GND[dat$PV_GND==3] <- 1 #specify male as 1
    dat$PV_GND <- as.factor(dat$PV_GND)

    #ALL GI symptoms
    dat$ALL_GI[dat$ALL_GI==2] <- 0  #0 is no GI symptoms
    dat$ALL_GI[dat$ALL_GI==88] <- "NA"
    dat$ALL_GI <- as.factor(as.numeric(dat$ALL_GI))

    #Specific GI symptoms
    dat$VOM <- recode_factor(dat$VOM, '0'='0', '1'='1', .default = NA_character_)
    dat$LSCP <- recode_factor(dat$LSCP, '0'='0', '1'='1', .default = NA_character_)
    dat$DEFEC <- recode_factor(dat$DEFEC, '0'='0', '1'='1', .default = NA_character_)
    dat$GAS <- recode_factor(dat$GAS, '0'='0', '1'='1', .default = NA_character_)
    dat$MEAL <- recode_factor(dat$MEAL, '0'='0', '1'='1', .default = NA_character_)
    dat$PAIN <- recode_factor(dat$PAIN, '0'='0', '1'='1', .default = NA_character_)
    dat$LOOSE <- recode_factor(dat$LOOSE, '0'='0', '1'='1', .default = NA_character_)
    dat$DIAR <- recode_factor(dat$DIAR, '0'='0', '1'='1', .default = NA_character_)
    dat$CONST <- recode_factor(dat$CONST, '0'='0', '1'='1', .default = NA_character_)

    # Recode possible cases as cases
    dat$ASD[dat$ASD=="POSSIBLE CASE"] <- "CASE"

    ##Determine genetic european ancestry by PC1 vs PC2
    #Determined cutoff is PC1 <= 0 & PC2 >= -0.01 for european ancestry
    # EUR: 1=european ancestry, 0=not european ancestry
    dat$G.EUR <- NULL
    dat$G.EUR[dat$PC1 <= 0 & dat$PC2 >= -0.01] <- 1 # EUR: 1=european ancestry, 0=not european ancestry
    dat$G.EUR[dat$PC1 > 0 & dat$PC2 >= -0.01] <- 0
    dat$G.EUR[dat$PC1 <= 0 & dat$PC2 < -0.01] <- 0
    dat$G.EUR[dat$PC1 > 0 & dat$PC2 < -0.01] <- 0
    dat$G.EUR <- as.factor(dat$G.EUR)

    ##Create new variable, ASDPOP: ASD=1, POP=0, everything else is NA
    dat$ASDPOP <- recode_factor(dat$ASD, "CASE"="1", "POP"="0",.default = NA_character_)
    dat$ASDPOP <- as.numeric(as.character(dat$ASDPOP))

    ##Restandardize PRS in new smaller sample: center to the mean and rescale so 1 unit is 1 SD
    prs_mean <- mean(dat$raw.pt0.001)
    prs_sd <- sd(dat$raw.pt0.001)
    dat <- dat %>% mutate(std.pt0.001= (raw.pt0.001 - prs_mean)/ prs_sd)

    write.table(dat, paste0(dataset_names[[i]], "_norels")

}



