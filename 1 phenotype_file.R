###File paths have been removed and file names changed

#load libraries
library(sas7bdat)
library(dplyr)

##Load in phenotype data

#ASD final classification status
FC <- read.sas7bdat("finalclassification.sas7bdat", debug=FALSE) 
FC <- FC %>% select(FamilyID, DR_FC) %>% rename(ASD=DR_FC)

#GI symptoms
GI <- read.sas7bdat("gi_symptoms.sas7bdat", debug=FALSE) 
GI <- GI %>% select(FamilyID, GI_14_BOWEL, GI_15_LOOSE, GI_15_DIAR, 
	GI_15_VOM, GI_15_MEAL, GI_15_CONST, GI_15_LSCP, GI_15_DEFEC, GI_15_GAS,
	GI_15_PAIN, GI_15_OTH) %>% rename(ALL_GI=GI_14_BOWEL, LSCP= GI_15_LSCP, 
	DIAR= GI_15_DIAR, DEFEC= GI_15_DEFEC, VOM=GI_15_VOM, GAS=GI_15_GAS, 
	MEAL=GI_15_MEAL, PAIN=GI_15_PAIN, LOOSE=GI_15_LOOSE, CONST=GI_15_CONST, OTH=GI_15_OTH)

#Race and Gender
race <- read.sas7bdat("person.sas7bdat", debug=FALSE) 
race <- race %>% select(FamilyID, PERSONID, PV_GND, PV_RACEWH)

#Eigenvectors
E <- read.table("eigenvectors.txt", header=TRUE)
E <- E %>% select(-FID) %>% rename( SubjectID=IID)

##Data Manipulation

#determine if there are duplicate IDs
length(unique(GI$FamilyID)) #One individual shows up 8 times.... eliminate

#Eliminate duplicate ID=1 in GI dataset
GI_nodup <- GI %>% filter(FamilyID != 1)

#recode 0 to 1 in All_GI because they both mean true
table(GI_nodup$ALL_GI)
GI_nodup$ALL_GI<- as.character(GI_nodup$ALL_GI)
GI_nodup$ALL_GI[GI_nodup$ALL_GI=="0"] <- "1"

##Merge

#merge GI_nodup with FC by FamilyID

pheno <- merge(GI_nodup, FC, by= "FamilyID", all.x= TRUE, all.y= TRUE)

#merge pheno and E by FamilyID

pheno2 <- merge(pheno, E, by.x= "FamilyID", by.y= "SubjectID", all.x= TRUE, all.y= TRUE)

#write to csv
write.csv(pheno2, file="phenotype_merge")
write.csv(race, file="race_derived")

#########################################

##Merge phenotype file with CD PRS file

#read in data
pheno <- read.csv("phenotype_merge")
CD <- read.csv("SEED.CD.polyscore.csv")

#rename columns
colnames(CD)[1] <- "FamilyID"
colnames(CD)[2] <- "PERSONID"

#merge Crohn's disease PRS with phenotype dataset
pheno.CD.merge <- merge(pheno, CD, by= "FamilyID", all.x=FALSE, all.y=TRUE)
pheno.CD.merge$pheno <- NULL

#write to csv
write.csv(pheno.CD.merge, file="pheno.CD.merge")

##Merge phenotype file with UC PRS file

#read in data
pheno <- read.csv("phenotype_merge")
CD <- read.csv("SEED.UC.polyscore.csv")

#rename columns
colnames(CD)[1] <- "FamilyID"
colnames(CD)[2] <- "PERSONID"

#merge Crohn's disease PRS with phenotype dataset
pheno.UC.merge <- merge(pheno, CD, by= "FamilyID", all.x=FALSE, all.y=TRUE)
pheno.YC.merge$pheno <- NULL

#write to csv
write.csv(pheno.UC.merge, file="pheno.UC.merge")

##Merge phenotype file with IBD PRS file

#read in data
pheno <- read.csv("phenotype_merge")
CD <- read.csv("SEED.IBD.polyscore.csv")

#rename columns
colnames(CD)[1] <- "FamilyID"
colnames(CD)[2] <- "PERSONID"

#merge Crohn's disease PRS with phenotype dataset
pheno.IBD.merge <- merge(pheno, CD, by= "FamilyID", all.x=FALSE, all.y=TRUE)
pheno.IBD.merge$pheno <- NULL

#write to csv
write.csv(pheno.IBD.merge, file="pheno.IBD.merge")
