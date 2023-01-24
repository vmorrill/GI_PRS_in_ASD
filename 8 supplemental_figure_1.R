###File paths have been removed and file names changed

##load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(pROC)
library(Hmisc)
library(data.table)

#IBD
dat <- fread("pheno.IBD.merge_norels")

dat %>%
    filter(ASDPOP==1 | ASDPOP==0) %>%
    ggplot(aes(x=std.pt0.001, fill=ASDPOP)) +
    geom_density(alpha=0.4) +
    xlim(-4,4) +
    labs(x="Standardized IBD PRS" ,title="Supplementary Figure 1: IBD") +
    scale_fill_discrete(name="ASD Status", labels= c("Case", "Control")) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal()

#CD
dat <- fread("pheno.CD.merge_norels")

dat %>%
    filter(ASDPOP==1 | ASDPOP==0) %>%
    ggplot(aes(x=std.pt0.001, fill=ASDPOP)) +
    geom_density(alpha=0.4) +
    xlim(-4,4) +
    labs(x="Standardized CD PRS" ,title="Supplementary Figure 1: CD") +
    scale_fill_discrete(name="ASD Status", labels= c("Case", "Control")) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal()

#UC
dat <- fread("pheno.UC.merge_norels")

dat %>%
    filter(ASDPOP==1 | ASDPOP==0) %>%
    ggplot(aes(x=std.pt0.001, fill=ASDPOP)) +
    geom_density(alpha=0.4) +
    xlim(-4,4) +
    labs(x="Standardized UC PRS" ,title="Supplementary Figure 1: UC") +
    scale_fill_discrete(name="ASD Status", labels= c("Case", "Control")) +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal()

