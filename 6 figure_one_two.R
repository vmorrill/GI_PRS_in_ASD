###File paths have been removed and file names changed

#load libraries
library(ggplot2)
library(dplyr)

##Figure 1: CASES only##

#Construct data frame for plotting
c <- "Crohn's Disease PRS"
u <- "Ulcerative colitis PRS"
i <- "Inflammatory Bowel Disease PRS"

CASES <- data.frame(

Condition=c(c,c,c,c,c,c,c,c,c,u,u,u,u,u,u,u,u,u,i,i,i,i,i,i,i,i,i),

Symptom=c("All GI Symptoms", "Loose stools", "Diarrhea",
"Abdominal pain with meals", "Constipation", "Loose stools alternating with constipation" ,
"Abdominal pain relieved by defecation", "Gas", "Pain on stooling",

"All GI Symptoms", "Loose stools", "Diarrhea",
"Abdominal pain with meals", "Constipation", "Loose stools alternating with constipation" ,
"Abdominal pain relieved by defecation", "Gas", "Pain on stooling",

"All GI Symptoms", "Loose stools", "Diarrhea",
"Abdominal pain with meals", "Constipation", "Loose stools alternating with constipation" ,
"Abdominal pain relieved by defecation", "Gas", "Pain on stooling"),

Estimate=c(0.87, 1.28, 1.02, 1.08, 0.84, 0.73, 0.97, 0.88, 1.03,
1.11, 0.95, 1.00, 1.11, 0.98, 0.93, 1.32, 0.92, 0.69,
0.92, 1.23, 0.91, 0.89, 0.82, 0.79, 0.75, 0.91, 0.76),

Lower=c(0.72, 0.92, 0.72, 0.67, 0.60, 0.52, 0.63, 0.62, 0.72,
0.90, 0.67, 0.68, 0.63, 0.70, 0.65, 0.81, 0.64, 0.46,
0.73, 0.84, 0.59, 0.49, 0.57, 0.54, 0.44, 0.61, 0.50),

Upper=c(1.05, 1.79, 1.47, 1.75, 1.14, 1.00, 1.48, 1.23, 1.46,
1.38, 1.33, 1.47, 1.97, 1.38, 1.31, 2.18, 1.34, 1.02,
1.16, 1.81, 1.39, 1.62, 1.18, 1.16, 1.24, 1.36, 1.13)

)

#Edit the data formats for plotting
CASES$Estimate <- as.numeric(as.character(CASES$Estimate))
CASES$Upper <- as.numeric(as.character(CASES$Upper))
CASES$Lower <- as.numeric(as.character(CASES$Lower))
CASES$Symptom <- relevel(CASES$Symptom, "All GI Symptoms")

#Subset to results to "All GI Symptoms"
CASES_all_GI <- CASES %>% filter(Symptom=="All GI Symptoms")

#Plot
ggplotdata=CASES_all_GI, aes(x = Symptom,y = Estimate, ymin = Lower, ymax = Upper ))+
    geom_pointrange(aes(col= Symptom), colour="black")+
    geom_hline(aes(fill= Symptom),yintercept =1, linetype=2)+
    xlab('GI Symptom')+
    ylab("Log OR of GI Symptom (95% Confidence Interval)")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper,col= Symptom),width=0.2,cex=1, colour="black")+
    scale_y_log10(limits = c(0.7, 2)) +
    facet_wrap(~Condition,strip.position="top",nrow=1,scales = "free_x") +
    theme(plot.title=element_text(size=24,face="bold"),
    axis.text.x=element_blank(),
    axis.title=element_text(size=12,face="bold"))

#Remove results for "All GI Symptoms"
CASES_not_all_GI <- CASES %>% filter(Symptom!="All GI Symptoms")

#Plot
pallete <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#6a3d9a')

ggplot(data=CASES_not_all_GI, aes(x = Symptom,y = Estimate, ymin = Lower, ymax = Upper ))+
    geom_pointrange(aes(col= Symptom))+
    geom_hline(aes(fill= Symptom),yintercept =1, linetype=2)+
    xlab('GI Symptom')+
    ylab("Log OR of GI Symptom (95% Confidence Interval)")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper,col= Symptom),width=0.2,cex=1)+
    scale_y_log10(limits = c(0.3, 32)) +
    facet_wrap(~Condition,strip.position="top",nrow=1,scales = "free_x") +
    heme(plot.title=element_text(size=24,face="bold"),
    axis.text.x=element_blank(),
    axis.title=element_text(size=12,face="bold")) +
    scale_color_manual(values=pallete)







##Figure 2: CONTROLS only##

#Construct data frame for plotting
c <- "Crohn's Disease PRS"
u <- "Ulcerative colitis PRS"
i <- "Inflammatory Bowel Disease PRS"

CONTROLS <- data.frame(

Condition=c(c,c,c,c,c,c,c,c,c,u,u,u,u,u,u,u,u,u,i,i,i,i,i,i,i,i,i),

Symptom=c("All GI Symptoms", "Loose stools", "Diarrhea",
"Abdominal pain with meals", "Constipation", "Loose stools alternating with constipation" ,
"Abdominal pain relieved by defecation", "Gas", "Pain on stooling",

"All GI Symptoms", "Loose stools", "Diarrhea",
"Abdominal pain with meals", "Constipation", "Loose stools alternating with constipation" ,
"Abdominal pain relieved by defecation", "Gas", "Pain on stooling",

"All GI Symptoms", "Loose stools", "Diarrhea",
"Abdominal pain with meals", "Constipation", "Loose stools alternating with constipation" ,
"Abdominal pain relieved by defecation", "Gas", "Pain on stooling"),

Estimate=c(1.14, 1.73, 2.46, 0.75, 1.14, 2.57, 0.66, 1.90, 1.03,
1.37, 1.53, 5.26, 1.50, 1.20, 1.51, 0.93, 1.70, 1.07,
1.20, 1.57, 3.55, 0.98, 1.09, 1.75, 0.74, 1.41, 1.00),

Lower=c(0.88, 0.90, 0.80, 0.33, 0.67, 1.13, 0.34, 1.01, 0.55,
1.04, 0.84, 1.72, 0.69, 0.74, 0.79, 0.51, 0.94, 0.61,
0.88, 0.83, 1.25, 0.42, 0.64, 0.85, 0.38, 0.78, 0.53),

Upper=c(1.47, 3.51, 9.25, 1.59, 1.96, 6.55, 1.22, 3.80, 1.92,
1.84, 2.96, 25.86, 3.77, 1.98, 3.12, 1.69, 3.29, 1.91,
1.63, 3.12, 12.34, 2.23, 1.86, 3.77, 1.38, 2.61, 1.86)

)

#Edit the data formats for plotting
CONTROLS$Estimate <- as.numeric(as.character(CONTROLS$Estimate))
CONTROLS$Upper <- as.numeric(as.character(CONTROLS$Upper))
CONTROLS$Lower <- as.numeric(as.character(CONTROLS$Lower))
CONTROLS$Symptom <- relevel(CASES$Symptom, "All GI Symptoms")

#Subset to results to "All GI Symptoms"
CONTROLS_all_GI <- CONTROLS %>% filter(Symptom=="All GI Symptoms")

#Plot
ggplot(data=CONTROLS_all_GI,
    aes(x = Symptom,y = Estimate, ymin = Lower, ymax = Upper ))+
    geom_pointrange(aes(col= Symptom), colour="black")+
    geom_hline(aes(fill= Symptom),yintercept =1, linetype=2)+
    xlab('GI Symptom')+
    ylab("Log OR of GI Symptom (95% Confidence Interval)")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper,col= Symptom),width=0.2,cex=1, colour="black")+
    scale_y_log10(limits = c(0.7, 2)) +
    facet_wrap(~Condition,strip.position="top",nrow=1,scales = "free_x") +
    theme(plot.title=element_text(size=24,face="bold"),
    axis.text.x=element_blank(),
    axis.title=element_text(size=12,face="bold"))

#Remove results for "All GI Symptoms"
CONTROLS_not_all_GI <- CONTROLS %>% filter(Symptom!="All GI Symptoms")

#Plot
pallete <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#6a3d9a')

ggplot(data=CONTROLS_not_all_GI,
    aes(x = Symptom,y = Estimate, ymin = Lower, ymax = Upper ))+
    geom_pointrange(aes(col= Symptom))+
    geom_hline(aes(fill= Symptom),yintercept =1, linetype=2)+
    xlab('GI Symptom')+
    ylab("Log OR of GI Symptom (95% Confidence Interval)")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper,col= Symptom),width=0.2,cex=1)+
    scale_y_log10(limits = c(0.3, 32)) +
    facet_wrap(~Condition,strip.position="top",nrow=1,scales = "free_x") +
    theme(plot.title=element_text(size=24,face="bold"),
    axis.text.x=element_blank(),
    axis.title=element_text(size=12,face="bold")) +
    scale_color_manual(values=pallete)






