
# load all relevant libraries
library(nlme)
library(lme4)
library(boot)
library(car) 
library(reshape2)
library(ggplot2)
library(ez)
library(plyr)
library(ggsignif)
library(lsr)
library(sjmisc)
library(sjstats)
library(BayesFactor)
library(foreign)
library(dplyr)
library(lattice)
library(openxlsx)
options(scipen=9999)

# DATA CLEAN UP AND RESTRUCTURING #
# load: exp2data.csv
D = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# get dimension of dataframe
dim(D)

# remove unnecessary rows
D = D[c(1:64),]

# reshape dataframe from wide to long
D_tall =  reshape(D, varying = c(11:26), v.names = "measure", 
                  timevar = "condition",   direction = "long")

D_tall = D_tall[order(D_tall$ID),] # order the data frame in terms of participant ID;
# to avoid wonky things happening and to save yourself 
# a full-day headache in the future, reorder by ID
# immediately after reshaping the dataframe.

names(D_tall)
D_tall$id = NULL
names(D_tall)

D_tall$trialType = rep(c("control","control","control","control","control","control","control",
                         "control","control", "control","experimental","experimental",
                         "experimental","experimental","experimental","experimental"), times =64)

D_tall$testPhase = rep(c("first","first","first","first","first",
                         "second","second","second","second","second",
                         "first","first","first","second","second","second"), times = 64)

D_tall$objectType = rep(c("A","B","C","D","E",
                          "A","B","C","D","E",
                          "A","B","C",
                          "A","B","C"), times = 64)

D_tall$phaseOrder = rep(c("Phase 1","Phase 1","Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2","Phase 2","Phase 2",
                          "Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2"), times = 64)





# CHANGE SOME OF THE COLUMN NAMES
names(D_tall)
colnames(D_tall)[which(names(D_tall) == "AGE.Y.")] <- "AgeCat"
colnames(D_tall)[which(names(D_tall) == "AGE.M.")] <- "AgeNum"
colnames(D_tall)[which(names(D_tall) == "RACE")] <- "Race"
colnames(D_tall)[which(names(D_tall) == "SEX")] <- "Sex"
colnames(D_tall)[which(names(D_tall) == "BB.IS")] <- "Condition"
colnames(D_tall)[which(names(D_tall) == "SUBCONDITION")] <- "SubCondition"
colnames(D_tall)[which(names(D_tall) == "VIDORDER")] <- "Vidorder"
colnames(D_tall)[which(names(D_tall) == "PRETEST")] <- "Pretest"
colnames(D_tall)[which(names(D_tall) == "measure")] <- "choice"

# remove more unnecessary columns
names(D_tall)
D_tall$CONDITION = NULL
D_tall$condition = NULL

# colnames(D_tall)[which(names(D_tall) == "SUBCONDITION")] <- "Subcondition"
names(D_tall)


# get counts for choice 
table(D_tall$choice)




# RENAME LEVELS OF COLUMNS
D_tall$Condition = revalue(x = as.factor(D_tall$Condition), 
                           c("0" = "Backwards Blocking", "1"="Indirect Screening-Off"))

D_tall$Sex = revalue(x = as.factor(D_tall$Sex), 
                     c("F" = "Female", "M"="Male"))


# D_tall$Subcondition = revalue(x = as.factor(D_tall$Subcondition), 
#                     c("0" = "A", "1"="B", "2"="C", "3"="D"))

D_tall$Vidorder = revalue(x = as.factor(D_tall$Vidorder), 
                          c("0" = "LtoR", "1"="RtoL"))

D_tall$Pretest = revalue(x = as.factor(D_tall$Pretest), 
                         c("0" = "Incorrect", "1"="Correct"))


D_tall$AgeCat = as.factor(D_tall$AgeCat)
D_tall$testPhase = as.factor(D_tall$testPhase)
D_tall$objectType = as.factor(D_tall$objectType)
D_tall$trialType = as.factor(D_tall$trialType)
D_tall$choice = as.numeric(D_tall$choice)

# REODRDER COLUMNS
names(D_tall)
D_tall$condition = NULL
names(D_tall)
dim(D_tall)
D_tall = as.data.frame(D_tall[,c(1:3,5,4,7,8,6,11:14,10,9)])
fix(D_tall)




# get the number of participants in each condition by age
table(D_tall$Condition)/16

table(D_tall$Condition[D_tall$AgeCat=="5"])/16
table(D_tall$Condition[D_tall$AgeCat=="6"])/16

# get the number of participants in each condition by sex
table(D_tall$Condition[D_tall$AgeCat=="5" & D_tall$Sex=="Male"])/16
table(D_tall$Condition[D_tall$AgeCat=="5" & D_tall$Sex=="Female"])/16

table(D_tall$Condition[D_tall$AgeCat=="6" & D_tall$Sex=="Male"])/16
table(D_tall$Condition[D_tall$AgeCat=="6" & D_tall$Sex=="Female"])/16

# mean ages and ranges
mean(D_tall$AgeNum[D_tall$AgeCat=="5"])
range(D_tall$AgeNum[D_tall$AgeCat=="5"])
sd(D_tall$AgeNum[D_tall$AgeCat=="5"])

mean(D_tall$AgeNum[D_tall$AgeCat=="6"])
range(D_tall$AgeNum[D_tall$AgeCat=="6"])
sd(D_tall$AgeNum[D_tall$AgeCat=="6"])

# demographics info
# asian 
table(D_tall$Race)[[1]]/sum(table(D_tall$Race))

# black 
table(D_tall$Race)[[2]]/sum(table(D_tall$Race))

# hispanic
table(D_tall$Race)[[3]]/sum(table(D_tall$Race))

# white
table(D_tall$Race)[[4]]/sum(table(D_tall$Race))


#####################
#####################
###               ###                     
### MAIN ANALYSIS ###
###               ###
#####################
#####################

## BB EXPERIMENTAL CONDITION ##
glmer.fit = glmer(choice~(AgeNum+Condition+trialType+phaseOrder+objectType)^5+(1|ID), family=binomial, 
                  data=D_tall,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e3)))
Anova(glmer.fit)


## follow up analyses 
# BB
bb.glmer = glmer(choice~trialType+(1|ID), family=binomial,
                            data=D_tall)
Anova(bb.glmer)

summary(bb.glmer)
exp(fixef(bb.glmer)) 
bb.CIs = exp(confint(bb.glmer, method = "profile"))
bb.CIs

################################################################################################################
# ANALYSES THAT ASSESS EVIDENCE OF BACKWARDS BLOCKING BY COMPARING REDUNDANT OBJECTS ACROSS THE EXPERIMENTAL ###
# AND CONTROL TRIALS                                                                                         ###
################################################################################################################
# create a dataframe just for the BB condition, and only for the redundant objects within the experimental
# and control conditions

## backwards blocking ##
bb.df = data.frame(ID = c(1:64), C_exp = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"],
                  A_control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"],
                  B_control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"],
                  C_control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"])

bb.df_tall = reshape(bb.df, varying = c(2:5), v.names = "choice", 
                        timevar = "condition",   direction = "long")
bb.df_tall = bb.df_tall[order(bb.df_tall$ID),] 

bb.df_tall$objectType = rep(c("C","A","B","C"), times = 32)
bb.df_tall$trialType = rep(c("experimental","control",
                                "control","control"), times = 32)
bb.df_tall$objectType = factor(bb.df_tall$objectType)
bb.df_tall$trialType = factor(bb.df_tall$trialType)

# run model to test for main effects and two-way interactions
bb_condition_only_glmer = glmer(choice~(objectType+trialType)^2+(1|ID), family=binomial,
                                data=bb.df_tall)
Anova(bb_condition_only_glmer)

bb_condition_only_trial_type_glmer = glmer(choice~trialType+(1|ID), family=binomial,
                                data=bb.df_tall)
summary(bb_condition_only_trial_type_glmer)
exp(fixef(bb_condition_only_trial_type_glmer))
exp(confint(bb_condition_only_trial_type_glmer, method = "profile"))


## indirect screening-off ##
iso.df = data.frame(ID = c(1:64), 
                    C_exp = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"],
                    A_control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"],
                    B_control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"],
                    C_control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"])

iso.df_tall = reshape(iso.df, varying = c(2:5), v.names = "choice", 
                      timevar = "condition",   direction = "long")
iso.df_tall = iso.df_tall[order(iso.df_tall$ID),] 

iso.df_tall$objectType = rep(c("C","A","B","C"), times = 32)
iso.df_tall$trialType = rep(c("experimental","control",
                              "control","control"), times = 32)
iso.df_tall$objectType = factor(iso.df_tall$objectType)
iso.df_tall$trialType = factor(iso.df_tall$trialType)

# run model to test for main effects and two-way interactions
iso_condition_only_glmer = glmer(choice~(objectType+trialType)^2+(1|ID), family=binomial,
                                 data=iso.df_tall)
Anova(iso_condition_only_glmer)

iso_condition_only_trial_type_glmer = glmer(choice~trialType+(1|ID), family=binomial,
                                            data=iso.df_tall)
summary(iso_condition_only_trial_type_glmer)
exp(fixef(iso_condition_only_trial_type_glmer))
exp(confint(iso_condition_only_trial_type_glmer, method = "Wald"))
