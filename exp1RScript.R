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
library(BFpack)
options(scipen=9999)

# DATA CLEAN UP AND RESTRUCTURING #
# load: newData03202023.csv
D = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# remove unnecessary columns
names(D)
D$CTRL_1__E = NULL
D$CTRL_2__E = NULL
names(D)
dim(D)


D_tall =  reshape(D, varying = c(10:23), v.names = "measure", 
                  timevar = "condition",   direction = "long")

D_tall = D_tall[order(D_tall$ID),] # order the data frame in terms of participant ID;
# to avoid wonky things happening and to save yourself 
# a full-day headache in the future, reorder by ID
# immediately after reshaping the dataframe.

names(D_tall)
D_tall$id = NULL
names(D_tall)

D_tall$trialType = rep(c("control","control","control","control","control","control","control",
                         "control","main","main",
                         "main","main","main","main"), times = 64)

D_tall$testPhase = rep(c("first","first","first","first","second","second","second","second",
                         "first","first","first","second","second","second"), times = 64)

D_tall$objectType = rep(c("A","B","C","D",
                          "A","B","C","D",
                          "A","B","C",
                          "A","B","C"), times = 64)

D_tall$phaseOrder = rep(c("Phase 1","Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2","Phase 2",
                          "Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2"), times = 64)





# CHANGE SOME OF THE COLUMN NAMES
names(D_tall)
colnames(D_tall)[which(names(D_tall) == "AGE.Y.")] <- "Age"
colnames(D_tall)[which(names(D_tall) == "SEX")] <- "Sex"
colnames(D_tall)[which(names(D_tall) == "BB.IS")] <- "Condition"
colnames(D_tall)[which(names(D_tall) == "SUBCONDITION")] <- "SubCondition"
colnames(D_tall)[which(names(D_tall) == "VIDORDER")] <- "Vidorder"
colnames(D_tall)[which(names(D_tall) == "PRETEST")] <- "Pretest"
colnames(D_tall)[which(names(D_tall) == "measure")] <- "choice"

# remove more unnecessary columns
names(D_tall)
D_tall$CONDITION = NULL

# colnames(D_tall)[which(names(D_tall) == "SUBCONDITION")] <- "Subcondition"
names(D_tall)





# MODIFY CHOICES COLUMN
# Deal with "unsures" in the choice column
D_tall$choices = rep(0, nrow(D_tall))
for(i in 1:nrow(D_tall)){
  if(is.na(D_tall$choice[i])==T|D_tall$choice[i]=="NaN"){
    D_tall$choices[i]= NA
  } else if(D_tall$choice[i]==1){
    D_tall$choices[i]=1
  } else if(D_tall$choice[i]==0){
    D_tall$choices[i]=0
  } else {
    D_tall$choices[i]=NA
  }
}

D_tall$choice = D_tall$choices
D_tall$choices = NULL
D_tall$choice = as.numeric(D_tall$choice)

# get counts for choice 
table(D_tall$choice)




# RENAME LEVELS OF COLUMNS
D_tall$Condition = revalue(x = as.factor(D_tall$Condition), 
                           c("0" = "Backwards Blocking", "1"="Indirect Screening-Off"))

D_tall$Sex = revalue(x = as.factor(D_tall$Sex), 
                     c("0" = "female", "1"="male"))


# D_tall$Subcondition = revalue(x = as.factor(D_tall$Subcondition), 
#                     c("0" = "A", "1"="B", "2"="C", "3"="D"))

D_tall$Vidorder = revalue(x = as.factor(D_tall$Vidorder), 
                          c("0" = "LtoR", "1"="RtoL"))

D_tall$Pretest = revalue(x = as.factor(D_tall$Pretest), 
                         c("0" = "Incorrect", "1"="Correct"))


D_tall$Age = as.factor(D_tall$Age)
D_tall$testPhase = as.factor(D_tall$testPhase)
D_tall$objectType = as.factor(D_tall$objectType)
D_tall$trialType = as.factor(D_tall$trialType)

# REODRDER COLUMNS
D_tall$condition = NULL
names(D_tall)
dim(D_tall)
D_tall = as.data.frame(D_tall[,c(1:4,6:7,5,10:13,8:9)])
fix(D_tall)


###################################
###################################
###                             ###                     
### PARTICIPANT CHARACTERISTICS ###
###                             ###
###################################
###################################

# mean age and range for the 5-year-olds
mean(D_tall$AgeNum[D_tall$Age=="5"])
sd(D_tall$AgeNum[D_tall$Age=="5"])
range(D_tall$AgeNum[D_tall$Age=="5"])


# mean age and range for the 6-year-olds
mean(D_tall$AgeNum[D_tall$Age=="6"])
range(D_tall$AgeNum[D_tall$Age=="6"])


########################
########################
###                  ###                     
### MODEL COMPARISON ###
###                  ###
########################
########################
# define linear and linear mixed-effects models
lm.fit = lm(choice~(AgeNum+Condition+trialType+phaseOrder+objectType)^5, 
            data=D_tall)
lmer.fit = lmer(choice~(AgeNum+Condition+trialType+phaseOrder+objectType)^5+(1|ID), 
                data=D_tall)

# compare the models via a log-likelihood test
anova(lmer.fit,lm.fit)


# Conclusion: The linear mixed-effects model wins

#####################
#####################
###               ###                     
### MAIN ANALYSIS ###
###               ###
#####################
#####################
# this is just the model from above
lmer.fit = lmer(choice~(AgeNum+Condition+trialType+phaseOrder+objectType)^5+(1|ID), 
                data=D_tall)
Anova(lmer.fit)


##########################################################
##########################################################
###                                                    ###
### ANALYSIS FOR CHILDREN ASSIGNED TO THE BB CONDITION ###
###                                                    ###
##########################################################
##########################################################

# BB MAIN 
A.BB.MAIN.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="A"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="A"])-2
length(A.BB.MAIN.SUM)
mean(A.BB.MAIN.SUM, na.rm=TRUE)

B.BB.MAIN.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="B"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="B"])-2
length(B.BB.MAIN.SUM)
mean(B.BB.MAIN.SUM, na.rm=TRUE)

C.BB.MAIN.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="C"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="C"])-2
length(C.BB.MAIN.SUM)
mean(C.BB.MAIN.SUM, na.rm=TRUE)


# BB CONTROL
A.BB.CONTROL.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="A"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="A"])-2
length(A.BB.CONTROL.SUM)
mean(A.BB.CONTROL.SUM, na.rm=TRUE)

B.BB.CONTROL.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="B"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="B"])-2
length(B.BB.CONTROL.SUM)
mean(B.BB.CONTROL.SUM, na.rm=TRUE)

C.BB.CONTROL.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="C"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="C"])-2
length(C.BB.CONTROL.SUM)
mean(C.BB.CONTROL.SUM, na.rm=TRUE)

D.BB.CONTROL.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="D"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="D"])-2
length(D.BB.CONTROL.SUM)
mean(D.BB.CONTROL.SUM, na.rm=TRUE)


# Combined dataframe
D.BB.DF = data.frame(ID = c(1:31), Condition = rep(c("Backwards Blocking"),31),
                     A.BB.MAIN.SUM = A.BB.MAIN.SUM, 
                     B.BB.MAIN.SUM = B.BB.MAIN.SUM,
                     C.BB.MAIN.SUM = C.BB.MAIN.SUM,
                     A.BB.CONTROL.SUM = A.BB.CONTROL.SUM,
                     B.BB.CONTROL.SUM = B.BB.CONTROL.SUM,
                     C.BB.CONTROL.SUM = C.BB.CONTROL.SUM,
                     D.BB.CONTROL.SUM = D.BB.CONTROL.SUM)
names(D.BB.DF)
dim(D.BB.DF)

D.BB.DF_tall = reshape(D.BB.DF, varying = c(3:9), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D.BB.DF_tall = D.BB.DF_tall[order(D.BB.DF_tall$ID),] 

D.BB.DF_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 31)
D.BB.DF_tall$eventType = rep(c("main","main","main",
                               "control","control","control","control"), times = 31)

names(D.BB.DF_tall)
D.BB.DF_tall$id = NULL
D.BB.DF_tall$condition = NULL
names(D.BB.DF_tall)
dim(D.BB.DF_tall)
D.BB.DF_tall = D.BB.DF_tall[,c(1:2,5,4,3)]
names(D.BB.DF_tall)


D.BB.DF_tall$objects = factor(D.BB.DF_tall$objects)
D.BB.DF_tall$eventType = factor(D.BB.DF_tall$eventType)
D.BB.DF_tall$Condition = factor(D.BB.DF_tall$Condition)

fix(D.BB.DF_tall)

###########################################################
###########################################################
###                                                     ###
### ANALYSIS FOR CHILDREN ASSIGNED TO THE ISO CONDITION ###
###                                                     ###
###########################################################
###########################################################

# ISO MAIN 
A.ISO.MAIN.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="A"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="A"])-2
length(A.ISO.MAIN.SUM)
mean(A.ISO.MAIN.SUM, na.rm=TRUE)

B.ISO.MAIN.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="B"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="B"])-2
length(B.ISO.MAIN.SUM)
mean(B.ISO.MAIN.SUM, na.rm=TRUE)

C.ISO.MAIN.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="C"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="C"])-2
length(C.ISO.MAIN.SUM)
mean(C.ISO.MAIN.SUM, na.rm=TRUE)


# ISO CONTROL
A.ISO.CONTROL.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="A"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="A"])-2
length(A.ISO.CONTROL.SUM)
mean(A.ISO.CONTROL.SUM, na.rm=TRUE)

B.ISO.CONTROL.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="B"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="B"])-2
length(B.ISO.CONTROL.SUM)
mean(B.ISO.CONTROL.SUM, na.rm=TRUE)

C.ISO.CONTROL.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="C"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="C"])-2
length(C.ISO.CONTROL.SUM)
mean(C.ISO.CONTROL.SUM, na.rm=TRUE)

D.ISO.CONTROL.SUM = as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="D"])+
  as.numeric(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder == "Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="D"])-2
length(D.ISO.CONTROL.SUM)
mean(D.ISO.CONTROL.SUM, na.rm=TRUE)


# Combined dataframe
D.ISO.DF = data.frame(ID = c(1:33), Condition = rep(c("Indirect Screening-Off"),33),
                      A.ISO.MAIN.SUM = A.ISO.MAIN.SUM, 
                      B.ISO.MAIN.SUM = B.ISO.MAIN.SUM,
                      C.ISO.MAIN.SUM = C.ISO.MAIN.SUM,
                      A.ISO.CONTROL.SUM = A.ISO.CONTROL.SUM,
                      B.ISO.CONTROL.SUM = B.ISO.CONTROL.SUM,
                      C.ISO.CONTROL.SUM = C.ISO.CONTROL.SUM,
                      D.ISO.CONTROL.SUM = D.ISO.CONTROL.SUM)
names(D.ISO.DF)
dim(D.ISO.DF)

D.ISO.DF_tall = reshape(D.ISO.DF, varying = c(3:9), v.names = "measure", 
                        timevar = "condition",   direction = "long")
D.ISO.DF_tall = D.ISO.DF_tall[order(D.ISO.DF_tall$ID),] 

D.ISO.DF_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 33)
D.ISO.DF_tall$eventType = rep(c("main","main","main",
                                "control","control","control","control"), times = 33)

names(D.ISO.DF_tall)
D.ISO.DF_tall$id = NULL
D.ISO.DF_tall$condition = NULL
names(D.ISO.DF_tall)
dim(D.ISO.DF_tall)
D.ISO.DF_tall = D.ISO.DF_tall[,c(1:2,5,4,3)]
names(D.ISO.DF_tall)


D.ISO.DF_tall$objects = factor(D.ISO.DF_tall$objects)
D.ISO.DF_tall$eventType = factor(D.ISO.DF_tall$eventType)
D.ISO.DF_tall$Condition = factor(D.ISO.DF_tall$Condition)

fix(D.ISO.DF_tall)



##################################################################
##################################################################
###                                                            ###
### COMBINE THE DATA FOR CHILDREN IN THE BB AND ISO CONDITIONS ###
###                                                            ###
##################################################################
##################################################################


# Combine the BB and ISO dataframes
D_tall_new = rbind(D.BB.DF_tall, D.ISO.DF_tall)
D_tall_new$ID = rep(c(1:64),each=7) 


# BB follow-up analyses
lmer.fit.2 = lmer(measure~objects+(1|ID), 
                  data = D_tall_new[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control",])
Anova(lmer.fit.2)

# A
A = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"], na.rm=TRUE)

# B
B = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"], na.rm=TRUE)

# C
C = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"], na.rm=TRUE)

# D
D = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="D"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="D"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="D"], na.rm=TRUE)

t.test(D,A, alternative="two.sided", paired = TRUE)
t.test(D,B, alternative="two.sided", paired = TRUE)
t.test(D,C, alternative="two.sided", paired = TRUE)



lmer.fit.3 = lmer(measure~objects+(1|ID), 
                  data = D_tall_new[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main",])
Anova(lmer.fit.3)

# A
A = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="A"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="A"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="A"], na.rm=TRUE)

# B
B = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"], na.rm=TRUE)

# C
C = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"], na.rm=TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)

## CRITICAL COMPARISON: this looks at the treatment of B in the control condition of BB compared 
# to the treatment of B in the main 
# condition of BB ##
lm.fit.5b = lm(measure[D_tall_new$objects=="B" & D_tall_new$Condition=="Backwards Blocking"]~
                 eventType[D_tall_new$objects=="B" & D_tall_new$Condition=="Backwards Blocking"], 
               data = D_tall_new, 
               na.action=na.exclude)
Anova(lm.fit.5b)

t.test(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$objects=="B" & D_tall_new$eventType=="main"],
       D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$objects=="B" & D_tall_new$eventType=="control"],
       paired = TRUE, alternative = "two.sided", na.rm = TRUE)

lm.null = lm(measure[D_tall_new$objects=="B" & D_tall_new$Condition=="Backwards Blocking"]~
               1, 
             data = D_tall_new, 
             na.action=na.exclude)
lm.alt = lm(measure[D_tall_new$objects=="B" & D_tall_new$Condition=="Backwards Blocking"]~
              eventType[D_tall_new$objects=="B" & D_tall_new$Condition=="Backwards Blocking"], 
            data = D_tall_new, 
            na.action=na.exclude)
null.bic = BIC(lm.null)
alt.bic = BIC(lm.alt)

BF01 = exp((alt.bic - null.bic)/2) 
BF10 = 1/BF01

# ISO follow-up analyses
lmer.fit.4 = lmer(measure~objects+(1|ID), 
                  data = D_tall_new[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control",])
Anova(lmer.fit.4)

# A
A = D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"]
mean(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"], na.rm=TRUE)

# B
B = D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"]
mean(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"], na.rm=TRUE)

# C
C = D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"]
mean(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"], na.rm=TRUE)

# D
D = D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="D"]
mean(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="D"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="control" & D_tall_new$objects=="D"], na.rm=TRUE)

t.test(D,A, alternative="two.sided", paired = TRUE)
t.test(D,B, alternative="two.sided", paired = TRUE)
t.test(D,C, alternative="two.sided", paired = TRUE)



lmer.fit.5 = lmer(measure~objects+(1|ID), 
                  data = D_tall_new[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main",])
Anova(lmer.fit.5)

# A
A = D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="A"]
mean(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="A"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="A"], na.rm=TRUE)

# B
B = D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"]
mean(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"], na.rm=TRUE)

# C
C = D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"]
mean(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Indirect Screening-Off" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"], na.rm=TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)


########################################
########################################
# OLD AND NEW OPERATIONALIZATION OF BB #
########################################
########################################
# A control
A.control = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="A"], na.rm=TRUE)

# B main v B control
B.control = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="B"], na.rm=TRUE)


B.main = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="B"], na.rm=TRUE)


# C main v C control
C.control = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="control" & D_tall_new$objects=="C"], na.rm=TRUE)


C.main = D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"]
mean(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"], na.rm=TRUE)
sd(D_tall_new$measure[D_tall_new$Condition=="Backwards Blocking" & D_tall_new$eventType=="main" & D_tall_new$objects=="C"], na.rm=TRUE)


# create data frame and add columns from the variables above 
D.new.op = data.frame(ID = c(1:31), A.control = A.control, B.control = B.control, B.main = B.main,
                         C.control = C.control, C.main = C.main)

names(D.new.op)
dim(D.new.op)

D.new.op_tall = reshape(D.new.op, varying = c(2:6), v.names = "measure", 
                        timevar = "condition",   direction = "long")
D.new.op_tall = D.new.op_tall[order(D.new.op_tall$ID),] 

D.new.op_tall$objects = rep(c("A","B","B","C","C"), times = 31)
D.new.op_tall$eventType = rep(c("control","control","main",
                                "control","main"), times = 31)
D.new.op_tall$objects = factor(D.new.op_tall$objects)
D.new.op_tall$eventType = factor(D.new.op_tall$eventType)

# main analysis 
lm.fit.6 = lmer(measure~(objects+eventType)^2+(1|ID), 
              data = D.new.op_tall)
Anova(lm.fit.6)


mean(D.new.op_tall$measure[D.new.op_tall$eventType=="main"], na.rm = TRUE)
sd(D.new.op_tall$measure[D.new.op_tall$eventType=="main"], na.rm = TRUE)

mean(D.new.op_tall$measure[D.new.op_tall$eventType=="control"], na.rm = TRUE)
sd(D.new.op_tall$measure[D.new.op_tall$eventType=="control"], na.rm = TRUE)


# follow-up comparisons #

# B main vs A control = significant
t.test(D.new.op$B.main,
       D.new.op$A.control, 
       paired = TRUE,
       alternative = "two.sided")

mean(D.new.op$B.main, na.rm = TRUE)
sd(D.new.op$B.main, na.rm = TRUE)
mean(D.new.op$A.control, na.rm = TRUE)
sd(D.new.op$A.control, na.rm = TRUE)


# B main vs B control = significant
t.test(D.new.op$B.main,
       D.new.op$B.control, 
       paired = TRUE,
       alternative = "two.sided")

mean(D.new.op$B.main, na.rm = TRUE)
sd(D.new.op$B.main, na.rm = TRUE)
mean(D.new.op$B.control, na.rm = TRUE)
sd(D.new.op$B.control, na.rm = TRUE)


# B main vs C control = significant
t.test(D.new.op$B.main,
       D.new.op$C.control, 
       paired = TRUE,
       alternative = "two.sided")


mean(D.new.op$B.main, na.rm = TRUE)
sd(D.new.op$B.main, na.rm = TRUE)
mean(D.new.op$C.control, na.rm = TRUE)
sd(D.new.op$C.control, na.rm = TRUE)



# C main vs A control 
t.test(D.new.op$C.main,
       D.new.op$A.control, 
       paired = TRUE,
       alternative = "two.sided")

mean(D.new.op$C.main, na.rm = TRUE)
sd(D.new.op$C.main, na.rm = TRUE)
mean(D.new.op$A.control, na.rm = TRUE)
sd(D.new.op$A.control, na.rm = TRUE)


# C main vs B control 
t.test(D.new.op$C.main,
       D.new.op$B.control, 
       paired = TRUE,
       alternative = "two.sided")

mean(D.new.op$C.main, na.rm = TRUE)
sd(D.new.op$C.main, na.rm = TRUE)
mean(D.new.op$B.control, na.rm = TRUE)
sd(D.new.op$B.control, na.rm = TRUE)


# C main vs C control 
t.test(D.new.op$C.main,
       D.new.op$C.control, 
       paired = TRUE,
       alternative = "two.sided")


mean(D.new.op$C.main, na.rm = TRUE)
sd(D.new.op$C.main, na.rm = TRUE)
mean(D.new.op$C.control, na.rm = TRUE)
sd(D.new.op$C.control, na.rm = TRUE)






# Figure
condition_barplot = ggplot(D_tall_new, aes(eventType, measure, fill = objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  facet_wrap(~Condition) +
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                             "#888888",
                             "#C8C8C8",
                             "#696969")) +
  
  coord_cartesian(ylim=c(0, 2.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank()) + 
  ylab("# of questions children judged object was a blicket")
