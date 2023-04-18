##########################
##########################
## MODEL/DATA COMPARISON #
##########################
##########################

# load libraries
library(nlme)
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
library(BFpack)
options(scipen=9999)

# upload the participant data
D = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# remove unnecessary columns
names(D)
D$CTRL_1__E = NULL
D$CTRL_2__E = NULL
names(D)
dim(D)


D_tall =  reshape(D, varying = c(9:22), v.names = "measure", 
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
D_tall$choice = as.factor(D_tall$choice)

# get counts for choice 
table(D_tall$choice)




# RENAME LEVELS OF COLUMNS
D_tall$Condition = revalue(x = as.factor(D_tall$Condition), 
                           c("0" = "BB", "1"="ISO"))

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
D_tall = as.data.frame(D_tall[,c(1:3,5:6,4,9:12,7:8)])
fix(D_tall)


# subset dataframes by experiments
table(D_tall$Age)
D_tall_Exp1_5and6yos = D_tall
D_tall_Exp1_5and6yos$Age = factor(D_tall_Exp1_5and6yos$Age)
D_tall_Exp1_5and6yos$Condition = factor(D_tall_Exp1_5and6yos$Condition)

dim(D_tall_Exp1_5and6yos)



##########################################################
##########################################################
###                                                    ###
### ANALYSIS FOR CHILDREN ASSIGNED TO THE BB CONDITION ###
###                                                    ###
##########################################################
##########################################################

# BB MAIN 
A.BB.MAIN.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(A.BB.MAIN.SUM.5s)
mean(A.BB.MAIN.SUM.5s, na.rm=TRUE)


A.BB.MAIN.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(A.BB.MAIN.SUM.6s)
mean(A.BB.MAIN.SUM.6s, na.rm=TRUE)


B.BB.MAIN.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(B.BB.MAIN.SUM.5s)
mean(B.BB.MAIN.SUM.5s, na.rm=TRUE)


B.BB.MAIN.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(B.BB.MAIN.SUM.6s)
mean(B.BB.MAIN.SUM.6s, na.rm=TRUE)


C.BB.MAIN.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(C.BB.MAIN.SUM.5s)
mean(C.BB.MAIN.SUM.5s, na.rm=TRUE)


C.BB.MAIN.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(C.BB.MAIN.SUM.6s)
mean(C.BB.MAIN.SUM.6s, na.rm=TRUE)


# BB CONTROL
A.BB.CONTROL.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(A.BB.CONTROL.SUM.5s)
mean(A.BB.CONTROL.SUM.5s, na.rm=TRUE)


A.BB.CONTROL.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(A.BB.CONTROL.SUM.6s)
mean(A.BB.CONTROL.SUM.6s, na.rm=TRUE)


B.BB.CONTROL.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(B.BB.CONTROL.SUM.5s)
mean(B.BB.CONTROL.SUM.5s, na.rm=TRUE)


B.BB.CONTROL.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(B.BB.CONTROL.SUM.6s)
mean(B.BB.CONTROL.SUM.6s, na.rm=TRUE)


C.BB.CONTROL.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(C.BB.CONTROL.SUM.5s)
mean(C.BB.CONTROL.SUM.5s, na.rm=TRUE)


C.BB.CONTROL.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(C.BB.CONTROL.SUM.6s)
mean(C.BB.CONTROL.SUM.6s, na.rm=TRUE)

D.BB.CONTROL.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(D.BB.CONTROL.SUM.5s)
mean(D.BB.CONTROL.SUM.5s, na.rm=TRUE)


D.BB.CONTROL.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(D.BB.CONTROL.SUM.6s)
mean(D.BB.CONTROL.SUM.6s, na.rm=TRUE)


# 5s data frame
D.BB.DF.5s = data.frame(ID = c(1:15), Age = rep(c("5"),15), Condition = rep(c("BB"),15),
                        A.BB.MAIN.SUM.5s = A.BB.MAIN.SUM.5s, 
                        B.BB.MAIN.SUM.5s = B.BB.MAIN.SUM.5s,
                        C.BB.MAIN.SUM.5s = C.BB.MAIN.SUM.5s,
                        A.BB.CONTROL.SUM.5s = A.BB.CONTROL.SUM.5s,
                        B.BB.CONTROL.SUM.5s = B.BB.CONTROL.SUM.5s,
                        C.BB.CONTROL.SUM.5s = C.BB.CONTROL.SUM.5s,
                        D.BB.CONTROL.SUM.5s = D.BB.CONTROL.SUM.5s)
names(D.BB.DF.5s)
dim(D.BB.DF.5s)

D.BB.DF.5s_tall = reshape(D.BB.DF.5s, varying = c(4:10), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D.BB.DF.5s_tall = D.BB.DF.5s_tall[order(D.BB.DF.5s_tall$ID),] 

D.BB.DF.5s_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 15)
D.BB.DF.5s_tall$eventType = rep(c("main","main","main",
                                  "control","control","control","control"), times = 15)

names(D.BB.DF.5s_tall)
D.BB.DF.5s_tall$id = NULL
D.BB.DF.5s_tall$condition = NULL
names(D.BB.DF.5s_tall)
dim(D.BB.DF.5s_tall)
D.BB.DF.5s_tall = D.BB.DF.5s_tall[,c(1:3,5:6,4)]
names(D.BB.DF.5s_tall)

D.BB.DF.5s_tall$Age = factor(D.BB.DF.5s_tall$Age)
D.BB.DF.5s_tall$objects = factor(D.BB.DF.5s_tall$objects)
D.BB.DF.5s_tall$eventType = factor(D.BB.DF.5s_tall$eventType)
D.BB.DF.5s_tall$Condition = factor(D.BB.DF.5s_tall$Condition)


# 6s data frame
D.BB.DF.6s = data.frame(ID = c(1:16), Age = rep(c("6"),16), Condition = rep(c("BB"),16),
                        A.BB.MAIN.SUM.6s = A.BB.MAIN.SUM.6s, 
                        B.BB.MAIN.SUM.6s = B.BB.MAIN.SUM.6s,
                        C.BB.MAIN.SUM.6s = C.BB.MAIN.SUM.6s,
                        A.BB.CONTROL.SUM.6s = A.BB.CONTROL.SUM.6s,
                        B.BB.CONTROL.SUM.6s = B.BB.CONTROL.SUM.6s,
                        C.BB.CONTROL.SUM.6s = C.BB.CONTROL.SUM.6s,
                        D.BB.CONTROL.SUM.6s = D.BB.CONTROL.SUM.6s)
names(D.BB.DF.6s)

D.BB.DF.6s_tall = reshape(D.BB.DF.6s, varying = c(4:10), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D.BB.DF.6s_tall = D.BB.DF.6s_tall[order(D.BB.DF.6s_tall$ID),] 

D.BB.DF.6s_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 16)
D.BB.DF.6s_tall$eventType = rep(c("main","main","main",
                                  "control","control","control","control"), times = 16)

names(D.BB.DF.6s_tall)
D.BB.DF.6s_tall$id = NULL
D.BB.DF.6s_tall$condition = NULL
names(D.BB.DF.6s_tall)
dim(D.BB.DF.6s_tall)
D.BB.DF.6s_tall = D.BB.DF.6s_tall[,c(1:3,5:6,4)]
names(D.BB.DF.6s_tall)

D.BB.DF.6s_tall$Age = factor(D.BB.DF.6s_tall$Age)
D.BB.DF.6s_tall$objects = factor(D.BB.DF.6s_tall$objects)
D.BB.DF.6s_tall$eventType = factor(D.BB.DF.6s_tall$eventType)
D.BB.DF.6s_tall$Condition = factor(D.BB.DF.6s_tall$Condition)

###########################################################
###########################################################
###                                                     ###
### ANALYSIS FOR CHILDREN ASSIGNED TO THE ISO CONDITION ###
###                                                     ###
###########################################################
###########################################################
# ISO MAIN

A.ISO.MAIN.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(A.ISO.MAIN.SUM.5s)
mean(A.ISO.MAIN.SUM.5s, na.rm=TRUE)


A.ISO.MAIN.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(A.ISO.MAIN.SUM.6s)
mean(A.ISO.MAIN.SUM.6s, na.rm=TRUE)


B.ISO.MAIN.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(B.ISO.MAIN.SUM.5s)
mean(B.ISO.MAIN.SUM.5s, na.rm=TRUE)


B.ISO.MAIN.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(B.ISO.MAIN.SUM.6s)
mean(B.ISO.MAIN.SUM.6s, na.rm=TRUE)


C.ISO.MAIN.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(C.ISO.MAIN.SUM.5s)
mean(C.ISO.MAIN.SUM.5s, na.rm=TRUE)


C.ISO.MAIN.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(C.ISO.MAIN.SUM.6s)
mean(C.ISO.MAIN.SUM.6s, na.rm=TRUE)


# ISO CONTROL
A.ISO.CONTROL.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(A.ISO.CONTROL.SUM.5s)
mean(A.ISO.CONTROL.SUM.5s, na.rm=TRUE)


A.ISO.CONTROL.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(A.ISO.CONTROL.SUM.6s)
mean(A.ISO.CONTROL.SUM.6s, na.rm=TRUE)


B.ISO.CONTROL.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(B.ISO.CONTROL.SUM.5s)
mean(B.ISO.CONTROL.SUM.5s, na.rm=TRUE)


B.ISO.CONTROL.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(B.ISO.CONTROL.SUM.6s)
mean(B.ISO.CONTROL.SUM.6s, na.rm=TRUE)


C.ISO.CONTROL.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(C.ISO.CONTROL.SUM.5s)
mean(C.ISO.CONTROL.SUM.5s, na.rm=TRUE)


C.ISO.CONTROL.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(C.ISO.CONTROL.SUM.6s)
mean(C.ISO.CONTROL.SUM.6s, na.rm=TRUE)

D.ISO.CONTROL.SUM.5s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D" & D_tall_Exp1_5and6yos$Age=="5"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D" & D_tall_Exp1_5and6yos$Age=="5"])-2
length(D.ISO.CONTROL.SUM.5s)
mean(D.ISO.CONTROL.SUM.5s, na.rm=TRUE)


D.ISO.CONTROL.SUM.6s = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D" & D_tall_Exp1_5and6yos$Age=="6"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D" & D_tall_Exp1_5and6yos$Age=="6"])-2
length(D.ISO.CONTROL.SUM.6s)
mean(D.ISO.CONTROL.SUM.6s, na.rm=TRUE)


# 5s data frame
# note: the number "16" comes from the length of the one of the columns above
# e.g., length(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C" & D_tall_Exp1_5and6yos$Age=="5"])
D.ISO.DF.5s = data.frame(ID = c(1:17), Age = rep(c("5"),17), Condition = rep(c("ISO"),17),
                         A.ISO.MAIN.SUM.5s = A.ISO.MAIN.SUM.5s, 
                         B.ISO.MAIN.SUM.5s = B.ISO.MAIN.SUM.5s,
                         C.ISO.MAIN.SUM.5s = C.ISO.MAIN.SUM.5s,
                         A.ISO.CONTROL.SUM.5s = A.ISO.CONTROL.SUM.5s,
                         B.ISO.CONTROL.SUM.5s = B.ISO.CONTROL.SUM.5s,
                         C.ISO.CONTROL.SUM.5s = C.ISO.CONTROL.SUM.5s,
                         D.ISO.CONTROL.SUM.5s = D.ISO.CONTROL.SUM.5s)
names(D.ISO.DF.5s)

D.ISO.DF.5s_tall = reshape(D.ISO.DF.5s, varying = c(4:10), v.names = "measure", 
                           timevar = "condition",   direction = "long")
D.ISO.DF.5s_tall = D.ISO.DF.5s_tall[order(D.ISO.DF.5s_tall$ID),] 

D.ISO.DF.5s_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 17)
D.ISO.DF.5s_tall$eventType = rep(c("main","main","main",
                                   "control","control","control","control"), times = 17)

names(D.ISO.DF.5s_tall)
D.ISO.DF.5s_tall$id = NULL
D.ISO.DF.5s_tall$condition = NULL
names(D.ISO.DF.5s_tall)
D.ISO.DF.5s_tall = D.ISO.DF.5s_tall[,c(1:3,5:6,4)]
names(D.ISO.DF.5s_tall)


D.ISO.DF.5s_tall$Age = factor(D.ISO.DF.5s_tall$Age)
D.ISO.DF.5s_tall$objects = factor(D.ISO.DF.5s_tall$objects)
D.ISO.DF.5s_tall$eventType = factor(D.ISO.DF.5s_tall$eventType)
D.ISO.DF.5s_tall$Condition = factor(D.ISO.DF.5s_tall$Condition)


# 6s data frame
# Note: The number "15" comes from the length of one of the columns above;
# e.g., D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A" & D_tall_Exp1_5and6yos$Age=="6"]
D.ISO.DF.6s = data.frame(ID = c(1:16), Age = rep(c("6"),16), Condition = rep(c("ISO"),16),
                         A.ISO.MAIN.SUM.6s = A.ISO.MAIN.SUM.6s, 
                         B.ISO.MAIN.SUM.6s = B.ISO.MAIN.SUM.6s,
                         C.ISO.MAIN.SUM.6s = C.ISO.MAIN.SUM.6s,
                         A.ISO.CONTROL.SUM.6s = A.ISO.CONTROL.SUM.6s,
                         B.ISO.CONTROL.SUM.6s = B.ISO.CONTROL.SUM.6s,
                         C.ISO.CONTROL.SUM.6s = C.ISO.CONTROL.SUM.6s,
                         D.ISO.CONTROL.SUM.6s = D.ISO.CONTROL.SUM.6s)
names(D.ISO.DF.6s)

D.ISO.DF.6s_tall = reshape(D.ISO.DF.6s, varying = c(4:10), v.names = "measure", 
                           timevar = "condition",   direction = "long")
D.ISO.DF.6s_tall = D.ISO.DF.6s_tall[order(D.ISO.DF.6s_tall$ID),] 

D.ISO.DF.6s_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 16)
D.ISO.DF.6s_tall$eventType = rep(c("main","main","main",
                                   "control","control","control","control"), times = 16)

names(D.ISO.DF.6s_tall)
D.ISO.DF.6s_tall$id = NULL
D.ISO.DF.6s_tall$condition = NULL
names(D.ISO.DF.6s_tall)
D.ISO.DF.6s_tall = D.ISO.DF.6s_tall[,c(1:3,5:6,4)]
names(D.ISO.DF.6s_tall)


D.ISO.DF.6s_tall$Age = factor(D.ISO.DF.6s_tall$Age)
D.ISO.DF.6s_tall$objects = factor(D.ISO.DF.6s_tall$objects)
D.ISO.DF.6s_tall$eventType = factor(D.ISO.DF.6s_tall$eventType)
D.ISO.DF.6s_tall$Condition = factor(D.ISO.DF.6s_tall$Condition)


##################################################################
##################################################################
###                                                            ###
### COMBINE THE DATA FOR CHILDREN IN THE BB AND ISO CONDITIONS ###
###                                                            ###
##################################################################
##################################################################

# COMBINE 5S AND 6S BB DATAFRAMES
D.DF.5s.and.6s_tall = rbind(D.BB.DF.5s_tall, D.BB.DF.6s_tall, 
                            D.ISO.DF.5s_tall, D.ISO.DF.6s_tall)
D.DF.5s.and.6s_tall$ID = rep(c(1:64),each=7) # 64 --> 32 5s and 32 6s

D.DF.5s.and.6s_tall  = D.DF.5s.and.6s_tall[order(D.DF.5s.and.6s_tall$eventType),]


#########################
#########################
### UPLOAD MODEL DATA ###
#########################
#########################
# p = 0.5
BB.A.main = rep("2",16)
BB.A.main = as.numeric(BB.A.main)

BB.B.main = rep("1",16) # 0.5+0.5
BB.B.main = as.numeric(BB.B.main)

BB.C.main = rep("1",16) # 0.5+0.5
BB.C.main = as.numeric(BB.C.main)


# control
BB.A.control = rep("1.14",16) # 0.5+0.5
BB.A.control = as.numeric(BB.A.control)


BB.B.control = rep("1.14",16) # 0.5+0.5
BB.B.control = as.numeric(BB.B.control)

BB.C.control = rep("1.14",16) # 0.5+0.5
BB.C.control = as.numeric(BB.C.control)

BB.D.control = rep("2",16) # 0.5+0.5
BB.D.control = as.numeric(BB.D.control)


# ISO
ISO.A.main = rep("0",16)
ISO.A.main = as.numeric(ISO.A.main)

ISO.B.main = rep("1.33",16) # 0.5+0.5
ISO.B.main = as.numeric(ISO.B.main)

ISO.C.main = rep("1.33",16) # 0.5+0.5
ISO.C.main = as.numeric(ISO.C.main)


# control
ISO.A.control = rep("1.14",16) # 0.5+0.5
ISO.A.control = as.numeric(ISO.A.control)


ISO.B.control = rep("1.14",16) # 0.5+0.5
ISO.B.control = as.numeric(ISO.B.control)

ISO.C.control = rep("1.14",16) # 0.5+0.5
ISO.C.control = as.numeric(ISO.C.control)

ISO.D.control = rep("0",16) # 0.5+0.5
ISO.D.control = as.numeric(ISO.D.control)



# create a dataframe that combines the variables
D.new.main = data.frame(BB.A.main = as.numeric(BB.A.main), BB.B.main = as.numeric(BB.B.main),
                        BB.C.main = as.numeric(BB.C.main), ISO.A.main = as.numeric(ISO.A.main), 
                        ISO.B.main = as.numeric(ISO.B.main), ISO.C.main = as.numeric(ISO.C.main))

D_main_tall =  reshape(D.new.main, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_main_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_main_tall$id = NULL

# organize the dataframe by ID
D_main_tall = D_main_tall[order(D_main_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_main_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_main_tall$trial = rep(c("main"), times = 96)

# create 'object' column
D_main_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_main_tall$row.names = NULL

# reorder columns 
D_main_tall = D_main_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = as.numeric(BB.A.control),
                           BB.B.control = as.numeric(BB.B.control), BB.C.control = as.numeric(BB.C.control),
                           BB.D.control = as.numeric(BB.D.control), ISO.A.control = as.numeric(ISO.A.control),
                           ISO.B.control = as.numeric(ISO.B.control), ISO.C.control = as.numeric(ISO.C.control),
                           ISO.D.control = as.numeric(ISO.D.control))

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_main_tall,D_main_tall,
               D_control_tall, D_control_tall)

D_tall = D_tall[order(D_tall$condition),]
D_tall = D_tall[order(D_tall$trial),]


################################
################################
#### MODEL-DATA COMPARISONS ####
################################
################################
# load relevant library
library(Metrics)


# model predictions
model_predictions = c(mean(BB.A.main), mean(BB.B.main), mean(BB.C.main), mean(BB.A.control), mean(BB.B.control), mean(BB.C.control), mean(BB.D.control),
                      mean(ISO.A.main), mean(ISO.B.main), mean(ISO.C.main), mean(ISO.A.control), mean(ISO.B.control), mean(ISO.C.control), mean(ISO.D.control))

# behavioral predictions 
#BB
A.BB.MAIN.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A"])-2

B.BB.MAIN.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B"])-2

C.BB.MAIN.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C"])-2


A.BB.CONTROL.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A"])-2

B.BB.CONTROL.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B"])-2

C.BB.CONTROL.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C"])-2

D.BB.CONTROL.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="BB" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D"])-2

#ISO
A.ISO.MAIN.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="A"])-2

B.ISO.MAIN.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="B"])-2

C.ISO.MAIN.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="main" & D_tall_Exp1_5and6yos$objectType=="C"])-2


A.ISO.CONTROL.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="A"])-2

B.ISO.CONTROL.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="B"])-2

C.ISO.CONTROL.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="C"])-2

D.ISO.CONTROL.SUM = as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder == "Phase 1" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D"])+
  as.numeric(D_tall_Exp1_5and6yos$choice[D_tall_Exp1_5and6yos$Condition=="ISO" & D_tall_Exp1_5and6yos$phaseOrder=="Phase 2" & D_tall_Exp1_5and6yos$trialType=="control" & D_tall_Exp1_5and6yos$objectType=="D"])-2

# behavioral predictions
behavioral_predictions = c(mean(A.BB.MAIN.SUM, na.rm=TRUE), mean(B.BB.MAIN.SUM, na.rm=TRUE), mean(C.BB.MAIN.SUM, na.rm=TRUE), mean(A.BB.CONTROL.SUM, na.rm =TRUE), 
                           mean(B.BB.CONTROL.SUM, na.rm =TRUE), mean(C.BB.CONTROL.SUM, na.rm =TRUE), mean(D.BB.CONTROL.SUM, na.rm =TRUE),
                           mean(A.ISO.MAIN.SUM, na.rm=TRUE), mean(B.ISO.MAIN.SUM, na.rm=TRUE), mean(C.ISO.MAIN.SUM, na.rm=TRUE), mean(A.ISO.CONTROL.SUM, na.rm =TRUE), 
                           mean(B.ISO.CONTROL.SUM, na.rm =TRUE), mean(C.ISO.CONTROL.SUM, na.rm =TRUE), mean(D.ISO.CONTROL.SUM, na.rm =TRUE))



#####################
# model fit indices #
#####################
caret::postResample(model_predictions, behavioral_predictions)

#   RMSE  Rsquared       MAE 
# 0.5845204 0.6609857 0.5396651


######################
######################
###### P = 0.65 ######
######################
######################
# p = 0.65
BB.A.main = rep("2",16)
BB.A.main = as.numeric(BB.A.main)

BB.B.main = rep("1.3",16) 
BB.B.main = as.numeric(BB.B.main)

BB.C.main = rep("1.3",16) 
BB.C.main = as.numeric(BB.C.main)


# control
BB.A.control = rep("1.3",16) 
BB.A.control = as.numeric(BB.A.control)


BB.B.control = rep("1.3",16) 
BB.B.control = as.numeric(BB.B.control)

BB.C.control = rep("1.3",16) 
BB.C.control = as.numeric(BB.C.control)

BB.D.control = rep("2",16)
BB.D.control = as.numeric(BB.D.control)


# ISO
ISO.A.main = rep("0",16)
ISO.A.main = as.numeric(ISO.A.main)

ISO.B.main = rep("1.3",16) 
ISO.B.main = as.numeric(ISO.B.main)

ISO.C.main = rep("1.3",16) 
ISO.C.main = as.numeric(ISO.C.main)


# control
ISO.A.control = rep("1.3",16) 
ISO.A.control = as.numeric(ISO.A.control)


ISO.B.control = rep("1.3",16) 
ISO.B.control = as.numeric(ISO.B.control)

ISO.C.control = rep("1.3",16) 
ISO.C.control = as.numeric(ISO.C.control)

ISO.D.control = rep("0",16) 
ISO.D.control = as.numeric(ISO.D.control)



# create a dataframe that combines the variables
D.new.main = data.frame(BB.A.main = as.numeric(BB.A.main), BB.B.main = as.numeric(BB.B.main),
                        BB.C.main = as.numeric(BB.C.main), ISO.A.main = as.numeric(ISO.A.main), 
                        ISO.B.main = as.numeric(ISO.B.main), ISO.C.main = as.numeric(ISO.C.main))

D_main_tall =  reshape(D.new.main, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_main_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_main_tall$id = NULL

# organize the dataframe by ID
D_main_tall = D_main_tall[order(D_main_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_main_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_main_tall$trial = rep(c("main"), times = 96)

# create 'object' column
D_main_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_main_tall$row.names = NULL

# reorder columns 
D_main_tall = D_main_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = as.numeric(BB.A.control),
                           BB.B.control = as.numeric(BB.B.control), BB.C.control = as.numeric(BB.C.control),
                           BB.D.control = as.numeric(BB.D.control), ISO.A.control = as.numeric(ISO.A.control),
                           ISO.B.control = as.numeric(ISO.B.control), ISO.C.control = as.numeric(ISO.C.control),
                           ISO.D.control = as.numeric(ISO.D.control))

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_main_tall,D_main_tall,
               D_control_tall, D_control_tall)

D_tall = D_tall[order(D_tall$condition),]
D_tall = D_tall[order(D_tall$trial),]


################################
################################
#### MODEL-DATA COMPARISONS ####
################################
################################

# model predictions
model_predictions = c(mean(BB.A.main), mean(BB.B.main), mean(BB.C.main), mean(BB.A.control), mean(BB.B.control), mean(BB.C.control), mean(BB.D.control),
                      mean(ISO.A.main), mean(ISO.B.main), mean(ISO.C.main), mean(ISO.A.control), mean(ISO.B.control), mean(ISO.C.control), mean(ISO.D.control))



#####################
# model fit indices #
#####################
caret::postResample(model_predictions, behavioral_predictions)

#     RMSE  Rsquared       MAE 
# 0.3862931 0.7929145 0.3376243




######################
######################
###### P = 0.80 ######
######################
######################
# p = 0.80
BB.A.main = rep("2",16)
BB.A.main = as.numeric(BB.A.main)

BB.B.main = rep("1.6",16) 
BB.B.main = as.numeric(BB.B.main)

BB.C.main = rep("1.6",16) 
BB.C.main = as.numeric(BB.C.main)


# control
BB.A.control = rep("1.6",16) 
BB.A.control = as.numeric(BB.A.control)


BB.B.control = rep("1.6",16) 
BB.B.control = as.numeric(BB.B.control)

BB.C.control = rep("1.6",16) 
BB.C.control = as.numeric(BB.C.control)

BB.D.control = rep("2",16)
BB.D.control = as.numeric(BB.D.control)


# ISO
ISO.A.main = rep("0",16)
ISO.A.main = as.numeric(ISO.A.main)

ISO.B.main = rep("1.6",16) 
ISO.B.main = as.numeric(ISO.B.main)

ISO.C.main = rep("1.6",16) 
ISO.C.main = as.numeric(ISO.C.main)


# control
ISO.A.control = rep("1.6",16) 
ISO.A.control = as.numeric(ISO.A.control)


ISO.B.control = rep("1.6",16) 
ISO.B.control = as.numeric(ISO.B.control)

ISO.C.control = rep("1.6",16) 
ISO.C.control = as.numeric(ISO.C.control)

ISO.D.control = rep("0",16) 
ISO.D.control = as.numeric(ISO.D.control)



# create a dataframe that combines the variables
D.new.main = data.frame(BB.A.main = as.numeric(BB.A.main), BB.B.main = as.numeric(BB.B.main),
                        BB.C.main = as.numeric(BB.C.main), ISO.A.main = as.numeric(ISO.A.main), 
                        ISO.B.main = as.numeric(ISO.B.main), ISO.C.main = as.numeric(ISO.C.main))

D_main_tall =  reshape(D.new.main, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_main_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_main_tall$id = NULL

# organize the dataframe by ID
D_main_tall = D_main_tall[order(D_main_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_main_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_main_tall$trial = rep(c("main"), times = 96)

# create 'object' column
D_main_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_main_tall$row.names = NULL

# reorder columns 
D_main_tall = D_main_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = as.numeric(BB.A.control),
                           BB.B.control = as.numeric(BB.B.control), BB.C.control = as.numeric(BB.C.control),
                           BB.D.control = as.numeric(BB.D.control), ISO.A.control = as.numeric(ISO.A.control),
                           ISO.B.control = as.numeric(ISO.B.control), ISO.C.control = as.numeric(ISO.C.control),
                           ISO.D.control = as.numeric(ISO.D.control))

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_main_tall,D_main_tall,
               D_control_tall, D_control_tall)

D_tall = D_tall[order(D_tall$condition),]
D_tall = D_tall[order(D_tall$trial),]


################################
################################
#### MODEL-DATA COMPARISONS ####
################################
################################

# model predictions
model_predictions = c(mean(BB.A.main), mean(BB.B.main), mean(BB.C.main), mean(BB.A.control), mean(BB.B.control), mean(BB.C.control), mean(BB.D.control),
                      mean(ISO.A.main), mean(ISO.B.main), mean(ISO.C.main), mean(ISO.A.control), mean(ISO.B.control), mean(ISO.C.control), mean(ISO.D.control))



#####################
# model fit indices #
#####################
caret::postResample(model_predictions, behavioral_predictions)

#      RMSE  Rsquared       MAE 
# 0.2921508 0.8345130 0.2087566




######################
######################
###### P = 0.95 ######
######################
######################
# p = 0.95
BB.A.main = rep("2",16)
BB.A.main = as.numeric(BB.A.main)

BB.B.main = rep("1.9",16) 
BB.B.main = as.numeric(BB.B.main)

BB.C.main = rep("1.9",16) 
BB.C.main = as.numeric(BB.C.main)


# control
BB.A.control = rep("1.9",16) 
BB.A.control = as.numeric(BB.A.control)


BB.B.control = rep("1.9",16) 
BB.B.control = as.numeric(BB.B.control)

BB.C.control = rep("1.9",16) 
BB.C.control = as.numeric(BB.C.control)

BB.D.control = rep("2",16)
BB.D.control = as.numeric(BB.D.control)


# ISO
ISO.A.main = rep("0",16)
ISO.A.main = as.numeric(ISO.A.main)

ISO.B.main = rep("1.9",16) 
ISO.B.main = as.numeric(ISO.B.main)

ISO.C.main = rep("1.9",16) 
ISO.C.main = as.numeric(ISO.C.main)


# control
ISO.A.control = rep("1.9",16) 
ISO.A.control = as.numeric(ISO.A.control)


ISO.B.control = rep("1.9",16) 
ISO.B.control = as.numeric(ISO.B.control)

ISO.C.control = rep("1.9",16) 
ISO.C.control = as.numeric(ISO.C.control)

ISO.D.control = rep("0",16) 
ISO.D.control = as.numeric(ISO.D.control)



# create a dataframe that combines the variables
D.new.main = data.frame(BB.A.main = as.numeric(BB.A.main), BB.B.main = as.numeric(BB.B.main),
                        BB.C.main = as.numeric(BB.C.main), ISO.A.main = as.numeric(ISO.A.main), 
                        ISO.B.main = as.numeric(ISO.B.main), ISO.C.main = as.numeric(ISO.C.main))

D_main_tall =  reshape(D.new.main, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_main_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_main_tall$id = NULL

# organize the dataframe by ID
D_main_tall = D_main_tall[order(D_main_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_main_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_main_tall$trial = rep(c("main"), times = 96)

# create 'object' column
D_main_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_main_tall$row.names = NULL

# reorder columns 
D_main_tall = D_main_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = as.numeric(BB.A.control),
                           BB.B.control = as.numeric(BB.B.control), BB.C.control = as.numeric(BB.C.control),
                           BB.D.control = as.numeric(BB.D.control), ISO.A.control = as.numeric(ISO.A.control),
                           ISO.B.control = as.numeric(ISO.B.control), ISO.C.control = as.numeric(ISO.C.control),
                           ISO.D.control = as.numeric(ISO.D.control))

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_main_tall,D_main_tall,
               D_control_tall, D_control_tall)

D_tall = D_tall[order(D_tall$condition),]
D_tall = D_tall[order(D_tall$trial),]


################################
################################
#### MODEL-DATA COMPARISONS ####
################################
################################

# model predictions
model_predictions = c(mean(BB.A.main), mean(BB.B.main), mean(BB.C.main), mean(BB.A.control), mean(BB.B.control), mean(BB.C.control), mean(BB.D.control),
                      mean(ISO.A.main), mean(ISO.B.main), mean(ISO.C.main), mean(ISO.A.control), mean(ISO.B.control), mean(ISO.C.control), mean(ISO.D.control))



#####################
# model fit indices #
#####################

caret::postResample(model_predictions, behavioral_predictions)

#       RMSE  Rsquared       MAE 
# 0.3873670 0.8087243 0.3267641 



######################
######################
###### P = 1 ######
######################
######################
# p = 1
BB.A.main = rep("2",16)
BB.A.main = as.numeric(BB.A.main)

BB.B.main = rep("2",16) 
BB.B.main = as.numeric(BB.B.main)

BB.C.main = rep("2",16) 
BB.C.main = as.numeric(BB.C.main)


# control
BB.A.control = rep("2",16) 
BB.A.control = as.numeric(BB.A.control)


BB.B.control = rep("2",16) 
BB.B.control = as.numeric(BB.B.control)

BB.C.control = rep("2",16) 
BB.C.control = as.numeric(BB.C.control)

BB.D.control = rep("2",16)
BB.D.control = as.numeric(BB.D.control)


# ISO
ISO.A.main = rep("0",16)
ISO.A.main = as.numeric(ISO.A.main)

ISO.B.main = rep("2",16) 
ISO.B.main = as.numeric(ISO.B.main)

ISO.C.main = rep("2",16) 
ISO.C.main = as.numeric(ISO.C.main)


# control
ISO.A.control = rep("2",16) 
ISO.A.control = as.numeric(ISO.A.control)


ISO.B.control = rep("2",16) 
ISO.B.control = as.numeric(ISO.B.control)

ISO.C.control = rep("2",16) 
ISO.C.control = as.numeric(ISO.C.control)

ISO.D.control = rep("0",16) 
ISO.D.control = as.numeric(ISO.D.control)



# create a dataframe that combines the variables
D.new.main = data.frame(BB.A.main = as.numeric(BB.A.main), BB.B.main = as.numeric(BB.B.main),
                        BB.C.main = as.numeric(BB.C.main), ISO.A.main = as.numeric(ISO.A.main), 
                        ISO.B.main = as.numeric(ISO.B.main), ISO.C.main = as.numeric(ISO.C.main))

D_main_tall =  reshape(D.new.main, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_main_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_main_tall$id = NULL

# organize the dataframe by ID
D_main_tall = D_main_tall[order(D_main_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_main_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_main_tall$trial = rep(c("main"), times = 96)

# create 'object' column
D_main_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_main_tall$row.names = NULL

# reorder columns 
D_main_tall = D_main_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = as.numeric(BB.A.control),
                           BB.B.control = as.numeric(BB.B.control), BB.C.control = as.numeric(BB.C.control),
                           BB.D.control = as.numeric(BB.D.control), ISO.A.control = as.numeric(ISO.A.control),
                           ISO.B.control = as.numeric(ISO.B.control), ISO.C.control = as.numeric(ISO.C.control),
                           ISO.D.control = as.numeric(ISO.D.control))

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_main_tall,D_main_tall,
               D_control_tall, D_control_tall)

D_tall = D_tall[order(D_tall$condition),]
D_tall = D_tall[order(D_tall$trial),]


################################
################################
#### MODEL-DATA COMPARISONS ####
################################
################################

# model predictions
model_predictions = c(mean(BB.A.main), mean(BB.B.main), mean(BB.C.main), mean(BB.A.control), mean(BB.B.control), mean(BB.C.control), mean(BB.D.control),
                      mean(ISO.A.main), mean(ISO.B.main), mean(ISO.C.main), mean(ISO.A.control), mean(ISO.B.control), mean(ISO.C.control), mean(ISO.D.control))


#####################
# model fit indices #
#####################
caret::postResample(model_predictions, behavioral_predictions)
#     RMSE  Rsquared       MAE 
# 0.4474278 0.7918340 0.3981927

