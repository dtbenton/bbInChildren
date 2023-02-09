# load all relevant libraries
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
library(openxlsx)
library(BFpack)
options(scipen=9999)

# DATA CLEAN UP AND RESTRUCTURING #
D = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# remove E columns 
D$CTRL_1__E = NULL
D$CTRL_2__E = NULL

names(D)


D_tall =  reshape(D, varying = c(9:22), v.names = "measure", 
                  timevar = "condition",   direction = "long")

D_tall = D_tall[order(D_tall$ID),] # order the data frame in terms of participant ID;
# to avoid wonky things happening and to save yourself 
# a full-day headache in the future, reorder by ID
# immediately after reshaping the dataframe.


D_tall$trialType = rep(c("control","control","control","control","control","control","control",
                         "control","main","main",
                         "main","main","main","main"), times = 94)

D_tall$testPhase = rep(c("first","first","first","first","second","second","second","second",
                         "first","first","first","second","second","second"), times = 94)

D_tall$objectType = rep(c("A","B","C","D",
                          "A","B","C","D",
                          "A","B","C",
                          "A","B","C"), times = 94)

D_tall$phaseOrder = rep(c("Phase 1","Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2","Phase 2",
                          "Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2"), times = 94)




# remove unnecessary columns
D_tall$id = NULL


# CHANGE SOME OF THE COLUMN NAMES
names(D_tall)
colnames(D_tall)[which(names(D_tall) == "AGE.Y.")] <- "Age"
colnames(D_tall)[which(names(D_tall) == "SEX")] <- "Sex"
colnames(D_tall)[which(names(D_tall) == "CONDITION")] <- "Experiment"
colnames(D_tall)[which(names(D_tall) == "BB.IS")] <- "Condition"
colnames(D_tall)[which(names(D_tall) == "SUBCONDITION")] <- "SubCondition"
colnames(D_tall)[which(names(D_tall) == "VIDORDER")] <- "Vidorder"
colnames(D_tall)[which(names(D_tall) == "PRETEST")] <- "Pretest"
colnames(D_tall)[which(names(D_tall) == "measure")] <- "choice"

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

D_tall$Experiment = revalue(x = as.factor(D_tall$Experiment), 
                            c("0" = "Experiment 1", "1"="Experiment 1",
                              "2" = "Experiment 2"))

D_tall$Age = as.factor(D_tall$Age)
D_tall$testPhase = as.factor(D_tall$testPhase)
D_tall$objectType = as.factor(D_tall$objectType)
D_tall$trialType = as.factor(D_tall$trialType)
D_tall$Experiment = as.factor(D_tall$Experiment)

# REODRDER COLUMNS
D_tall$condition = NULL
names(D_tall)
D_tall = as.data.frame(D_tall[,c(1:3,6,5,7,4,10,11,12,13,9,8)])
fix(D_tall)


# subset dataframes by experiments
D_tall_Exp1_5and6yos = subset(D_tall, ! Experiment %in% c("Experiment 2"))
D_tall_Exp1_5and6yos = subset(D_tall, ! Age %in% c("4"))
D_tall_Exp1_5and6yos$Age = factor(D_tall_Exp1_5and6yos$Age)
D_tall_Exp1_5and6yos$Experiment = factor(D_tall_Exp1_5and6yos$Experiment)

dim(D_tall_Exp1_5and6yos)
D_tall_Exp1_5and6yos = D_tall_Exp1_5and6yos[1:1204,]


# get the number of participants in each condition
table(D_tall_Exp1_5and6yos$Condition)/14


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
D.BB.DF.5s = data.frame(ID = c(1:32), Age = rep(c("5"),32), Condition = rep(c("BB"),32),
                        A.BB.MAIN.SUM.5s = A.BB.MAIN.SUM.5s, 
                        B.BB.MAIN.SUM.5s = B.BB.MAIN.SUM.5s,
                        C.BB.MAIN.SUM.5s = C.BB.MAIN.SUM.5s,
                        A.BB.CONTROL.SUM.5s = A.BB.CONTROL.SUM.5s,
                        B.BB.CONTROL.SUM.5s = B.BB.CONTROL.SUM.5s,
                        C.BB.CONTROL.SUM.5s = C.BB.CONTROL.SUM.5s,
                        D.BB.CONTROL.SUM.5s = D.BB.CONTROL.SUM.5s)
names(D.BB.DF.5s)

D.BB.DF.5s_tall = reshape(D.BB.DF.5s, varying = c(4:10), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D.BB.DF.5s_tall = D.BB.DF.5s_tall[order(D.BB.DF.5s_tall$ID),] 

D.BB.DF.5s_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 32)
D.BB.DF.5s_tall$eventType = rep(c("main","main","main",
                                  "control","control","control","control"), times = 32)

names(D.BB.DF.5s_tall)
D.BB.DF.5s_tall$id = NULL
D.BB.DF.5s_tall$condition = NULL
names(D.BB.DF.5s_tall)
D.BB.DF.5s_tall = D.BB.DF.5s_tall[,c(1:3,5:6,4)]
names(D.BB.DF.5s_tall)

D.BB.DF.5s_tall$Age = factor(D.BB.DF.5s_tall$Age)
D.BB.DF.5s_tall$objects = factor(D.BB.DF.5s_tall$objects)
D.BB.DF.5s_tall$eventType = factor(D.BB.DF.5s_tall$eventType)
D.BB.DF.5s_tall$Condition = factor(D.BB.DF.5s_tall$Condition)


# 6s data frame
D.BB.DF.6s = data.frame(ID = c(1:24), Age = rep(c("6"),24), Condition = rep(c("BB"),24),
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

D.BB.DF.6s_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 24)
D.BB.DF.6s_tall$eventType = rep(c("main","main","main",
                                  "control","control","control","control"), times = 24)

names(D.BB.DF.6s_tall)
D.BB.DF.6s_tall$id = NULL
D.BB.DF.6s_tall$condition = NULL
names(D.BB.DF.6s_tall)
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
D.ISO.DF.5s = data.frame(ID = c(1:5), Age = rep(c("5"),5), Condition = rep(c("ISO"),5),
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

D.ISO.DF.5s_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 5)
D.ISO.DF.5s_tall$eventType = rep(c("main","main","main",
                                   "control","control","control","control"), times = 5)

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
D.ISO.DF.6s = data.frame(ID = c(1:25), Age = rep(c("6"),25), Condition = rep(c("ISO"),25),
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

D.ISO.DF.6s_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 25)
D.ISO.DF.6s_tall$eventType = rep(c("main","main","main",
                                   "control","control","control","control"), times = 25)

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
D.DF.5s.and.6s_tall$ID = rep(c(1:86),each=7)


# main analysis
lm.fit = lm(measure~(Age+Condition+objects+eventType)^4, data = D.DF.5s.and.6s_tall, 
              na.action=na.exclude)
Anova(lm.fit)


# BB follow-up analyses
lm.fit.4 = lm(measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control"]~
                objects[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control"], data = D.DF.5s.and.6s_tall, 
              na.action=na.exclude)
Anova(lm.fit.4)

# A
A = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)

# B
B = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)

# C
C = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)

# D
D = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="D"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="D"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="D"], na.rm=TRUE)

t.test(D,A, alternative="two.sided", paired = TRUE)
t.test(D,B, alternative="two.sided", paired = TRUE)
t.test(D,C, alternative="two.sided", paired = TRUE)



lm.fit.5 = lm(measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main"]~
                objects[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main"], data = D.DF.5s.and.6s_tall, 
              na.action=na.exclude)
Anova(lm.fit.5)

# A
A = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="A"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)

# B
B = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)

# C
C = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)


# ISO follow-up analyses
lm.fit.4 = lm(measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control"]~
                objects[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control"], data = D.DF.5s.and.6s_tall, 
              na.action=na.exclude)
Anova(lm.fit.4)

# A
A = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)

# B
B = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)

# C
C = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)

# D
D = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="D"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="D"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="D"], na.rm=TRUE)

t.test(D,A, alternative="two.sided", paired = TRUE)
t.test(D,B, alternative="two.sided", paired = TRUE)
t.test(D,C, alternative="two.sided", paired = TRUE)



lm.fit.5 = lm(measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main"]~
                objects[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main"], data = D.DF.5s.and.6s_tall, 
              na.action=na.exclude)
Anova(lm.fit.5)

# A
A = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="A"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)

# B
B = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)

# C
C = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)


########################################
########################################
# OLD AND NEW OPERATIONALIZATION OF BB #
########################################
########################################
# NEW OPERATIONALIZATION #
# BB #
# A control
A.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"], na.rm=TRUE)


# B main v B control
B.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)

B.main = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"], na.rm=TRUE)
t.test(B.main,B.control, alternative="two.sided", paired = TRUE)


# C main v C control
C.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)

C.main = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"]
mean(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)
sd(D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"], na.rm=TRUE)
t.test(C.main,C.control, alternative="two.sided", paired = TRUE)


# create data frame and add columns from the variables above 
D.new.op = data.frame(ID = c(1:56),
                      A.control = A.control, B.main = B.main, B.control = B.control,
                      C.main = C.main, C.control = C.control)
names(D.new.op)

D.new.op_tall = reshape(D.new.op, varying = c(2:6), v.names = "measure", 
                        timevar = "condition",   direction = "long")
D.new.op_tall = D.new.op_tall[order(D.new.op_tall$ID),] 

D.new.op_tall$objects = rep(c("A","B","B","C","C"), times = 56)
D.new.op_tall$eventType = rep(c("control","main","control",
                                "main","control"), times = 56)
D.new.op_tall$objects = factor(D.new.op_tall$objects)
D.new.op_tall$eventType = factor(D.new.op_tall$eventType)

# main analysis 
lm.fit.7 = lm(measure~(objects+eventType)^2, data = D.new.op_tall, 
              na.action=na.exclude)
Anova(lm.fit.7)


mean(D.new.op_tall$measure[D.new.op_tall$eventType=="main"], na.rm = TRUE)
sd(D.new.op_tall$measure[D.new.op_tall$eventType=="main"], na.rm = TRUE)

mean(D.new.op_tall$measure[D.new.op_tall$eventType=="control"], na.rm = TRUE)
sd(D.new.op_tall$measure[D.new.op_tall$eventType=="control"], na.rm = TRUE)


# OLD OPERATIONALIZATION #
# BB #
b.bb.main = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"]
c.bb.main = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"]
A.bb.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"]
B.bb.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"]
C.bb.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="BB" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"]

b.iso.main = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="B"]
c.iso.main = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="main" & D.DF.5s.and.6s_tall$objects=="C"]
A.iso.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="A"]
B.iso.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="B"]
C.iso.control = D.DF.5s.and.6s_tall$measure[D.DF.5s.and.6s_tall$Condition=="ISO" & D.DF.5s.and.6s_tall$eventType=="control" & D.DF.5s.and.6s_tall$objects=="C"]




# create data frame and add columns from the variables above 
# BB
D.old.bb = data.frame(ID = c(1:56), Condition = rep("BB",times=56),
                      B.main = b.bb.main, C.main = c.bb.main, A.control = A.bb.control,
                      B.control = B.bb.control, C.control = C.bb.control)
names(D.old.bb)

D.old.bb_tall = reshape(D.old.bb, varying = c(3:7), v.names = "measure", 
                        timevar = "condition",   direction = "long")
D.old.bb_tall = D.old.bb_tall[order(D.old.bb_tall$ID),] 
D.old.bb_tall$condition = NULL

D.old.bb_tall$objects = rep(c("B","C","A","B","C"), times = 56)
D.old.bb_tall$eventType = rep(c("main","main","control",
                                "control","control"), times = 56)
D.old.bb_tall$objects = factor(D.old.bb_tall$objects)
D.old.bb_tall$eventType = factor(D.old.bb_tall$eventType)

# ISO
D.old.iso = data.frame(ID = c(57:86), Condition = rep("ISO",times=30),
                      B.main = b.iso.main, C.main = c.iso.main, A.control = A.iso.control,
                      B.control = B.iso.control, C.control = C.iso.control)
names(D.old.iso)

D.old.iso_tall = reshape(D.old.iso, varying = c(3:7), v.names = "measure", 
                        timevar = "condition",   direction = "long")
D.old.iso_tall = D.old.iso_tall[order(D.old.iso_tall$ID),] 
D.old.iso_tall$condition = NULL

D.old.iso_tall$objects = rep(c("B","C","A","B","C"), times = 30)
D.old.iso_tall$eventType = rep(c("main","main","control",
                                "control","control"), times = 30)
D.old.iso_tall$objects = factor(D.old.iso_tall$objects)
D.old.iso_tall$eventType = factor(D.old.iso_tall$eventType)


# combine the BB and ISO dataframes
D.old.BB.ISO = rbind(D.old.bb_tall, D.old.iso_tall)
names(D.old.BB.ISO)
D.old.BB.ISO$id = NULL
names(D.old.BB.ISO)


# main analysis 
lm.fit.8 = lm(measure~(objects+eventType)^2, data = D.old.BB.ISO, 
              na.action=na.exclude)
Anova(lm.fit.8)


mean(D.old.BB.ISO$measure[D.old.BB.ISO$eventType=="main"], na.rm = TRUE)
sd(D.old.BB.ISO$measure[D.old.BB.ISO$eventType=="main"], na.rm = TRUE)

mean(D.old.BB.ISO$measure[D.old.BB.ISO$eventType=="control"], na.rm = TRUE)
sd(D.old.BB.ISO$measure[D.old.BB.ISO$eventType=="control"], na.rm = TRUE)

# Figure
condition_barplot = ggplot(D.DF.5s.and.6s_tall, aes(eventType, measure, fill = objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  facet_wrap(~Condition) +
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) 