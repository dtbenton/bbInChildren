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


names(D)

# add ID column
D$ID = c(1:20)
D$SUBJECTID = NULL

# reorder D
D = D[,c(20,1:19)]

D_tall =  reshape(D, varying = c(7:20), v.names = "measure", 
                  timevar = "condition",   direction = "long")

D_tall = D_tall[order(D_tall$ID),] # order the data frame in terms of participant ID;
# to avoid wonky things happening and to save yourself 
# a full-day headache in the future, reorder by ID
# immediately after reshaping the dataframe.


D_tall$trialType = rep(c("control","control","control","control","control","control","control",
                         "control","main","main",
                         "main","main","main","main"), times = 20)

D_tall$testPhase = rep(c("first","first","first","first","second","second","second","second",
                         "first","first","first","second","second","second"), times = 20)

D_tall$objectType = rep(c("A","B","C","D",
                          "A","B","C","D",
                          "A","B","C",
                          "A","B","C"), times = 20)

D_tall$phaseOrder = rep(c("Phase 1","Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2","Phase 2",
                          "Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2"), times = 20)




# remove unnecessary columns
D_tall$id = NULL


# CHANGE SOME OF THE COLUMN NAMES
names(D_tall)
colnames(D_tall)[which(names(D_tall) == "AGE.Y.")] <- "Age"
colnames(D_tall)[which(names(D_tall) == "SEX")] <- "Sex"
colnames(D_tall)[which(names(D_tall) == "BB.IS")] <- "Condition"
colnames(D_tall)[which(names(D_tall) == "VIDORDER")] <- "Vidorder"
colnames(D_tall)[which(names(D_tall) == "PRETEST")] <- "Pretest"
colnames(D_tall)[which(names(D_tall) == "measure")] <- "choice"

# colnames(D_tall)[which(names(D_tall) == "SUBCONDITION")] <- "Subcondition"
names(D_tall)


# reorder D_tall
D_tall = D_tall[,c(1:7,9:12,8)]
names(D_tall)
fix(D_tall)

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




# get the number of participants in each condition
table(D_tall$Condition)/18


##########################################################
##########################################################
###                                                    ###
### ANALYSIS FOR CHILDREN ASSIGNED TO THE BB CONDITION ###
###                                                    ###
##########################################################
##########################################################

# BB MAIN 
A.BB.MAIN.SUM = D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="A"]+
  D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="A"]
length(A.BB.MAIN.SUM)
mean(A.BB.MAIN.SUM, na.rm=TRUE)


B.BB.MAIN.SUM = D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="B"]+
  D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="B"]
length(B.BB.MAIN.SUM)
mean(B.BB.MAIN.SUM, na.rm=TRUE)


C.BB.MAIN.SUM = D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="C"]+
  D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="C"]
length(C.BB.MAIN.SUM)
mean(C.BB.MAIN.SUM, na.rm=TRUE)




# BB CONTROL
A.BB.CONTROL.SUM = D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="A"]+
  D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="A"]
length(A.BB.CONTROL.SUM)
mean(A.BB.CONTROL.SUM, na.rm=TRUE)


B.BB.CONTROL.SUM = D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="B"]+
  D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="B"]
length(B.BB.CONTROL.SUM)
mean(B.BB.CONTROL.SUM, na.rm=TRUE)


C.BB.CONTROL.SUM = D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="C"]+
  D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="C"]
length(C.BB.CONTROL.SUM)
mean(C.BB.CONTROL.SUM, na.rm=TRUE)

D.BB.CONTROL.SUM = D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="D"]+
  D_tall$choice[D_tall$Condition=="BB" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="D"]
length(D.BB.CONTROL.SUM)
mean(D.BB.CONTROL.SUM, na.rm=TRUE)


# 5s data frame
D.BB.DF = data.frame(ID = c(1:18), Condition = rep(c("BB"),18),
                        A.BB.MAIN.SUM = A.BB.MAIN.SUM, 
                        B.BB.MAIN.SUM = B.BB.MAIN.SUM,
                        C.BB.MAIN.SUM = C.BB.MAIN.SUM,
                        A.BB.CONTROL.SUM = A.BB.CONTROL.SUM,
                        B.BB.CONTROL.SUM = B.BB.CONTROL.SUM,
                        C.BB.CONTROL.SUM = C.BB.CONTROL.SUM,
                        D.BB.CONTROL.SUM = D.BB.CONTROL.SUM)
names(D.BB.DF)

D.BB.DF_tall = reshape(D.BB.DF, varying = c(3:9), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D.BB.DF_tall = D.BB.DF_tall[order(D.BB.DF_tall$ID),] 

D.BB.DF_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 18)
D.BB.DF_tall$eventType = rep(c("main","main","main",
                                  "control","control","control","control"), times = 18)

names(D.BB.DF_tall)
D.BB.DF_tall$id = NULL
D.BB.DF_tall$condition = NULL
names(D.BB.DF_tall)
D.BB.DF_tall = D.BB.DF_tall[,c(1:2,4:5,3)]
names(D.BB.DF_tall)

D.BB.DF_tall$objects = factor(D.BB.DF_tall$objects)
D.BB.DF_tall$eventType = factor(D.BB.DF_tall$eventType)
D.BB.DF_tall$Condition = factor(D.BB.DF_tall$Condition)



###########################################################
###########################################################
###                                                     ###
### ANALYSIS FOR CHILDREN ASSIGNED TO THE ISO CONDITION ###
###                                                     ###
###########################################################
###########################################################
# ISO MAIN

A.ISO.MAIN.SUM = D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="A"]+
  D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="A"]
length(A.ISO.MAIN.SUM)
mean(A.ISO.MAIN.SUM, na.rm=TRUE)


B.ISO.MAIN.SUM = D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="B"]+
  D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="B"]
length(B.ISO.MAIN.SUM)
mean(B.ISO.MAIN.SUM, na.rm=TRUE)


C.ISO.MAIN.SUM = D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="main" & D_tall$objectType=="C"]+
  D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="main" & D_tall$objectType=="C"]
length(C.ISO.MAIN.SUM)
mean(C.ISO.MAIN.SUM, na.rm=TRUE)



# ISO CONTROL
A.ISO.MAIN.SUM = D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="A"]+
  D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="A"]
length(A.ISO.MAIN.SUM)
mean(A.ISO.MAIN.SUM, na.rm=TRUE)



B.ISO.MAIN.SUM = D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="B"]+
  D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="B"]
length(B.ISO.MAIN.SUM)
mean(B.ISO.MAIN.SUM, na.rm=TRUE)



C.ISO.MAIN.SUM = D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="C"]+
  D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="C"]
length(C.ISO.MAIN.SUM)
mean(C.ISO.MAIN.SUM, na.rm=TRUE)


D.ISO.MAIN.SUM = D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder == "Phase 1" & D_tall$trialType=="control" & D_tall$objectType=="D"]+
  D_tall$choice[D_tall$Condition=="ISO" & D_tall$phaseOrder=="Phase 2" & D_tall$trialType=="control" & D_tall$objectType=="D"]
length(D.ISO.MAIN.SUM)
mean(D.ISO.MAIN.SUM, na.rm=TRUE)



# 5s data frame
D.ISO.DF = data.frame(ID = c(1:2), Condition = rep(c("ISO"),2),
                         A.ISO.MAIN.SUM = A.ISO.MAIN.SUM, 
                         B.ISO.MAIN.SUM = B.ISO.MAIN.SUM,
                         C.ISO.MAIN.SUM = C.ISO.MAIN.SUM,
                         A.ISO.MAIN.SUM = A.ISO.MAIN.SUM,
                         B.ISO.MAIN.SUM = B.ISO.MAIN.SUM,
                         C.ISO.MAIN.SUM = C.ISO.MAIN.SUM,
                         D.ISO.MAIN.SUM = D.ISO.MAIN.SUM)
names(D.ISO.DF)

D.ISO.DF_tall = reshape(D.ISO.DF, varying = c(3:9), v.names = "measure", 
                           timevar = "condition",   direction = "long")
D.ISO.DF_tall = D.ISO.DF_tall[order(D.ISO.DF_tall$ID),] 

D.ISO.DF_tall$objects = rep(c("A","B","C","A","B","C","D"), times = 2)
D.ISO.DF_tall$eventType = rep(c("main","main","main",
                                   "control","control","control","control"), times = 2)

names(D.ISO.DF_tall)
D.ISO.DF_tall$id = NULL
D.ISO.DF_tall$condition = NULL
names(D.ISO.DF_tall)
D.ISO.DF_tall = D.ISO.DF_tall[,c(1:2,4:5,3)]
names(D.ISO.DF_tall)


D.ISO.DF_tall$objects = factor(D.ISO.DF_tall$objects)
D.ISO.DF_tall$eventType = factor(D.ISO.DF_tall$eventType)
D.ISO.DF_tall$Condition = factor(D.ISO.DF_tall$Condition)




##################################################################
##################################################################
###                                                            ###
### COMBINE THE DATA FOR CHILDREN IN THE BB AND ISO CONDITIONS ###
###                                                            ###
##################################################################
##################################################################


# COMBINE 5S AND 6S BB DATAFRAMES
D.DF.BB.IS = rbind(D.BB.DF_tall, 
                            D.ISO.DF_tall)
dim(D.DF.BB.IS)
D.DF.BB.IS$ID = rep(c(1:20),each=7)


# main analysis
lm.fit = lm(measure~(Condition+objects+eventType)^3, data = D.DF.BB.IS, 
              na.action=na.exclude)
Anova(lm.fit)


# BB follow-up analyses
lm.fit.4 = lm(measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control"]~
                objects[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control"], data = D.DF.BB.IS, 
              na.action=na.exclude)
Anova(lm.fit.4)

# A
A = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)

# B
B = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)

# C
C = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)

# D
D = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="D"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="D"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="D"], na.rm=TRUE)

t.test(D,A, alternative="two.sided", paired = TRUE)
t.test(D,B, alternative="two.sided", paired = TRUE)
t.test(D,C, alternative="two.sided", paired = TRUE)



lm.fit.5 = lm(measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main"]~
                objects[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main"], data = D.DF.BB.IS, 
              na.action=na.exclude)
Anova(lm.fit.5)

# A
A = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="A"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)

# B
B = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)

# C
C = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)


# ISO follow-up analyses
lm.fit.4 = lm(measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control"]~
                objects[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control"], data = D.DF.BB.IS, 
              na.action=na.exclude)
Anova(lm.fit.4)

# A
A = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)

# B
B = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)

# C
C = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)

# D
D = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="D"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="D"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="D"], na.rm=TRUE)

t.test(D,A, alternative="two.sided", paired = TRUE)
t.test(D,B, alternative="two.sided", paired = TRUE)
t.test(D,C, alternative="two.sided", paired = TRUE)



lm.fit.5 = lm(measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main"]~
                objects[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main"], data = D.DF.BB.IS, 
              na.action=na.exclude)
Anova(lm.fit.5)

# A
A = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="A"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)

# B
B = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)

# C
C = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)

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
A.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"], na.rm=TRUE)


# B main v B control
B.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)

B.main = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"], na.rm=TRUE)
t.test(B.main,B.control, alternative="two.sided", paired = TRUE)


# C main v C control
C.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)

C.main = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"]
mean(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)
sd(D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"], na.rm=TRUE)
t.test(C.main,C.control, alternative="two.sided", paired = TRUE)


# create data frame and add columns from the variables above 
D.new.op = data.frame(ID = c(1:18),
                      A.control = A.control, B.main = B.main, B.control = B.control,
                      C.main = C.main, C.control = C.control)
names(D.new.op)

D.new.op_tall = reshape(D.new.op, varying = c(2:6), v.names = "measure", 
                        timevar = "condition",   direction = "long")
D.new.op_tall = D.new.op_tall[order(D.new.op_tall$ID),] 

D.new.op_tall$objects = rep(c("A","B","B","C","C"), times = 18)
D.new.op_tall$eventType = rep(c("control","main","control",
                                "main","control"), times = 18)
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
b.bb.main = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"]
c.bb.main = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"]
A.bb.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"]
B.bb.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"]
C.bb.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="BB" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"]

b.iso.main = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="B"]
c.iso.main = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="main" & D.DF.BB.IS$objects=="C"]
A.iso.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="A"]
B.iso.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="B"]
C.iso.control = D.DF.BB.IS$measure[D.DF.BB.IS$Condition=="ISO" & D.DF.BB.IS$eventType=="control" & D.DF.BB.IS$objects=="C"]




# create data frame and add columns from the variables above 
# BB
D.old.bb = data.frame(ID = c(1:18), Condition = rep("BB",times=18),
                      B.main = b.bb.main, C.main = c.bb.main, A.control = A.bb.control,
                      B.control = B.bb.control, C.control = C.bb.control)
names(D.old.bb)

D.old.bb_tall = reshape(D.old.bb, varying = c(3:7), v.names = "measure", 
                        timevar = "condition",   direction = "long")
D.old.bb_tall = D.old.bb_tall[order(D.old.bb_tall$ID),] 
names(D.old.bb_tall)
D.old.bb_tall$condition = NULL

D.old.bb_tall$objects = rep(c("B","C","A","B","C"), times = 18)
D.old.bb_tall$eventType = rep(c("main","main","control",
                                "control","control"), times = 18)
D.old.bb_tall$objects = factor(D.old.bb_tall$objects)
D.old.bb_tall$eventType = factor(D.old.bb_tall$eventType)

# ISO
D.old.iso = data.frame(ID = c(1:2), Condition = rep("ISO",times=2),
                      B.main = b.iso.main, C.main = c.iso.main, A.control = A.iso.control,
                      B.control = B.iso.control, C.control = C.iso.control)
names(D.old.iso)

D.old.iso_tall = reshape(D.old.iso, varying = c(3:7), v.names = "measure", 
                        timevar = "condition",   direction = "long")
D.old.iso_tall = D.old.iso_tall[order(D.old.iso_tall$ID),] 
D.old.iso_tall$condition = NULL

D.old.iso_tall$objects = rep(c("B","C","A","B","C"), times = 2)
D.old.iso_tall$eventType = rep(c("main","main","control",
                                "control","control"), times = 2)
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
condition_barplot = ggplot(D.DF.BB.IS, aes(eventType, measure, fill = objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
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
        panel.background = element_blank(), axis.line = element_line(colour = "black"))