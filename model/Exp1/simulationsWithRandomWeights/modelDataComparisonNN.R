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
                         "control","experimental","experimental",
                         "experimental","experimental","experimental","experimental"), times = 64)

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
  } else if(D_tall$choice[i]==2) {
    D_tall$choices[i]=0
  }
  else {
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



# behavioral predictions 
#BB
A.BB.MAIN.SUM = mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="A"], na.rm=TRUE)

B.BB.MAIN.SUM = mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)

C.BB.MAIN.SUM = mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)


A.BB.CONTROL.SUM = mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)

B.BB.CONTROL.SUM = mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)

C.BB.CONTROL.SUM = mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)

D.BB.CONTROL.SUM = mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="D"], na.rm=TRUE)


#ISO
A.ISO.MAIN.SUM = mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="A"], na.rm=TRUE)

B.ISO.MAIN.SUM = mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)

C.ISO.MAIN.SUM = mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)


A.ISO.CONTROL.SUM = mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)

B.ISO.CONTROL.SUM = mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)

C.ISO.CONTROL.SUM = mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)

D.ISO.CONTROL.SUM = mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="D"], na.rm=TRUE)


# behavioral predictions
behavioral_predictions = c(A.BB.MAIN.SUM, B.BB.MAIN.SUM, C.BB.MAIN.SUM, A.BB.CONTROL.SUM, B.BB.CONTROL.SUM, C.BB.CONTROL.SUM, D.BB.CONTROL.SUM,
                           A.ISO.MAIN.SUM, B.ISO.MAIN.SUM, C.ISO.MAIN.SUM, A.ISO.CONTROL.SUM, B.ISO.CONTROL.SUM, C.ISO.CONTROL.SUM, D.ISO.CONTROL.SUM)


#########################
#########################
### UPLOAD MODEL DATA ###
#########################
#########################
# load data
D.2 = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# get names of columns
names(D)

# BB trial objects #
# main
BB.A.main = D.2$choice[D.2$object=="A" & D.2$trial=="main" & D.2$condition=="BB"]

BB.B.main = D.2$choice[D.2$object=="B" & D.2$trial=="main" & D.2$condition=="BB"]

BB.C.main = D.2$choice[D.2$object=="C" & D.2$trial=="main" & D.2$condition=="BB"]


# control
BB.A.control = D.2$choice[D.2$object=="A" & D.2$trial=="control" & D.2$condition=="BB"]

BB.B.control = D.2$choice[D.2$object=="B" & D.2$trial=="control" & D.2$condition=="BB"]

BB.C.control = D.2$choice[D.2$object=="C" & D.2$trial=="control" & D.2$condition=="BB"]

BB.D.control = D.2$choice[D.2$object=="D" & D.2$trial=="control" & D.2$condition=="BB"]



## ISO trial objects ##
# main
ISO.A.main = D.2$choice[D.2$object=="A" & D.2$trial=="main" & D.2$condition=="ISO"]

ISO.B.main = D.2$choice[D.2$object=="B" & D.2$trial=="main" & D.2$condition=="ISO"]

ISO.C.main = D.2$choice[D.2$object=="C" & D.2$trial=="main" & D.2$condition=="ISO"]


# control
ISO.A.control = D.2$choice[D.2$object=="A" & D.2$trial=="control" & D.2$condition=="ISO"]

ISO.B.control = D.2$choice[D.2$object=="B" & D.2$trial=="control" & D.2$condition=="ISO"]

ISO.C.control = D.2$choice[D.2$object=="C" & D.2$trial=="control" & D.2$condition=="ISO"]

ISO.D.control = D.2$choice[D.2$object=="D" & D.2$trial=="control" & D.2$condition=="ISO"]


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





#####################
# model fit indices #
#####################

caret::postResample(model_predictions, behavioral_predictions)
cor(model_predictions, behavioral_predictions)
#      RMSE  Rsquared       MAE 
# 0.2233398 0.7830250 0.1550331

