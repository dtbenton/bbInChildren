############################################################
############################################################
############################################################
#############                                  #############
#############      BACKWARD BLOCKING STUDY     #############
#############                                  #############
############################################################
############################################################
############################################################
# LOAD ALL RELEVANT LIBRARIES:
library(lme4)
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
options(scipen=9999)

# DATA CLEAN UP AND RESTRUCTURING #
D = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

names(D)

# write a function to replace "NaNs" and "N/As" with NA
for(i in 1:nrow(D)){
  for(j in 1:ncol(D)){
    if(is.na(D[[i,j]]=="NaN")){
      D[[i,j]] = NA
    } else if(D[[i,j]]=="N/A"){
      D[[i,j]] = NA
    }else {
      D[[i,j]] = D[[i,j]]
    }
  }
}


D_tall =  reshape(D, varying = c(9:24), v.names = "measure", 
                  timevar = "condition",   direction = "long")


D_tall$trialType = rep(c("control","control","control","control","control","control","control",
                         "control","control","control","main","main",
                         "main","main","main","main"), times = 94)

D_tall$testPhase = rep(c("first","first","first","first","first","second","second","second","second",
                          "second","first","first","first","second","second","second"), times = 94)

D_tall$objectType = rep(c("A","B","C","D","E",
                           "A","B","C","D","E",
                           "A","B","C",
                           "A","B","C"), times = 94)


D_tall = D_tall[order(D_tall$ID),] # order the data frame in terms of participant ID

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
D_tall$choices = rep(0, nrow(D_tall))
for(i in 1:nrow(D_tall)){
  if(is.na(D_tall$choice[i])==T){
    D_tall$choices[i]= NA
  } else if(D_tall$choice[i]==1){
    D_tall$choices[i]="Yes"
  } else if(D_tall$choice[i]==0){
    D_tall$choices[i]="No"
  } else {
    D_tall$choices[i]="Unsure"
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
D_tall = as.data.frame(D_tall[,c(1:3,6,5,7,4,10,11,12,9,8)])
fix(D_tall)


# subset dataframes by experiments
D_tall_Exp1_5yos = subset(D_tall, ! Experiment %in% c("Experiment 2"), ! Age %in% c("4","6"))
D_tall_Exp1_5yos = subset(D_tall_Exp1_5yos, ! Age %in% c("4","6"))
D_tall_Exp2_6yos = subset(D_tall, ! Experiment %in% c("Experiment 1"))
D_tall_Exp2_6yos = subset(D_tall_Exp2_6yos, ! Age %in% c("4","5"))


# number of participants in each condition by age
table(D_tall$Condition[D_tall$Age=="4"])/16
table(D_tall$Condition[D_tall$Age=="5"])/16
table(D_tall$Condition[D_tall$Age=="6"])/16


####################
# OMNIBUS ANALYSIS #
####################
omnibus.glm = glm(choice~(Condition+object_type+Pretest)^3, family = binomial,
                  data = D_tall)
Anova(omnibus.glm)



# Visualize the data for 4s and 5s
# omnibus 2-yo figure
ggplot(data = D_tall_Exp1_5yos) + geom_bar(mapping = aes(x=choice, color = trialType, fill = objectType),
                                           stat="count", position = "dodge") + facet_wrap(~Condition)


############################################
# 5 YEAR OLD PRELIMINARY AND MAIN ANALYSES #
############################################
### PRELIMINARY ANALYSES ###
# BB: Compare B in the BB trial to B in the BB-control trial;
# B should be lower in the BB control trial than the BB main trial
# if children engaged in genuine BB reasoning. Ditto for the other objects.

# A
# main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_A_BB_main_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_A_BB_main_pretest_pass


# control
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_A_BB_control_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_A_BB_control_pretest_pass



# B
# main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_B_BB_main_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                         table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                         table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_B_BB_main_pretest_pass


# control
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_B_BB_control_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_B_BB_control_pretest_pass


# C
# main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_C_BB_main_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_C_BB_main_pretest_pass


# control
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_C_BB_control_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_C_BB_control_pretest_pass




# D
# main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_C_BB_main_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_C_BB_main_pretest_pass


# control
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_C_BB_control_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_C_BB_control_pretest_pass





# E
# main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_C_BB_main_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_C_BB_main_pretest_pass


# control
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_C_BB_control_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_C_BB_control_pretest_pass


#####################
### MAIN ANALYSES ###
#####################
# Comparing A between the BB main and control trials
# passers
BB.5.A.glm.passers = glm(choice[D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"]~trialType[D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"], data=D_tall_Exp1_5yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.5.A.glm.passers)

# nonpassers
BB.5.A.glm.nonpassers = glm(choice[D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Incorrect"]~trialType[D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Incorrect"], data=D_tall_Exp1_5yos,
                            family=binomial, subset = (choice != "Unsure"))
summary(BB.5.A.glm.nonpassers)



# Comparing B between the BB main and control trials
# passers
BB.5.B.glm.passers = glm(choice[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"]~trialType[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"], data=D_tall_Exp1_5yos,
                  family=binomial, subset = (choice != "Unsure"))
summary(BB.5.B.glm.passers)

# nonpassers
BB.5.B.glm.nonpassers = glm(choice[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Incorrect"]~trialType[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Incorrect"], data=D_tall_Exp1_5yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.5.B.glm.nonpassers)



# Comparing C between the BB main and control trials
# passers
BB.5.C.glm.passers = glm(choice[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"]~trialType[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct"], data=D_tall_Exp1_5yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.5.C.glm.passers)

# nonpassers
BB.5.C.glm.nonpassers = glm(choice[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Incorrect"]~trialType[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Incorrect"], data=D_tall_Exp1_5yos,
                            family=binomial, subset = (choice != "Unsure"))
summary(BB.5.C.glm.nonpassers)



# Comparing A between the BB main and IS main conditions
D_tall_Exp1_5yos_obj_A = subset(D_tall_Exp1_5yos, ! objectType %in% c("B","C","D","E"))
xtabs(~choice+trialType+Condition, data=D_tall_Exp1_5yos_obj_A)
BB.IS.5.A..main.glm.passers = glm(choice[D_tall_Exp1_5yos_obj_A$trialType=="main" & D_tall_Exp1_5yos_obj_A$Pretest=="Correct"]~Condition[D_tall_Exp1_5yos_obj_A$trialType=="main" & D_tall_Exp1_5yos_obj_A$Pretest=="Correct"], data=D_tall_Exp1_5yos_obj_A,
                 family=binomial, subset = (choice != "Unsure"))
summary(BB.IS.5.A..main.glm.passers)

prop.test(x = c(90, 10), n = c(113, 12))


# A BB main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])
prob_yes_5yo_A_BB_main = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="B"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[1]+
                                                                                                                              table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[2]+
                                                                                                                              table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[3])
prob_yes_5yo_A_BB_main


# A IS main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="ISO" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])
prob_yes_5yo_A_IS_main = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="IS" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="B"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="IS" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[1]+
                                                                                                                                                                        table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="IS" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[2]+
                                                                                                                                                                        table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="IS" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[3])
prob_yes_5yo_A_IS_main




############################################
# 6 YEAR OLD PRELIMINARY AND MAIN ANALYSES #
############################################
### PRELIMINARY ANALYSES ###
# BB: Compare B in the BB trial to B in the BB-control trial;
# B should be lower in the BB control trial than the BB main trial
# if children engaged in genuine BB reasoning. Ditto for the other objects.

# B
table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="B"])
prob_yes_6yo_B_BB_main = table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="B"])[3]/(table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="B"])[1]+
                                                                                                                              table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="B"])[2]+
                                                                                                                              table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="B"])[3])
prob_yes_6yo_B_BB_main



table(D.6yo$choice[D.6yo$Condition=="BB"& D.6yo$trial_type=="control" & D.6yo$object_type=="B"])
prob_yes_6yo_B_BB_control = table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="control" & D.6yo$object_type=="B"])[3]/(table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="control" & D.6yo$object_type=="B"])[1]+
                                                                                                                                    table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="control" & D.6yo$object_type=="B"])[2]+
                                                                                                                                    table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="control" & D.6yo$object_type=="B"])[3])
prob_yes_6yo_B_BB_control


# C
table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="C"])
prob_yes_6yo_C_BB_main = table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="C"])[3]/(table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="C"])[1]+
                                                                                                                              table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="C"])[2]+
                                                                                                                              table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="C"])[3])
prob_yes_6yo_C_BB_main



table(D.6yo$choice[D.6yo$Condition=="BB"& D.6yo$trial_type=="control" & D.6yo$object_type=="C"])
prob_yes_6yo_C_BB_control = table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="control" & D.6yo$object_type=="C"])[3]/(table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="control" & D.6yo$object_type=="C"])[1]+
                                                                                                                                    table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="control" & D.6yo$object_type=="C"])[2]+
                                                                                                                                    table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="control" & D.6yo$object_type=="C"])[3])
prob_yes_6yo_C_BB_control



### MAIN ANALYSES ###
# Comparing B between the BB main and control trials
BB.6.B.glm = glm(choice[D.6yo$object_type=="B"]~trial_type[D.6yo$object_type=="B"], data=D.6yo,
                 family=binomial, subset = (choice != "Unsure"))
summary(BB.6.B.glm)

# Comparing C between the BB main and control trials
BB.6.C.glm = glm(choice[D.6yo$object_type=="C"]~trial_type[D.6yo$object_type=="C"], data=D.6yo,
                 family=binomial, subset = (choice != "Unsure"))
summary(BB.6.C.glm)


# Comparing A between the BB main and IS main conditions
D.6yo.A = subset(D.6yo, ! object_type %in% c("B","C","D","E"))
BB.IS.6.A..main.glm = glm(choice[D.6yo.A$trial_type=="main"]~Condition[D.6yo.A$trial_type=="main"], data=D.6yo.A,
                          family=binomial, subset = (choice != "Unsure"))
summary(BB.IS.6.A..main.glm)
Anova(BB.IS.6.A..main.glm)


# A BB main
table(D.6yo.A$choice[D.6yo.A$Condition=="BB" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])
prob_yes_6yo_A_BB_main = table(D.6yo$choice[D.6yo$Condition=="BB" & D.6yo$trial_type=="main" & D.6yo$object_type=="B"])[3]/(table(D.6yo.A$choice[D.6yo.A$Condition=="BB" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])[1]+
                                                                                                                              table(D.6yo.A$choice[D.6yo.A$Condition=="BB" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])[2]+
                                                                                                                              table(D.6yo.A$choice[D.6yo.A$Condition=="BB" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])[3])
prob_yes_6yo_A_BB_main


# A IS main
table(D.6yo.A$choice[D.6yo.A$Condition=="ISO" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])
prob_yes_6yo_A_ISO_main = table(D.6yo.A$choice[D.6yo.A$Condition=="ISO" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])[3]/(table(D.6yo.A$choice[D.6yo.A$Condition=="ISO" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])[1]+
                                                                                                                                        table(D.6yo.A$choice[D.6yo.A$Condition=="ISO" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])[2]+
                                                                                                                                        table(D.6yo.A$choice[D.6yo.A$Condition=="ISO" & D.6yo.A$trial_type=="main" & D.6yo.A$object_type=="A"])[3])
prob_yes_6yo_A_ISO_main





##########################################
# OVERALL  PRELIMINARY AND MAIN ANALYSES #
##########################################
# B
table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="main" & D_tall$object_type=="B"])
overall_prob_B_BB_main = table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="main" & D_tall$object_type=="B"])[3]/(table(D_tall$choice[D_tall$Condition=="BB" &D_tall$trial_type=="main" & D_tall$object_type=="B"])[1]+
                                                                                                                              table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="main" & D_tall$object_type=="B"])[2]+
                                                                                                                              table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="main" & D_tall$object_type=="B"])[3])
overall_prob_B_BB_main



table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="control" & D_tall$object_type=="B"])
overall_prob_B_BB_control = table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="control" & D_tall$object_type=="B"])[3]/(table(D_tall$choice[D_tall$Condition=="BB" &D_tall$trial_type=="control" & D_tall$object_type=="B"])[1]+
                                                                                                                                  table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="control" & D_tall$object_type=="B"])[2]+
                                                                                                                                  table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="control" & D_tall$object_type=="B"])[3])
overall_prob_B_BB_control


# C
table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="main" & D_tall$object_type=="C"])
overall_prob_C_BB_main = table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="main" & D_tall$object_type=="C"])[3]/(table(D_tall$choice[D_tall$Condition=="BB" &D_tall$trial_type=="main" & D_tall$object_type=="C"])[1]+
                                                                                                                                  table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="main" & D_tall$object_type=="C"])[2]+
                                                                                                                                  table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="main" & D_tall$object_type=="C"])[3])
overall_prob_C_BB_main



table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="control" & D_tall$object_type=="C"])
overall_prob_C_BB_control = table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="control" & D_tall$object_type=="C"])[3]/(table(D_tall$choice[D_tall$Condition=="BB" &D_tall$trial_type=="control" & D_tall$object_type=="C"])[1]+
                                                                                                                                        table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="control" & D_tall$object_type=="C"])[2]+
                                                                                                                                        table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trial_type=="control" & D_tall$object_type=="C"])[3])
overall_prob_C_BB_control



### MAIN ANALYSES ###
# Comparing B between the BB main and control trials
BB.B.glm = glm(choice[D_tall$object_type=="B"]~(trial_type[D_tall$object_type=="B"]+trial_type[D_tall$object_type=="B"]+Age[D_tall$object_type=="B"])^2, data=D_tall,
                 family=binomial, subset = (choice != "Unsure"))
summary(BB.B.glm)
Anova(BB.B.glm)

# Comparing C between the BB main and control trials
BB.C.glm = glm(choice[D_tall$object_type=="C"]~trial_type[D_tall$object_type=="C"], data=D_tall,
               family=binomial, subset = (choice != "Unsure"))
summary(BB.C.glm)


# Comparing A between the BB main and IS main conditions
D.A = subset(D_tall, ! object_type %in% c("B","C","D","E"))
BB.IS.A..main.glm = glm(choice[D.A$trial_type=="main"]~(Condition[D.A$trial_type=="main"]+Age[D.A$trial_type=="main"])^2, data=D.A,
                          family=binomial, subset = (choice != "Unsure"))
summary(BB.IS.A..main.glm)
Anova(BB.IS.A..main.glm)


# A BB main
table(D.A$choice[D.A$Condition=="BB" & D.A$trial_type=="main" & D.A$object_type=="A"])
prob_yes_A_BB_main = table(D.A$choice[D.A$Condition=="BB" & D.A$trial_type=="main" & D.A$object_type=="A"])[3]/(table(D.A$choice[D.A$Condition=="BB" & D.A$trial_type=="main" & D.A$object_type=="A"])[1]+
                                                                                                                              table(D.A$choice[D.A$Condition=="BB" & D.A$trial_type=="main" & D.A$object_type=="A"])[2]+
                                                                                                                              table(D.A$choice[D.A$Condition=="BB" & D.A$trial_type=="main" & D.A$object_type=="A"])[3])
prob_yes_A_BB_main


# A IS main
table(D.A$choice[D.A$Condition=="ISO" & D.A$trial_type=="main" & D.A$object_type=="A"])
prob_yes_A_IS_main = table(D.A$choice[D.A$Condition=="ISO" & D.A$trial_type=="main" & D.A$object_type=="A"])[3]/(table(D.A$choice[D.A$Condition=="ISO" & D.A$trial_type=="main" & D.A$object_type=="A"])[1]+
                                                                                                                  table(D.A$choice[D.A$Condition=="ISO" & D.A$trial_type=="main" & D.A$object_type=="A"])[2]+
                                                                                                                  table(D.A$choice[D.A$Condition=="ISO" & D.A$trial_type=="main" & D.A$object_type=="A"])[3])
prob_yes_A_IS_main