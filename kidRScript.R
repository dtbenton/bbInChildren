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

D_tall = D_tall[order(D_tall$ID),] # order the data frame in terms of participant ID;
                                   # to avoid wonky things happening and to save yourself 
                                   # a full-day headache in the future, reorder by ID
                                   # immediately after reshaping the dataframe.


D_tall$trialType = rep(c("control","control","control","control","control","control","control",
                         "control","control","control","main","main",
                         "main","main","main","main"), times = 94)

D_tall$testPhase = rep(c("first","first","first","first","first","second","second","second","second",
                          "second","first","first","first","second","second","second"), times = 94)

D_tall$objectType = rep(c("A","B","C","D","E",
                           "A","B","C","D","E",
                           "A","B","C",
                           "A","B","C"), times = 94)

D_tall$phaseOrder = rep(c("Phase 1","Phase 1","Phase 1","Phase 1","Phase 1",
                          "Phase 2","Phase 2","Phase 2","Phase 2","Phase 2",
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
D_tall$choices = rep(0, nrow(D_tall))
for(i in 1:nrow(D_tall)){
  if(is.na(D_tall$choice[i])==T|D_tall$choice[i]=="NaN"){
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
names(D_tall)
D_tall = as.data.frame(D_tall[,c(1:3,6,5,7,4,10,11,12,13,9,8)])
fix(D_tall)


# subset dataframes by experiments
D_tall_Exp1_5yos = subset(D_tall, ! Experiment %in% c("Experiment 2"), ! Age %in% c("4","6"))
D_tall_Exp1_5yos = subset(D_tall_Exp1_5yos, ! Age %in% c("4","6"))
D_tall_Exp1_6yos = subset(D_tall, ! Experiment %in% c("Experiment 1"))
D_tall_Exp1_6yos = subset(D_tall_Exp1_6yos, ! Age %in% c("4","5"))


# number of participants in each condition by age
table(D_tall$Condition[D_tall$Age=="4"])/16
table(D_tall$Condition[D_tall$Age=="5"])/16
table(D_tall$Condition[D_tall$Age=="6"])/16


####################
# OMNIBUS ANALYSIS #
####################
omnibus.glm = glm(choice~(Condition+objectType+Pretest)^3, family = binomial,
                  data = D_tall)
Anova(omnibus.glm)



# Visualize the data for 4s and 5s
# omnibus 2-yo figure
D_tall_Exp1_5yos_complete=D_tall_Exp1_5yos[complete.cases(D_tall_Exp1_5yos), ]
ggplot(data = D_tall_Exp1_5yos_complete) + geom_bar(mapping = aes(x=choice, color = trialType, fill = objectType),
                                           stat="count", position = "dodge",  na.rm = TRUE) + facet_wrap(~Condition)

############################################
############################################
# 5 YEAR OLD PRELIMINARY AND MAIN ANALYSES #
############################################
############################################

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
# control
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_C_BB_control_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="D" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_C_BB_control_pretest_pass





# E
# control
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])
prob_yes_5yo_C_BB_control_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="control" & D_tall_Exp1_5yos$objectType=="E" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_C_BB_control_pretest_pass



### MAIN ANALYSES ###

# Comparing A between the BB main and control trials
# passers
BB.5.A.glm.passers = glm(choice[D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct" & D_tall_Exp1_5yos$Condition=="BB"]~trialType[D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct" & D_tall_Exp1_5yos$Condition=="BB"], data=D_tall_Exp1_5yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.5.A.glm.passers)

# nonpassers
BB.5.A.glm.nonpassers = glm(choice[D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Incorrect" & D_tall_Exp1_5yos$Condition=="BB"]~trialType[D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Incorrect" & D_tall_Exp1_5yos$Condition=="BB"], data=D_tall_Exp1_5yos,
                            family=binomial, subset = (choice != "Unsure"))
summary(BB.5.A.glm.nonpassers)



# Comparing B between the BB main and control trials
# passers
BB.5.B.glm.passers = glm(choice[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct" & D_tall_Exp1_5yos$Condition=="BB"]~trialType[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Correct" & D_tall_Exp1_5yos$Condition=="BB"], data=D_tall_Exp1_5yos,
                  family=binomial, subset = (choice != "Unsure"))
summary(BB.5.B.glm.passers)

# nonpassers
BB.5.B.glm.nonpassers = glm(choice[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Incorrect" & D_tall_Exp1_5yos$Condition=="BB"]~trialType[D_tall_Exp1_5yos$objectType=="B" & D_tall_Exp1_5yos$Pretest=="Incorrect" & D_tall_Exp1_5yos$Condition=="BB"], data=D_tall_Exp1_5yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.5.B.glm.nonpassers)



# Comparing C between the BB main and control trials
# passers
BB.5.C.glm.passers = glm(choice[D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct" & D_tall_Exp1_5yos$Condition=="BB"]~trialType[D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Correct" & D_tall_Exp1_5yos$Condition=="BB"], data=D_tall_Exp1_5yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.5.C.glm.passers)

# nonpassers
BB.5.C.glm.nonpassers = glm(choice[D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Incorrect" & D_tall_Exp1_5yos$Condition=="BB"]~trialType[D_tall_Exp1_5yos$objectType=="C" & D_tall_Exp1_5yos$Pretest=="Incorrect" & D_tall_Exp1_5yos$Condition=="BB"], data=D_tall_Exp1_5yos,
                            family=binomial, subset = (choice != "Unsure"))
summary(BB.5.C.glm.nonpassers)



# Comparing A between the BB main and IS main conditions
D_tall_Exp1_5yos_obj_A = subset(D_tall_Exp1_5yos, ! objectType %in% c("B","C","D","E"))
xtabs(~choice[D_tall_Exp1_5yos_obj_A$trialType=="main"]+Condition[D_tall_Exp1_5yos_obj_A$trialType=="main"], data=D_tall_Exp1_5yos_obj_A)
BB.IS.5.A..main.glm.passers = glm(choice[D_tall_Exp1_5yos_obj_A$trialType=="main" & D_tall_Exp1_5yos_obj_A$Pretest=="Correct"]~Condition[D_tall_Exp1_5yos_obj_A$trialType=="main" & D_tall_Exp1_5yos_obj_A$Pretest=="Correct"], data=D_tall_Exp1_5yos_obj_A,
                 family=binomial, subset = (choice != "Unsure"))
summary(BB.IS.5.A..main.glm.passers)

prop.test(x = c(90, 11), n = c(100, 20))


# A BB main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])
prob_yes_5yo_A_BB_main_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="BB" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A" & D_tall_Exp1_5yos$Pretest=="Correct"])[3])
prob_yes_5yo_A_BB_main_pretest_pass


# A IS main
table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="ISO" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])
prob_yes_5yo_A_ISO_main_pretest_pass = table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="ISO" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[3]/(table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="ISO" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="ISO" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_5yos$choice[D_tall_Exp1_5yos$Condition=="ISO" & D_tall_Exp1_5yos$trialType=="main" & D_tall_Exp1_5yos$objectType=="A"])[3])
prob_yes_5yo_A_ISO_main_pretest_pass




############################################
############################################
# 6 YEAR OLD PRELIMINARY AND MAIN ANALYSES #
############################################
############################################

# A
# main
table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])
prob_yes_6yo_A_BB_main_pretest_pass = table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])[3])
prob_yes_6yo_A_BB_main_pretest_pass


# control
table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])
prob_yes_6yo_A_BB_control_pretest_pass = table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct"])[3])
prob_yes_6yo_A_BB_control_pretest_pass



# B
# main
table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])
prob_yes_6yo_B_BB_main_pretest_pass = table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])[3])
prob_yes_6yo_B_BB_main_pretest_pass


# control
table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])
prob_yes_6yo_B_BB_control_pretest_pass = table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct"])[3])
prob_yes_6yo_B_BB_control_pretest_pass


# C
# main
table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])
prob_yes_6yo_C_BB_main_pretest_pass = table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                           table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="main" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])[3])
prob_yes_6yo_C_BB_main_pretest_pass


# control
table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])
prob_yes_6yo_C_BB_control_pretest_pass = table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct"])[3])
prob_yes_6yo_C_BB_control_pretest_pass




# D
# control
table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="D" & D_tall_Exp1_6yos$Pretest=="Correct"])
prob_yes_6yo_D_BB_control_pretest_pass = table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="D" & D_tall_Exp1_6yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="D" & D_tall_Exp1_6yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="D" & D_tall_Exp1_6yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="D" & D_tall_Exp1_6yos$Pretest=="Correct"])[3])
prob_yes_6yo_D_BB_control_pretest_pass





# E
# control
table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="E" & D_tall_Exp1_6yos$Pretest=="Correct"])
prob_yes_6yo_E_BB_control_pretest_pass = table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="E" & D_tall_Exp1_6yos$Pretest=="Correct"])[3]/(table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="E" & D_tall_Exp1_6yos$Pretest=="Correct"])[1]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="E" & D_tall_Exp1_6yos$Pretest=="Correct"])[2]+
                                                                                                                                                                                                                                 table(D_tall_Exp1_6yos$choice[D_tall_Exp1_6yos$Condition=="BB" & D_tall_Exp1_6yos$trialType=="control" & D_tall_Exp1_6yos$objectType=="E" & D_tall_Exp1_6yos$Pretest=="Correct"])[3])
prob_yes_6yo_E_BB_control_pretest_pass



### MAIN ANALYSES ###

# Comparing A between the BB main and control trials
# passers
BB.6.A.glm.passers = glm(choice[D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct" & D_tall_Exp1_6yos$Condition=="BB"]~trialType[D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Correct" & D_tall_Exp1_6yos$Condition=="BB"], data=D_tall_Exp1_6yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.6.A.glm.passers)

# nonpassers
BB.6.A.glm.nonpassers = glm(choice[D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Incorrect" & D_tall_Exp1_6yos$Condition=="BB"]~trialType[D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Incorrect" & D_tall_Exp1_6yos$Condition=="BB"], data=D_tall_Exp1_6yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.6.A.glm.nonpassers)



# Comparing B between the BB main and control trials
# passers
BB.6.B.glm.passers = glm(choice[D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct" & D_tall_Exp1_6yos$Condition=="BB"]~trialType[D_tall_Exp1_6yos$objectType=="B" & D_tall_Exp1_6yos$Pretest=="Correct" & D_tall_Exp1_6yos$Condition=="BB"], data=D_tall_Exp1_6yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.6.B.glm.passers)

# nonpassers
BB.6.A.glm.nonpassers = glm(choice[D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Incorrect" & D_tall_Exp1_6yos$Condition=="BB"]~trialType[D_tall_Exp1_6yos$objectType=="A" & D_tall_Exp1_6yos$Pretest=="Incorrect" & D_tall_Exp1_6yos$Condition=="BB"], data=D_tall_Exp1_6yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.6.A.glm.nonpassers)



# Comparing C between the BB main and control trials
# passers
BB.6.C.glm.passers = glm(choice[D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct" & D_tall_Exp1_6yos$Condition=="BB"]~trialType[D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Correct" & D_tall_Exp1_6yos$Condition=="BB"], data=D_tall_Exp1_6yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.6.C.glm.passers)

# nonpassers
BB.6.C.glm.nonpassers = glm(choice[D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Incorrect" & D_tall_Exp1_6yos$Condition=="BB"]~trialType[D_tall_Exp1_6yos$objectType=="C" & D_tall_Exp1_6yos$Pretest=="Incorrect" & D_tall_Exp1_6yos$Condition=="BB"], data=D_tall_Exp1_6yos,
                         family=binomial, subset = (choice != "Unsure"))
summary(BB.6.C.glm.nonpassers)



# Comparing A between the BB main and IS main conditions
xtabs(~choice[D_tall$trialType=="main" & D_tall$Age=="6" & D_tall$objectType=="A"]+Condition[D_tall$trialType=="main" & D_tall$Age=="6" & D_tall$objectType=="A"], data=D_tall)
BB.IS.6.A..main.glm.passers = glm(choice[D_tall$trialType=="main" & D_tall$Age=="6" & D_tall$objectType=="A"]~Condition[D_tall$trialType=="main" & D_tall$Age=="6" & D_tall$objectType=="A"], data=D_tall,
                                  family=binomial, subset = (choice != "Unsure"))
summary(BB.IS.6.A..main.glm.passers)

prop.test(x = c(90, 11), n = c(100, 20))


# A BB main
table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])
prob_yes_6yo_A_BB_main_pretest_pass = table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])[3]/(table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])[1]+
                                                                                                                                                               table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])[2]+
                                                                                                                                                               table(D_tall$choice[D_tall$Condition=="BB" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])[3])
prob_yes_6yo_A_BB_main_pretest_pass


# A IS main
table(D_tall$choice[D_tall$Condition=="ISO" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])
prob_yes_6yo_A_IS_main_pretest_pass = table(D_tall$choice[D_tall$Condition=="ISO" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])[3]/(table(D_tall$choice[D_tall$Condition=="ISO" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])[1]+
                                                                                                                                                               table(D_tall$choice[D_tall$Condition=="ISO" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])[2]+
                                                                                                                                                               table(D_tall$choice[D_tall$Condition=="ISO" & D_tall$trialType=="main" & D_tall$objectType=="A" & D_tall$Age=="6"])[3])
prob_yes_6yo_A_IS_main_pretest_pass
