## helper functions ##
boot_func = function(y,x, data, condition, trial_type){
  glmer.boot = function(data,b,formula){ 
    d= data[b,]
    dif.1 =  glmer(d[,y]~d[,x]+(1|ID), data=D_tall[D_tall$Condition==condition & D_tall$trialType==trial_type,], 
                   family=binomial)
    return(exp(fixef(dif.1))) #return the fixed effects coefficients. Notice that you're returning the fixed effects
  }
  glmer.Bootobj = boot(data, glmer.boot, R=500)
  return(c(exp(glmer.Bootobj$t0[[1]]), exp(glmer.Bootobj$t0[[2]]),
           exp(glmer.Bootobj$t0[[1]])  + 1.96*c(-summary(glmer.Bootobj)$bootSE[[1]], 
                                                summary(glmer.Bootobj)$bootSE[[1]]),
           exp(glmer.Bootobj$t0[[2]])  + 1.96*c(-summary(glmer.Bootobj)$bootSE[[2]], 
                                                summary(glmer.Bootobj)$bootSE[[2]])))
}


boot_func_relev = function(x,y,data,relev){
  glmer.boot = function(data,b,formula){ 
    d= data[b,]
    dif.1 =  glmer(d[,x]~relevel(d[,y], ref = relev)+(1|ID), data=data, 
                   family=binomial)
    return(exp(fixef(dif.1))) #return the fixed effects coefficients. Notice that you're returning the fixed effects
  }
  glmer.Bootobj = boot(data, glmer.boot, R=500)
  return(c(exp(glmer.Bootobj$t0[[1]]), exp(glmer.Bootobj$t0[[2]]),
           exp(glmer.Bootobj$t0[[1]])  + 1.96*c(-summary(glmer.Bootobj)$bootSE[[1]], 
                                                summary(glmer.Bootobj)$bootSE[[1]]),
           exp(glmer.Bootobj$t0[[2]])  + 1.96*c(-summary(glmer.Bootobj)$bootSE[[2]], 
                                                summary(glmer.Bootobj)$bootSE[[2]])))
}

boot_func_relev(16,12,D.inservice,"Absolutist")

follow_up_func <- function(x, y, z) {
  D_tall$objectType <- relevel(D_tall$objectType, ref = x)
  bb.experimental.glmer = glmer(choice ~ objectType + (1|ID), family = binomial,
                                data = D_tall[D_tall$Condition == y & D_tall$trialType == z, ])
  bb.experimental.CIs = exp(confint(bb.experimental.glmer, method = "profile"))
  
  results <- list(
    fixed_effects = exp(fixef(bb.experimental.glmer)),
    confidence_intervals = bb.experimental.CIs,
    p.values = summary(bb.glmer)$coefficients[,4]
  )
  
  return(results)
}


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
fix(D_tall)


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
bb.glmer = glmer(choice~(trialType+objectType)^2+(1|ID), family=binomial,
                            data=D_tall[D_tall$Condition=="Backwards Blocking",])
Anova(bb.glmer)

# control trial
bb.control.glmer = glmer(choice~objectType+(1|ID), family=binomial,
                              data=D_tall[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control",])
Anova(bb.control.glmer)

# experimental trial #
# A vs C #
D_tall$objectType <- relevel(D_tall$objectType, ref = 2)
bb.experimental.glmer = glmer(choice~objectType+(1|ID), family=binomial,
                              data=D_tall[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental",])
Anova(bb.experimental.glmer)
summary(bb.experimental.glmer)
exp(fixef(bb.experimental.glmer))
bb.experimental.CIs = exp(confint(bb.experimental.glmer, method = "profile"))
bb.experimental.CIs

# A vs B & B vs C #
D_tall$objectType <- relevel(D_tall$objectType, ref = 2)
bb.experimental.glmer = glmer(choice~objectType+(1|ID), family=binomial,
                              data=D_tall[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental",])
Anova(bb.experimental.glmer)
summary(bb.experimental.glmer)
exp(fixef(bb.experimental.glmer))
bb.experimental.CIs = exp(confint(bb.experimental.glmer, method = "profile"))
bb.experimental.CIs




## ISO EXPERIMENTAL CONDITION ##
## follow up analyses 
# BB
iso.glmer = glmer(choice~(trialType+objectType)^2+(1|ID), family=binomial,
                  data=D_tall[D_tall$Condition=="Indirect Screening-Off",])
Anova(iso.glmer)

### control trial ###
# A as reference
D_tall$objectType <- relevel(D_tall$objectType, ref = 4)
iso.control.glmer = glmer(choice~objectType+(1|ID), family=binomial,
                          data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control",])
Anova(iso.control.glmer)
summary(iso.control.glmer)
exp(fixef(iso.control.glmer)) # 0.0594709
iso.control.CIs = exp(confint(iso.control.glmer, method = "profile"))
iso.control.CIs


# B as reference
D_tall$objectType <- relevel(D_tall$objectType, ref = 3)
iso.control.glmer = glmer(choice~objectType+(1|ID), family=binomial,
                          data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control",])
Anova(iso.control.glmer)
summary(iso.control.glmer)
exp(fixef(iso.control.glmer)) # 0.05197915
iso.control.CIs = exp(confint(iso.control.glmer, method = "profile"))
iso.control.CIs


# C as reference
D_tall$objectType <- relevel(D_tall$objectType, ref = 4)
iso.control.glmer = glmer(choice~objectType+(1|ID), family=binomial,
                          data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control",])
Anova(iso.control.glmer)
summary(iso.control.glmer)
exp(fixef(iso.control.glmer)) # 0.03266591
iso.control.CIs = exp(confint(iso.control.glmer, method = "profile"))
iso.control.CIs

### experimental trial ###
# B as reference
D_tall$objectType <- relevel(D_tall$objectType, ref = 3)
iso.experimental.glmer = glmer(choice~objectType+(1|ID), family=binomial,
                               data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental",])
Anova(iso.experimental.glmer)
summary(iso.experimental.glmer)
exp(fixef(iso.experimental.glmer)) # 0.06775889
iso.experimental.CIs = exp(confint(iso.experimental.glmer, method = "profile"))
iso.experimental.CIs

# C as reference
D_tall$objectType <- relevel(D_tall$objectType, ref = 4)
iso.experimental.glmer = glmer(choice~objectType+(1|ID), family=binomial,
                               data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental",])
Anova(iso.experimental.glmer)
summary(iso.experimental.glmer)
exp(fixef(iso.experimental.glmer)) # 0.04464715
iso.experimental.CIs = exp(confint(iso.experimental.glmer, method = "profile"))
iso.experimental.CIs

################################################################################################################
# ANALYSES THAT ASSESS EVIDENCE OF BACKWARDS BLOCKING BY COMPARING REDUNDANT OBJECTS ACROSS THE EXPERIMENTAL ###
# AND CONTROL TRIALS                                                                                         ###
################################################################################################################
# create a dataframe just for the BB condition, and only for the redundant objects within the experimental
# and control conditions

## backwards blocking ##
bb.df = data.frame(ID = c(1:62), B_exp = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="B"],
                  C_exp = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"],
                  A_control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"],
                  B_control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"],
                  C_control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"])

bb.df_tall = reshape(bb.df, varying = c(2:6), v.names = "choice", 
                        timevar = "condition",   direction = "long")
bb.df_tall = bb.df_tall[order(bb.df_tall$ID),] 

bb.df_tall$objectType = rep(c("B","C","A","B","C"), times = 31)
bb.df_tall$trialType = rep(c("experimental","experimental","control",
                                "control","control"), times = 31)
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
iso.df = data.frame(ID = c(1:66), B_exp = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="B"],
                    C_exp = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"],
                    A_control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"],
                    B_control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"],
                    C_control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"])

iso.df_tall = reshape(iso.df, varying = c(2:6), v.names = "choice", 
                      timevar = "condition",   direction = "long")
iso.df_tall = iso.df_tall[order(iso.df_tall$ID),] 

iso.df_tall$objectType = rep(c("B","C","A","B","C"), times = 33)
iso.df_tall$trialType = rep(c("experimental","experimental","control",
                              "control","control"), times = 33)
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
