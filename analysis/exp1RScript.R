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


##########################
##########################
### FOLLOW-UP ANALYSES ###
##########################
##########################

## FIRST THREE-WAY INTERACTION: AGE X CONDITION X OBJECT
## BB ##
first.three.way.BB.lmer = lmer(choice~(AgeNum+objectType)^3+(1|ID), 
                            data=D_tall[D_tall$Condition=="Backwards Blocking",])
summary(first.three.way.BB.lmer)
Anova(first.three.way.BB.lmer)

# explore the interaction for different reference levels of object
refLevFunc("A","Backwards Blocking")
refLevFunc("B","Backwards Blocking")
refLevFunc("C","Backwards Blocking")


## follow up analyses for the first three-way interaction for the BB condition 
A.BB.mean = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Backwards Blocking"], rm.na=TRUE)
A.BB.mean
A.BB.sd = sd(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Backwards Blocking"])
A.BB.sd

B.BB = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Backwards Blocking"], rm.na=TRUE)
B.BB
B.BB.sd = sd(D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Backwards Blocking"])
B.BB.sd

C.BB = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Backwards Blocking"], rm.na=TRUE)
C.BB
C.BB.sd = sd(D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Backwards Blocking"])
C.BB.sd

D.BB = mean(D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Backwards Blocking"], rm.na=TRUE)
D.BB
D.BB.sd = sd(D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Backwards Blocking"])
D.BB.sd

# A vs. B = SIG
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Backwards Blocking"],
       D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Backwards Blocking"], paired=TRUE)

# A vs. C = SIG
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Backwards Blocking"],
       D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Backwards Blocking"], paired=TRUE)

# A vs. D 
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Backwards Blocking"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Backwards Blocking"])


# B vs. C
t.test(D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Backwards Blocking"],
       D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Backwards Blocking"], paired=TRUE)

# B vs. D = SIGN
t.test(D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Backwards Blocking"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Backwards Blocking"])

# C vs. D = SIGN
t.test(D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Backwards Blocking"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Backwards Blocking"])

## ISO ##
first.three.way.ISO.lmer = lmer(choice~(AgeNum+objectType)^3+(1|ID), 
                               data=D_tall[D_tall$Condition=="Indirect Screening-Off",])
summary(first.three.way.ISO.lmer)
Anova(first.three.way.ISO.lmer)

# get p.value for first.three.way.BB.lmer
coefs = data.frame(coef(summary(first.three.way.BB.lmer)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value))) # a column where the p-value is computed from the z-distribution
coefs 

# explore the interaction for different reference levels of object
refLevFunc("A","Indirect Screening-Off")
refLevFunc("B","Indirect Screening-Off")
refLevFunc("C","Indirect Screening-Off")

# follow up analyses for the first three-way interaction
A.ISO.mean = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Indirect Screening-Off"], rm.na=TRUE)
A.ISO.mean
A.ISO.sd = sd(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Indirect Screening-Off"])
A.ISO.sd

B.ISO = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Indirect Screening-Off"], rm.na=TRUE)
B.ISO
B.ISO.sd = sd(D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Indirect Screening-Off"])
B.ISO.sd

C.ISO = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Indirect Screening-Off"], rm.na=TRUE)
C.ISO
C.ISO.sd = sd(D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Indirect Screening-Off"])
C.ISO.sd

D.ISO = mean(D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Indirect Screening-Off"], rm.na=TRUE)
D.ISO
D.ISO.sd = sd(D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Indirect Screening-Off"])
D.ISO.sd

# A vs. B = SIG
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Indirect Screening-Off"],
       D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Indirect Screening-Off"], paired=TRUE)

# A vs. C = SIG
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Indirect Screening-Off"],
       D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Indirect Screening-Off"], paired=TRUE)

# A vs. D = SIG
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$Condition=="Indirect Screening-Off"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Indirect Screening-Off"])


# B vs. C
t.test(D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Indirect Screening-Off"],
       D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Indirect Screening-Off"], paired=TRUE)

# B vs. D = SIGN
t.test(D_tall$choice[D_tall$objectType=="B" & D_tall$Condition=="Indirect Screening-Off"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Indirect Screening-Off"])

# C vs. D = SIGN
t.test(D_tall$choice[D_tall$objectType=="C" & D_tall$Condition=="Indirect Screening-Off"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$Condition=="Indirect Screening-Off"])



## a plot to visualize the 
# three-way interaction between age, object
# and condition
ggplot(D_tall, aes(x = AgeNum, y = choice, color = objectType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Condition)





## SECOND THREE-WAY INTERACTION: CONDITION X TRIAL X OBJECT ##
second.three.way.BB.lmer = lmer(choice~(phaseOrder+objectType)^3+(1|ID), 
                               data=D_tall[D_tall$Condition=="Backwards Blocking",])
summary(second.three.way.BB.lmer)
Anova(second.three.way.BB.lmer)

second.three.way.ISO.lmer = lmer(choice~(phaseOrder+objectType)^3+(1|ID), 
                                data=D_tall[D_tall$Condition=="Indirect Screening-Off",])
summary(second.three.way.ISO.lmer)
Anova(second.three.way.ISO.lmer)

follow.up.second.three.way.iso = lmer(choice~objectType+(1|ID), 
                                      data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$phaseOrder=="Phase 1",])
Anova(follow.up.second.three.way.iso)

# Phase 1
A.BB.mean = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 1"], rm.na=TRUE)
A.BB.mean
A.BB.sd = sd(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 1"])
A.BB.sd

B.BB = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 1"], rm.na=TRUE)
B.BB
B.BB.sd = sd(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 1"])
B.BB.sd

C.BB = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 1"], rm.na=TRUE)
C.BB
C.BB.sd = sd(D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 1"])
C.BB.sd

D.BB = mean(D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 1"], rm.na=TRUE)
D.BB
D.BB.sd = sd(D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 1"])
D.BB.sd



# A vs. B 
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 1"],
       D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 1"], paired=TRUE)

# A vs. C 
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 1"],
       D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 1"], paired=TRUE)

# A vs. D = SIG
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 1"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 1"])


# B vs. C = SIG
t.test(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 1"],
       D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 1"], paired=TRUE)

# B vs. D 
t.test(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 1"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 1"])

# C vs. D = SIG
t.test(D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 1"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 1"])




# Phase 2
A.BB.mean = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 2"], rm.na=TRUE)
A.BB.mean
A.BB.sd = sd(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 2"])
A.BB.sd

B.BB = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 2"], rm.na=TRUE)
B.BB
B.BB.sd = sd(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 2"])
B.BB.sd

C.BB = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 2"], rm.na=TRUE)
C.BB
C.BB.sd = sd(D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 2"])
C.BB.sd

D.BB = mean(D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 2"], rm.na=TRUE)
D.BB
D.BB.sd = sd(D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 2"])

# Phase 2
A.BB.mean = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 2"], rm.na=TRUE)
A.BB.mean
A.BB.sd = sd(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 2"])
A.BB.sd

B.BB = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 2"], rm.na=TRUE)
B.BB
B.BB.sd = sd(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 2"])
B.BB.sd

C.BB = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 2"], rm.na=TRUE)
C.BB
C.BB.sd = sd(D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 2"])
C.BB.sd

D.BB = mean(D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 2"], rm.na=TRUE)
D.BB
D.BB.sd = sd(D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 2"])
D.BB.sd



# A vs. B 
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 2"],
       D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 2"], paired=TRUE)

# A vs. C 
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 2"],
       D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 2"], paired=TRUE)

# A vs. D 
t.test(D_tall$choice[D_tall$objectType=="A" & D_tall$phaseOrder=="Phase 2"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 2"])


# B vs. C 
t.test(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 2"],
       D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 2"], paired=TRUE)

# B vs. D 
t.test(D_tall$choice[D_tall$objectType=="B" & D_tall$phaseOrder=="Phase 2"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 2"])

# C vs. D 
t.test(D_tall$choice[D_tall$objectType=="C" & D_tall$phaseOrder=="Phase 2"],
       D_tall$choice[D_tall$objectType=="D" & D_tall$phaseOrder=="Phase 2"])


## THIRD THREE-WAY INTERACTION: CONDITION, TRIAL TYPE, OBJECT
## BB EXPERIMENTAL CONDITION ##
bb.experimental.lmer = lmer(choice~objectType+(1|ID), data=D_tall[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental",])
Anova(bb.experimental.lmer)

# A
A = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="A"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="A"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="A"], na.rm=TRUE)

# B
B = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)

# C
C = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)


## BB CONTROL CONDITION ##
bb.control.lmer = lmer(choice~objectType+(1|ID), data=D_tall[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control",])
Anova(bb.control.lmer)

# A
A = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)

# B
B = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)

# C
C = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)

# D
D = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="D"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="D"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="D"], na.rm=TRUE)

t.test(D,A, alternative="two.sided", paired = TRUE)
t.test(D,B, alternative="two.sided", paired = TRUE)
t.test(D,C, alternative="two.sided", paired = TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)


## ISO EXPERIMENTAL CONDITION ##
iso.experimental.lmer = lmer(choice~objectType+(1|ID), data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental",])
Anova(iso.experimental.lmer)

# A
A = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="A"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="A"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="A"], na.rm=TRUE)

# B
B = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)

# C
C = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)


## ISO CONTROL CONDITION ##
iso.control.lmer = lmer(choice~objectType+(1|ID), data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control",])
Anova(iso.control.lmer)

# A
A = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)

# B
B = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)

# C
C = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)

# D
D = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="D"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="D"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="D"], na.rm=TRUE)

t.test(D,A, alternative="two.sided", paired = TRUE)
t.test(D,B, alternative="two.sided", paired = TRUE)
t.test(D,C, alternative="two.sided", paired = TRUE)

t.test(A,B, alternative="two.sided", paired = TRUE)
t.test(A,C, alternative="two.sided", paired = TRUE)
t.test(B,C, alternative="two.sided", paired = TRUE)


###################
###################
### BB ANALYSES ###
###################
# A control
A.control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)

# B experimental v B control
B.control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)


B.experimental = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)


# C experimental v C control
C.control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)


C.experimental = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)


# create data frame and add columns from the variables above 
D.new.op = data.frame(ID = c(1:62), A.control = A.control, B.control = B.control, B.experimental = B.experimental,
                      C.control = C.control, C.experimental = C.experimental)

names(D.new.op)
dim(D.new.op)

D.new.op_tall = reshape(D.new.op, varying = c(2:6), v.names = "choice", 
                        timevar = "condition",   direction = "long")
D.new.op_tall = D.new.op_tall[order(D.new.op_tall$ID),] 

D.new.op_tall$objectType = rep(c("A","B","B","C","C"), times = 31)
D.new.op_tall$trialType = rep(c("control","control","main",
                                "control","main"), times = 31)
D.new.op_tall$objectType = factor(D.new.op_tall$objectType)
D.new.op_tall$trialType = factor(D.new.op_tall$trialType)


# main analysis 
bb.evidence.lmer = lmer(choice~(objectType+trialType)^2+(1|ID), 
                data = D.new.op_tall)
Anova(bb.evidence.lmer)


mean(D.new.op_tall$choice[D.new.op_tall$trialType=="main"], na.rm = TRUE)
sd(D.new.op_tall$choice[D.new.op_tall$trialType=="main"], na.rm = TRUE)


mean(D.new.op_tall$choice[D.new.op_tall$trialType=="control"], na.rm = TRUE)
sd(D.new.op_tall$choice[D.new.op_tall$trialType=="control"], na.rm = TRUE)



###################
###################
### ISO ANALYSES ###
###################
# A control
A.control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)

# B experimental v B control
B.control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)


B.experimental = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="B"], na.rm=TRUE)


# C experimental v C control
C.control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)


C.experimental = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="experimental" & D_tall$objectType=="C"], na.rm=TRUE)


# create data frame and add columns from the variables above 
D.new.op = data.frame(ID = c(1:66), A.control = A.control, B.control = B.control, B.experimental = B.experimental,
                      C.control = C.control, C.experimental = C.experimental)

names(D.new.op)
dim(D.new.op)

D.new.op_tall = reshape(D.new.op, varying = c(2:6), v.names = "choice", 
                        timevar = "condition",   direction = "long")
D.new.op_tall = D.new.op_tall[order(D.new.op_tall$ID),] 

D.new.op_tall$objectType = rep(c("A","B","B","C","C"), times = 33)
D.new.op_tall$trialType = rep(c("control","control","main",
                                "control","main"), times = 33)
D.new.op_tall$objectType = factor(D.new.op_tall$objectType)
D.new.op_tall$trialType = factor(D.new.op_tall$trialType)


# main analysis 
iso.evidence.lmer = lmer(choice~(objectType+trialType)^2+(1|ID), 
                         data = D.new.op_tall)
Anova(iso.evidence.lmer)


mean(D.new.op_tall$choice[D.new.op_tall$trialType=="main"], na.rm = TRUE)
sd(D.new.op_tall$choice[D.new.op_tall$trialType=="main"], na.rm = TRUE)


mean(D.new.op_tall$choice[D.new.op_tall$trialType=="control"], na.rm = TRUE)
sd(D.new.op_tall$choice[D.new.op_tall$trialType=="control"], na.rm = TRUE)

##############
##############
### FIGURE ###
##############
##############
condition_barplot = ggplot(D_tall, aes(trialType, choice, fill = objectType)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  facet_wrap(~Condition) +
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                             "#888888",
                             "#C8C8C8",
                             "#696969")) +
  
  coord_cartesian(ylim=c(0, 1.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank()) + 
  ylab("# of questions children judged object was a blicket")
