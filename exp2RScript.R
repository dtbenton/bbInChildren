# BESPOKE MULTIPLE COMPARISONS FUNCTION #
multiple_comparison_func = function(trialTypeName, conditionName){
  # Define the object types
  objectTypes = c("A", "B", "C", "D", "E")
  
  # Initialize a data frame to hold the results
  results = data.frame()
  
  # Loop over the object types
  for(i in 1:length(objectTypes)){
    for(j in 1:length(objectTypes)){
      # Skip if object types are the same
      if(i == j) next
      
      # Extract the matched pairs
      choices_i <- D_tall$choice[D_tall$objectType==objectTypes[i] & D_tall$trialType==trialTypeName & D_tall$Condition==conditionName]
      choices_j <- D_tall$choice[D_tall$objectType==objectTypes[j] & D_tall$trialType==trialTypeName & D_tall$Condition==conditionName]
      
      # Find the minimum length of the two vectors
      min_len <- min(length(choices_i), length(choices_j))
      
      # Check if enough observations are available for the t-test
      if (min_len < 2) {
        # Skip the comparison if not enough observations
        next
      }
      
      # Perform the t-test using only the matched pairs
      test_result <- t.test(choices_i[1:min_len], choices_j[1:min_len], paired=TRUE)
      
      # Print what's being compared
      print(paste("Comparing", objectTypes[i], "vs.", objectTypes[j]))
      
      # Create column names dynamically
      col_i <- objectTypes[i]
      col_j <- objectTypes[j]
      
      # Save the comparison and the t-test results to the results data frame
      results = rbind(results, data.frame(Comparison = paste(objectTypes[i], "vs.", objectTypes[j]),
                                          P_Value = test_result$p.value,
                                          t_value = test_result$statistic,
                                          df = test_result$parameter,
                                          mean_i = mean(choices_i),
                                          mean_j = mean(choices_j),
                                          sd_i = sd(choices_i),
                                          sd_j = sd(choices_j)))
    }
  }
  
  # Print the data frame of results
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
library(BFpack)
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
                         "control","control", "control","main","main",
                         "main","main","main","main"), times =64)

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

###################################
# Participant section information #
###################################
# 5 yos #
# number of males and females
table(D_tall$Sex[D_tall$AgeCat=="5"])/16

# 6 yos #
# number of males and females
table(D_tall$Sex[D_tall$AgeCat=="6"])/16


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


## THREE-WAY INTERACTION: CONDITION X TRIAL TYPE X OBJECT

## BB main CONDITION ##
bb.main.lmer = lmer(choice~objectType+(1|ID), data=D_tall[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="main",])
Anova(bb.main.lmer)

multiple_comparison_func("main","Backwards Blocking")


## BB CONTROL CONDITION ##
bb.control.lmer = lmer(choice~objectType+(1|ID), data=D_tall[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control",])
Anova(bb.control.lmer)



## ISO main CONDITION ##
iso.main.lmer = lmer(choice~objectType+(1|ID), data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="main",])
Anova(iso.main.lmer)

multiple_comparison_func("main","Indirect Screening-Off")



## ISO CONTROL CONDITION ##
iso.control.lmer = lmer(choice~objectType+(1|ID), data=D_tall[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control",])
Anova(iso.control.lmer)

multiple_comparison_func("control","Indirect Screening-Off")



###################
###################
### BB ANALYSES ###
###################
# A control
A.control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="A"], na.rm=TRUE)

# B control
B.control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)


# C control
C.control = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)


# C main
C.main = D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="main" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="main" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Backwards Blocking" & D_tall$trialType=="main" & D_tall$objectType=="C"], na.rm=TRUE)


# create data frame and add columns from the variables above 
D.new.op = data.frame(ID = c(1:64), A.control = A.control, B.control = B.control, C.control = C.control,
                      C.main = C.main)

names(D.new.op)
dim(D.new.op)

D.new.op_tall = reshape(D.new.op, varying = c(2:5), v.names = "choice", 
                        timevar = "condition",   direction = "long")
D.new.op_tall = D.new.op_tall[order(D.new.op_tall$ID),] 

D.new.op_tall$objectType = rep(c("A","B","C","C"), times = 32)
D.new.op_tall$trialType = rep(c("control","control","control","main"), times = 32)
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

# B control
B.control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="B"], na.rm=TRUE)

# C control
C.control = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="control" & D_tall$objectType=="C"], na.rm=TRUE)


# C main 
C.main = D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="main" & D_tall$objectType=="C"]
mean(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="main" & D_tall$objectType=="C"], na.rm=TRUE)
sd(D_tall$choice[D_tall$Condition=="Indirect Screening-Off" & D_tall$trialType=="main" & D_tall$objectType=="C"], na.rm=TRUE)



# create data frame and add columns from the variables above 
D.new.op = data.frame(ID = c(1:64), A.control = A.control, B.control = B.control, C.control = C.control,
                      C.main = C.main)

names(D.new.op)
dim(D.new.op)

D.new.op_tall = reshape(D.new.op, varying = c(2:5), v.names = "choice", 
                        timevar = "condition",   direction = "long")
D.new.op_tall = D.new.op_tall[order(D.new.op_tall$ID),] 

D.new.op_tall$objectType = rep(c("A","B","C","C"), times = 32)
D.new.op_tall$trialType = rep(c("control","control","control",
                                "main"), times = 32)
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


c = rep(0,1000)
for(i in 1:1000){
  x = sample(D.new.op_tall$choice[D.new.op_tall$trialType=="main"], replace=TRUE)
  y = sample(D.new.op_tall$choice[D.new.op_tall$trialType=="control"], replace=TRUE)
  dif = x - y
  c[i] = mean(dif)
}

# plot the null data
hist(c)
segments(bb_dif,0,bb_dif,200, col="green")
abline(v = bb_dif, col = "blue", lwd = 2) #abline is probably a better function

# compute the actual difference beween BB pre and BB post
bb_dif = mean(D.new.op_tall$choice[D.new.op_tall$trialType=="main"]-D.new.op_tall$choice[D.new.op_tall$trialType=="control"])

# 1- and 2-tailed p-values
sum(c > bb_dif)/1000 # one-tailed
sum(abs(c) > bb_dif)/1000

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
                             "#696969",
                             "#A8A8A8")) +
  
  coord_cartesian(ylim=c(0, 1.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank()) + 
  ylab("# of questions children judged object was a blicket")
