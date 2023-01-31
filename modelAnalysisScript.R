# Simulation 1 Analysis #

# Load all relevant libraries
# Install packages
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
library(Hmisc)
options(scipen=9999)


########
########
## BB ##
########
########

# load data
D = read.csv(file.choose(), header = TRUE)

# remove unnecessary columns
names(D)

# add new columns
D$ID = rep(c(1:256), each = 14)


# convert variables that should be factors into factors
D$objects = factor(D$objects)
D$subcondition = factor(D$subcondition) 
D$condition = factor(D$condition)
D$trialType = factor(D$trialType)


###############
# main analyses
###############
omnibus.aov= aov(D$ratings~(D$trialType+D$condition+D$objects)^3,
                       data = D)
summary(omnibus.aov)

# A between BB and ISO experimental conditions #
aov.A.BB.ISO.exp = aov(D$ratings[D$trialType=="Experimental" & D$objects=="A"]~D$condition[D$trialType=="Experimental" & D$objects=="A"],
               data = D)
summary(aov.A.BB.ISO.exp)

# BB A experimental
mean(D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Experimental"])

# ISO A experimental
mean(D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Experimental"])



# D between BB and ISO control conditions #
aov.D.BB.ISO.ctrl = aov(D$ratings[D$trialType=="Control" & D$objects=="D"]~D$condition[D$trialType=="Control" & D$objects=="D"],
                       data = D)
summary(aov.D.BB.ISO.ctrl)

# BB D control
mean(D$ratings[D$condition=="BB" & D$objects=="D" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="D" & D$trialType=="Control"])

# ISO D control
mean(D$ratings[D$condition=="ISO" & D$objects=="D" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="D" & D$trialType=="Control"])


# NEW OPERATIONALIZATION OF BB REASONING #

# B between BB and experimental and control conditions
aov.B.BB.exp.ctrl = aov(D$ratings[D$condition=="BB" & D$objects=="B"]~D$trialType[D$condition=="BB" & D$objects=="B"],
                        data = D)
summary(aov.B.BB.exp.ctrl)


# C between BB and experimental and control conditions
aov.C.BB.exp.ctrl = aov(D$ratings[D$condition=="BB" & D$objects=="C"]~D$trialType[D$condition=="BB" & D$objects=="C"],
                        data = D)
summary(aov.C.BB.exp.ctrl)



# OLD OPERATIONALIZATION OF BB REASONING #

aov.B.BB.IS.exp = aov(D$ratings[D$objects=="B" & D$trialType=="Experimental"]~D$condition[D$objects=="B" & D$trialType=="Experimental"],
                        data = D)
summary(aov.B.BB.IS.exp)

mean(D$ratings[D$condition=="BB" & D$trialType=="Experimental" & D$objects=="B"])
sd(D$ratings[D$condition=="BB" & D$trialType=="Experimental" & D$objects=="B"])

mean(D$ratings[D$condition=="ISO" & D$trialType=="Experimental" & D$objects=="B"])
sd(D$ratings[D$condition=="ISO" & D$trialType=="Experimental" & D$objects=="B"])


aov.C.BB.IS.exp = aov(D$ratings[D$objects=="C" & D$trialType=="Experimental"]~D$condition[D$objects=="C" & D$trialType=="Experimental"],
                      data = D)
summary(aov.C.BB.IS.exp)

mean(D$ratings[D$condition=="BB" & D$trialType=="Experimental" & D$objects=="C"])
sd(D$ratings[D$condition=="BB" & D$trialType=="Experimental" & D$objects=="C"])

mean(D$ratings[D$condition=="ISO" & D$trialType=="Experimental" & D$objects=="C"])
sd(D$ratings[D$condition=="ISO" & D$trialType=="Experimental" & D$objects=="C"])



aov.B.BB.IS.ctrl = aov(D$ratings[D$objects=="B" & D$trialType=="Control"]~D$condition[D$objects=="B" & D$trialType=="Control"],
                      data = D)
summary(aov.B.BB.IS.ctrl)

mean(D$ratings[D$condition=="BB" & D$trialType=="Control" & D$objects=="B"])
sd(D$ratings[D$condition=="BB" & D$trialType=="Control" & D$objects=="B"])

mean(D$ratings[D$condition=="ISO" & D$trialType=="Control" & D$objects=="B"])
sd(D$ratings[D$condition=="ISO" & D$trialType=="Control" & D$objects=="B"])


aov.C.BB.IS.ctrl = aov(D$ratings[D$objects=="C" & D$trialType=="Control"]~D$condition[D$objects=="C" & D$trialType=="Control"],
                      data = D)
summary(aov.C.BB.IS.ctrl)






# figures
condition_barplot = ggplot(D, aes(objects, ratings, fill=trialType)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Ratings") + # change the label of the y-axis
  facet_wrap(~condition, ncol=2) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  labs(x = "Conditions") +
  labs(fill="") +
  theme_bw() 


