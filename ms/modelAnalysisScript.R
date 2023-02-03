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


################################
# bespoke permutation function #
################################
main_perm_func = function(x,y,z1,z2){
  set.seed(2019)
  b = rep(0,10000) 
  for(i in 1:length(b)){
    a = sample(D$ratings[D$condition==x & D$objects==y & D$trialType==z1])
    b = sample(D$ratings[D$condition==x & D$objects==y & D$trialType==z2])
  }
  
  bb_dif = mean(D$ratings[D$condition==x & D$objects==y & D$trialType==z1], na.rm=TRUE)-mean(D$ratings[D$condition==x & D$objects==y & D$trialType==z2], 
                                                                                             na.rm=TRUE)
  c((sum(abs(b) > bb_dif)/length(b)),(sum(b > bb_dif)/length(b)),
    sum((abs(b) < bb_dif)/length(b)),(sum(b < bb_dif)/length(b)),
    mean(D$ratings[D$condition==x & D$objects==y & D$trialType==z1]),
    mean(D$ratings[D$condition==x & D$objects==y & D$trialType==z2]),
    bb_dif)
}

# main_perm_func("BB","B","Experimental","Control")

###############
# main analyses
###############
# omnibus anova
omnibus.aov= aov(D$ratings~(D$trialType+D$condition+D$objects)^3,
                       data = D)
summary(omnibus.aov)

###########################
## BB CONDITION ANALYSES ##
###########################
# A (BB experimental) vs D (BB control)
A.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Experimental"]
D.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="D" & D$trialType=="Control"]

t.test(A.BB.experimental.rating, D.BB.control.rating, paired = TRUE, alternative = "two.sided")

# B (BB experimental) vs B (BB control)
B.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"])
B.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"])
t.test(B.BB.experimental.rating, B.BB.control.rating, paired = TRUE, alternative = "two.sided")

# C (BB experimental) vs B (BB control)
C.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"])
C.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"])
t.test(C.BB.experimental.rating, C.BB.control.rating, paired = TRUE, alternative = "two.sided")


# A (BB control) vs B (BB experimental)
A.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Control"])
B.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"])
t.test(A.BB.control.rating, B.BB.experimental.rating, paired = TRUE, alternative = "two.sided")


# A (BB control) vs C (BB experimental)
A.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Control"]
C.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"]
t.test(A.BB.control.rating, C.BB.experimental.rating, paired = TRUE, alternative = "two.sided")


# A (BB experimental vs B (BB experimental)
A.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Experimental"])
B.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"])
C.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"])
t.test(A.BB.experimental.rating, B.BB.experimental.rating, paired = TRUE, alternative = "two.sided")
t.test(A.BB.experimental.rating, C.BB.experimental.rating, paired = TRUE, alternative = "two.sided")


# D (BB control) vs A, B, and C (BB control)
D.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="D" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="D" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="D" & D$trialType=="Control"])
A.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="A" & D$trialType=="Control"])
B.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"])
C.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"])


t.test(D.BB.control.rating, A.BB.control.rating, paired = TRUE, alternative = "two.sided")
t.test(D.BB.control.rating, B.BB.control.rating, paired = TRUE, alternative = "two.sided")
t.test(D.BB.control.rating, C.BB.control.rating, paired = TRUE, alternative = "two.sided")

############################
## ISO CONDITION ANALYSES ##
############################
A.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Experimental"]
D.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="D" & D$trialType=="Control"]

t.test(A.ISO.experimental.rating, D.ISO.control.rating, paired = TRUE, alternative = "two.sided")

# B (ISO experimental) vs B (ISO control)
B.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"])
B.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"])
t.test(B.ISO.experimental.rating, B.ISO.control.rating, paired = TRUE, alternative = "two.sided")

# C (ISO experimental) vs B (ISO control)
C.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"])
C.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"])
t.test(C.ISO.experimental.rating, C.ISO.control.rating, paired = TRUE, alternative = "two.sided")


# A (ISO control) vs B (ISO experimental)
A.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Control"])
B.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"])
t.test(A.ISO.control.rating, B.ISO.experimental.rating, paired = TRUE, alternative = "two.sided")


# A (ISO control) vs C (ISO experimental)
A.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Control"]
C.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"]
t.test(A.ISO.control.rating, C.ISO.experimental.rating, paired = TRUE, alternative = "two.sided")


# A (ISO experimental vs B & C (ISO experimental)
A.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Experimental"])
B.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"])
C.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"])
t.test(A.ISO.experimental.rating, B.ISO.experimental.rating, paired = TRUE, alternative = "two.sided")
t.test(A.ISO.experimental.rating, C.ISO.experimental.rating, paired = TRUE, alternative = "two.sided")


# D (ISO control) vs A, B, and C (ISO control)
D.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="D" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="D" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="D" & D$trialType=="Control"])
A.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="A" & D$trialType=="Control"])
B.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"])
C.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"])


t.test(D.ISO.control.rating, A.ISO.control.rating, paired = TRUE, alternative = "two.sided")
t.test(D.ISO.control.rating, B.ISO.control.rating, paired = TRUE, alternative = "two.sided")
t.test(D.ISO.control.rating, C.ISO.control.rating, paired = TRUE, alternative = "two.sided")



############################################
## OLD OPERATIONALIZATION OF BB REASONING ##
############################################
## B
# EXPERIMENTAL
B.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Experimental"])

B.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Experimental"])

t.test(B.ISO.experimental.rating, B.BB.experimental.rating, paired = TRUE, alternative = "two.sided")

# CONTROL
B.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="B" & D$trialType=="Control"])

B.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="B" & D$trialType=="Control"])

t.test(B.ISO.control.rating, B.BB.control.rating, paired = TRUE, alternative = "two.sided")

## C
# EXPERIMENTAL
C.ISO.experimental.rating = D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Experimental"])

C.BB.experimental.rating = D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"]
mean(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"])
sd(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Experimental"])

t.test(C.ISO.experimental.rating, C.BB.experimental.rating, paired = TRUE, alternative = "two.sided")

# CONTROL
C.ISO.control.rating = D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"]
mean(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"])
sd(D$ratings[D$condition=="ISO" & D$objects=="C" & D$trialType=="Control"])

C.BB.control.rating = D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"]
mean(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"])
sd(D$ratings[D$condition=="BB" & D$objects=="C" & D$trialType=="Control"])

t.test(C.ISO.control.rating, C.BB.control.rating, paired = TRUE, alternative = "two.sided")





# figures
condition_barplot = ggplot(D, aes(condition, ratings, fill=objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Ratings") + # change the label of the y-axis
  facet_wrap(~trialType, ncol=2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                                      "#888888",
                                      "#C8C8C8",
                                      "#696969")) +
                                     
  coord_cartesian(ylim=c(0, 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))


