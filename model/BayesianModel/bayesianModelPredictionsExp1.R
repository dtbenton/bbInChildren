##################################
##################################
### BAYESIAN MODEL PREDICTIONS ###
##################################
##################################

# load relevant libraries
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
library(BFpack)
options(scipen=9999)

## p = 0.5 ##
# create variables
# BB
#experimental
BB.A.experimental = rep("1",16)
BB.B.experimental = rep("0.5",16) # 0.5+0.5
BB.C.experimental = rep("0.5",16) # 0.5+0.5


# control
BB.A.control = rep("0.57",16) # 0.5+0.5
BB.B.control = rep("0.57",16) # 0.5+0.5
BB.C.control = rep("0.57",16) # 0.5+0.5
BB.D.control = rep("1",16) # 0.5+0.5


# ISO
#experimental
ISO.A.experimental = rep("0",16)
ISO.B.experimental = rep("0.67",16) # 0.5+0.5
ISO.C.experimental = rep("0.67",16) # 0.5+0.5


# control
ISO.A.control = rep("0.57",16) # 0.5+0.5
ISO.B.control = rep("0.57",16) # 0.5+0.5
ISO.C.control = rep("0.57",16) # 0.5+0.5
ISO.D.control = rep("0",16) # 0.5+0.5



# create a dataframe that combines the variables
D.new.experimental = data.frame(BB.A.experimental = BB.A.experimental, BB.B.experimental = BB.B.experimental,
                        BB.C.experimental = BB.C.experimental, ISO.A.experimental = ISO.A.experimental, 
                        ISO.B.experimental = ISO.B.experimental, ISO.C.experimental = ISO.C.experimental)

D_experimental_tall =  reshape(D.new.experimental, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_experimental_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_experimental_tall$id = NULL

# organize the dataframe by ID
D_experimental_tall = D_experimental_tall[order(D_experimental_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_experimental_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_experimental_tall$trial = rep(c("experimental"), times = 96)

# create 'object' column
D_experimental_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_experimental_tall$row.names = NULL

# reorder columns 
D_experimental_tall = D_experimental_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = BB.A.control,
                           BB.B.control = BB.B.control, BB.C.control = BB.C.control,
                           BB.D.control = BB.D.control, ISO.A.control = ISO.A.control,
                           ISO.B.control = ISO.B.control, ISO.C.control = ISO.C.control,
                           ISO.D.control = ISO.D.control)

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_experimental_tall,
               D_control_tall)
names(D_tall)

# convert variables to their proper type
names(D_tall)
str(D_tall)
D_tall$measure = as.numeric(D_tall$measure)
D_tall$condition = factor(D_tall$condition)
D_tall$trial = factor(D_tall$trial)
D_tall$objects = factor(D_tall$objects)
str(D_tall)


# plot data
condition_barplot = ggplot(D_tall, aes(trial, measure, fill=objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  ylab("Ratings") + # change the label of the y-axis
  facet_wrap(~condition, ncol=2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                             "#888888",
                             "#C8C8C8",
                             "#696969")) +
  
  coord_cartesian(ylim=c(0, 1.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank())





######################
######################
###### P = 0.65 ######
######################
######################
# BB
#experimental
BB.A.experimental = rep("1",16)
BB.B.experimental = rep("0.65",16) # 0.5+0.5
BB.C.experimental = rep("0.65",16) # 0.5+0.5


# control
BB.A.control = rep("0.68",16) # 0.5+0.5
BB.B.control = rep("0.68",16) # 0.5+0.5
BB.C.control = rep("0.68",16) # 0.5+0.5
BB.D.control = rep("1",16) # 0.5+0.5


# ISO
#experimental
ISO.A.experimental = rep("0",16)
ISO.B.experimental = rep("0.74",16) # 0.5+0.5
ISO.C.experimental = rep("0.74",16) # 0.5+0.5


# control
ISO.A.control = rep("0.68",16) # 0.5+0.5
ISO.B.control = rep("0.68",16) # 0.5+0.5
ISO.C.control = rep("0.68",16) # 0.5+0.5
ISO.D.control = rep("0",16) # 0.5+0.5



# create a dataframe that combines the variables
D.new.experimental = data.frame(BB.A.experimental = BB.A.experimental, BB.B.experimental = BB.B.experimental,
                        BB.C.experimental = BB.C.experimental, ISO.A.experimental = ISO.A.experimental, 
                        ISO.B.experimental = ISO.B.experimental, ISO.C.experimental = ISO.C.experimental)

D_experimental_tall =  reshape(D.new.experimental, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_experimental_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_experimental_tall$id = NULL

# organize the dataframe by ID
D_experimental_tall = D_experimental_tall[order(D_experimental_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_experimental_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_experimental_tall$trial = rep(c("experimental"), times = 96)

# create 'object' column
D_experimental_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_experimental_tall$row.names = NULL

# reorder columns 
D_experimental_tall = D_experimental_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = BB.A.control,
                           BB.B.control = BB.B.control, BB.C.control = BB.C.control,
                           BB.D.control = BB.D.control, ISO.A.control = ISO.A.control,
                           ISO.B.control = ISO.B.control, ISO.C.control = ISO.C.control,
                           ISO.D.control = ISO.D.control)

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_experimental_tall,
               D_control_tall)
names(D_tall)


# convert variables to their proper type
names(D_tall)
str(D_tall)
D_tall$measure = as.numeric(D_tall$measure)
D_tall$condition = factor(D_tall$condition)
D_tall$trial = factor(D_tall$trial)
D_tall$objects = factor(D_tall$objects)
str(D_tall)

# plot data
# figures
condition_barplot = ggplot(D_tall, aes(trial, measure, fill=objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  ylab("Ratings") + # change the label of the y-axis
  facet_wrap(~condition, ncol=2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                                      "#888888",
                                      "#C8C8C8",
                                      "#696969")) +
                                        
  coord_cartesian(ylim=c(0, 1.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank())




######################
######################
###### P = 0.80 ######
######################
######################
# BB
#experimental
BB.A.experimental = rep("1",16)
BB.B.experimental = rep("0.8",16) # 0.5+0.5
BB.C.experimental = rep("0.8",16) # 0.5+0.5


# control
BB.A.control = rep("0.81",16) # 0.5+0.5
BB.B.control = rep("0.81",16) # 0.5+0.5
BB.C.control = rep("0.81",16) # 0.5+0.5
BB.D.control = rep("1",16) # 0.5+0.5


# ISO
#experimental
ISO.A.experimental = rep("0",16)
ISO.B.experimental = rep("0.83",16) # 0.5+0.5
ISO.C.experimental = rep("0.83",16) # 0.5+0.5


# control
ISO.A.control = rep("0.81",16) # 0.5+0.5
ISO.B.control = rep("0.81",16) # 0.5+0.5
ISO.C.control = rep("0.81",16) # 0.5+0.5
ISO.D.control = rep("0",16) # 0.5+0.5



# create a dataframe that combines the variables
D.new.experimental = data.frame(BB.A.experimental = BB.A.experimental, BB.B.experimental = BB.B.experimental,
                        BB.C.experimental = BB.C.experimental, ISO.A.experimental = ISO.A.experimental, 
                        ISO.B.experimental = ISO.B.experimental, ISO.C.experimental = ISO.C.experimental)

D_experimental_tall =  reshape(D.new.experimental, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_experimental_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_experimental_tall$id = NULL

# organize the dataframe by ID
D_experimental_tall = D_experimental_tall[order(D_experimental_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_experimental_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_experimental_tall$trial = rep(c("experimental"), times = 96)

# create 'object' column
D_experimental_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_experimental_tall$row.names = NULL

# reorder columns 
D_experimental_tall = D_experimental_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = BB.A.control,
                           BB.B.control = BB.B.control, BB.C.control = BB.C.control,
                           BB.D.control = BB.D.control, ISO.A.control = ISO.A.control,
                           ISO.B.control = ISO.B.control, ISO.C.control = ISO.C.control,
                           ISO.D.control = ISO.D.control)

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_experimental_tall,
               D_control_tall)
names(D_tall)

# convert variables to their proper type
names(D_tall)
str(D_tall)
D_tall$measure = as.numeric(D_tall$measure)
D_tall$condition = factor(D_tall$condition)
D_tall$trial = factor(D_tall$trial)
D_tall$objects = factor(D_tall$objects)
str(D_tall)

# figures
condition_barplot = ggplot(D_tall, aes(trial, measure, fill=objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  ylab("Ratings") + # change the label of the y-axis
  facet_wrap(~condition, ncol=2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                                      "#888888",
                                      "#C8C8C8",
                                      "#696969")) +
                                        
  coord_cartesian(ylim=c(0, 1.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank())





######################
######################
###### P = 0.95 ######
######################
######################
# BB
#experimental
BB.A.experimental = rep("1",16)
BB.B.experimental = rep("0.95",16) # 0.5+0.5
BB.C.experimental = rep("0.95",16) # 0.5+0.5


# control
BB.A.control = rep("0.95",16) # 0.5+0.5
BB.B.control = rep("0.95",16) # 0.5+0.5
BB.C.control = rep("0.95",16) # 0.5+0.5
BB.D.control = rep("1",16) # 0.5+0.5


# ISO
#experimental
ISO.A.experimental = rep("0",16)
ISO.B.experimental = rep("0.95",16) # 0.5+0.5
ISO.C.experimental = rep("0.95",16) # 0.5+0.5


# control
ISO.A.control = rep("0.95",16) # 0.5+0.5
ISO.B.control = rep("0.95",16) # 0.5+0.5
ISO.C.control = rep("0.95",16) # 0.5+0.5
ISO.D.control = rep("0",16) # 0.5+0.5



# create a dataframe that combines the variables
D.new.experimental = data.frame(BB.A.experimental = BB.A.experimental, BB.B.experimental = BB.B.experimental,
                        BB.C.experimental = BB.C.experimental, ISO.A.experimental = ISO.A.experimental, 
                        ISO.B.experimental = ISO.B.experimental, ISO.C.experimental = ISO.C.experimental)

D_experimental_tall =  reshape(D.new.experimental, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_experimental_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_experimental_tall$id = NULL

# organize the dataframe by ID
D_experimental_tall = D_experimental_tall[order(D_experimental_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_experimental_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_experimental_tall$trial = rep(c("experimental"), times = 96)

# create 'object' column
D_experimental_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_experimental_tall$row.names = NULL

# reorder columns 
D_experimental_tall = D_experimental_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = BB.A.control,
                           BB.B.control = BB.B.control, BB.C.control = BB.C.control,
                           BB.D.control = BB.D.control, ISO.A.control = ISO.A.control,
                           ISO.B.control = ISO.B.control, ISO.C.control = ISO.C.control,
                           ISO.D.control = ISO.D.control)

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_experimental_tall,
               D_control_tall)
names(D_tall)

# convert variables to their proper type
names(D_tall)
str(D_tall)
D_tall$measure = as.numeric(D_tall$measure)
D_tall$condition = factor(D_tall$condition)
D_tall$trial = factor(D_tall$trial)
D_tall$objects = factor(D_tall$objects)
str(D_tall)

# figures
condition_barplot = ggplot(D_tall, aes(trial, measure, fill=objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  ylab("Ratings") + # change the label of the y-axis
  facet_wrap(~condition, ncol=2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                                      "#888888",
                                      "#C8C8C8",
                                      "#696969")) +
                                        
  coord_cartesian(ylim=c(0, 1.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank())



######################
######################
###### P = 1    ######
######################
######################
# BB
#experimental
BB.A.experimental = rep("1",16)
BB.B.experimental = rep("1",16) # 0.5+0.5
BB.C.experimental = rep("1",16) # 0.5+0.5


# control
BB.A.control = rep("1",16) # 0.5+0.5
BB.B.control = rep("1",16) # 0.5+0.5
BB.C.control = rep("1",16) # 0.5+0.5
BB.D.control = rep("1",16) # 0.5+0.5


# ISO
#experimental
ISO.A.experimental = rep("0",16)
ISO.B.experimental = rep("1",16) # 0.5+0.5
ISO.C.experimental = rep("1",16) # 0.5+0.5


# control
ISO.A.control = rep("1",16) # 0.5+0.5
ISO.B.control = rep("1",16) # 0.5+0.5
ISO.C.control = rep("1",16) # 0.5+0.5
ISO.D.control = rep("0",16) # 0.5+0.5



# create a dataframe that combines the variables
D.new.experimental = data.frame(BB.A.experimental = BB.A.experimental, BB.B.experimental = BB.B.experimental,
                        BB.C.experimental = BB.C.experimental, ISO.A.experimental = ISO.A.experimental, 
                        ISO.B.experimental = ISO.B.experimental, ISO.C.experimental = ISO.C.experimental)

D_experimental_tall =  reshape(D.new.experimental, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_experimental_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_experimental_tall$id = NULL

# organize the dataframe by ID
D_experimental_tall = D_experimental_tall[order(D_experimental_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_experimental_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_experimental_tall$trial = rep(c("experimental"), times = 96)

# create 'object' column
D_experimental_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_experimental_tall$row.names = NULL

# reorder columns 
D_experimental_tall = D_experimental_tall[,c(3,4,1,5,2)]

#############
## control ##
#############
D.new.control = data.frame(BB.A.control = BB.A.control,
                           BB.B.control = BB.B.control, BB.C.control = BB.C.control,
                           BB.D.control = BB.D.control, ISO.A.control = ISO.A.control,
                           ISO.B.control = ISO.B.control, ISO.C.control = ISO.C.control,
                           ISO.D.control = ISO.D.control)

D_control_tall =  reshape(D.new.control, varying = c(1:8), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:16), times = 8)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 4, times = 16)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 128)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C","D"), times = 32)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]


# combine the dataframes
D_tall = rbind(D_experimental_tall,
               D_control_tall)
names(D_tall)

# convert variables to their proper type
names(D_tall)
str(D_tall)
D_tall$measure = as.numeric(D_tall$measure)
D_tall$condition = factor(D_tall$condition)
D_tall$trial = factor(D_tall$trial)
D_tall$objects = factor(D_tall$objects)
str(D_tall)

# figures
condition_barplot = ggplot(D_tall, aes(trial, measure, fill=objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  ylab("Ratings") + # change the label of the y-axis
  facet_wrap(~condition, ncol=2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                                      "#888888",
                                      "#C8C8C8",
                                      "#696969")) +
                                        
  coord_cartesian(ylim=c(0, 1.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank())

