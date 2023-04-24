############################
# EXPERIMENT 1 MODEL DATA #
###########################
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


# load data
D = read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# get names of columns
names(D)

# get structure of data
str(D)

# convert variables
D$trial = factor(D$trial)
D$phase = factor(D$phase)
D$object = factor(D$object)
D$choice = as.numeric(D$choice)

# change level names of D$trial
levels(D$trial) = c("control","experimental")
levels(D$trial)

# BB trial objects #
# experimental
BB.A.experimental = D$choice[D$object=="A" & D$trial=="experimental" & D$condition=="BB"]

BB.B.experimental = D$choice[D$object=="B" & D$trial=="experimental" & D$condition=="BB"]

BB.C.experimental = D$choice[D$object=="C" & D$trial=="experimental" & D$condition=="BB"]


# control
BB.A.control = D$choice[D$object=="A" & D$trial=="control" & D$condition=="BB"]

BB.B.control = D$choice[D$object=="B" & D$trial=="control" & D$condition=="BB"]

BB.C.control = D$choice[D$object=="C" & D$trial=="control" & D$condition=="BB"]

BB.D.control = D$choice[D$object=="D" & D$trial=="control" & D$condition=="BB"]



## ISO trial objects ##
# experimental
ISO.A.experimental = D$choice[D$object=="A" & D$trial=="experimental" & D$condition=="ISO"]

ISO.B.experimental = D$choice[D$object=="B" & D$trial=="experimental" & D$condition=="ISO"]

ISO.C.experimental = D$choice[D$object=="C" & D$trial=="experimental" & D$condition=="ISO"]


# control
ISO.A.control = D$choice[D$object=="A" & D$trial=="control" & D$condition=="ISO"]

ISO.B.control = D$choice[D$object=="B" & D$trial=="control" & D$condition=="ISO"]

ISO.C.control = D$choice[D$object=="C" & D$trial=="control" & D$condition=="ISO"]

ISO.D.control = D$choice[D$object=="D" & D$trial=="control" & D$condition=="ISO"]

## BB AND ISO EXPERIMENTAL

# create new data frame from new columns 
D.new.experimental = data.frame(BB.A.experimental = BB.A.experimental, BB.B.experimental = BB.B.experimental,
                        BB.C.experimental = BB.C.experimental, ISO.A.experimental = ISO.A.experimental, 
                        ISO.B.experimental = ISO.B.experimental, ISO.C.experimental = ISO.C.experimental)

D_experimental_tall =  reshape(D.new.experimental, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_experimental_tall$ID = rep(c(1:200), times = 6)

# remove 'id' column from the reshape() function
D_experimental_tall$id = NULL

# organize the dataframe by ID
D_experimental_tall = D_experimental_tall[order(D_experimental_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_experimental_tall$condition = rep(c("BB","ISO"), each = 3, times = 200)

# create a 'trial' column
D_experimental_tall$trial = rep(c("experimental"), times = 1200)

# create 'object' column
D_experimental_tall$objects = rep(c("A","B","C"), times = 400)

# remove 'row.names' column 
D_experimental_tall$row.names = NULL

# reorder columns 
D_experimental_tall = D_experimental_tall[,c(3,4,1,5,2)]

## BB AND ISO CONTROL

# create new data frame from new columns 
D.new.control = data.frame(BB.A.control = BB.A.control, BB.B.control = BB.B.control,
                           BB.C.control = BB.C.control, ISO.A.control = ISO.A.control, 
                           ISO.B.control = ISO.B.control, ISO.C.control = ISO.C.control)

D_control_tall =  reshape(D.new.control, varying = c(1:6), v.names = "measure", 
                          timevar = "condition",   direction = "long")
D_control_tall$ID = rep(c(1:200), times = 6)

# remove 'id' column from the reshape() function
D_control_tall$id = NULL

# organize the dataframe by ID
D_control_tall = D_control_tall[order(D_control_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_control_tall$condition = rep(c("BB","ISO"), each = 3, times = 200)

# create a 'trial' column
D_control_tall$trial = rep(c("control"), times = 1200)

# create 'object' column
D_control_tall$objects = rep(c("A","B","C"), times = 400)

# remove 'row.names' column 
D_control_tall$row.names = NULL

# reorder columns 
D_control_tall = D_control_tall[,c(3,4,1,5,2)]



# combine the dataframes
D_tall = rbind(D_experimental_tall,
               D_control_tall)
fix(D_tall)
names(D_tall)

# plot data
condition_barplot = ggplot(D_tall, aes(trial, measure, fill = objects)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
condition_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  facet_wrap(~condition) +
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("#000000",
                             "#888888",
                             "#C8C8C8",
                             "#696969",
                             "#548548")) +
  
  coord_cartesian(ylim=c(0, 0.95)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank())