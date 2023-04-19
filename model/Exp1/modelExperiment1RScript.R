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

# BB trial objects #
# main
BB.A.main = D$choice[D$object=="A" & D$phase=="Phase 1" & D$trial=="main" & D$condition=="BB"]+
  D$choice[D$object=="A" & D$phase=="Phase 2" & D$trial=="main" & D$condition=="BB"]

BB.B.main = D$choice[D$object=="B" & D$phase=="Phase 1" & D$trial=="main" & D$condition=="BB"]+
  D$choice[D$object=="B" & D$phase=="Phase 2" & D$trial=="main" & D$condition=="BB"]

BB.C.main = D$choice[D$object=="C" & D$phase=="Phase 1" & D$trial=="main" & D$condition=="BB"]+
  D$choice[D$object=="C" & D$phase=="Phase 2" & D$trial=="main" & D$condition=="BB"]


# control
BB.A.control = D$choice[D$object=="A" & D$phase=="Phase 1" & D$trial=="control" & D$condition=="BB"]+
  D$choice[D$object=="A" & D$phase=="Phase 2" & D$trial=="control" & D$condition=="BB"]

BB.B.control = D$choice[D$object=="B" & D$phase=="Phase 1" & D$trial=="control" & D$condition=="BB"]+
  D$choice[D$object=="B" & D$phase=="Phase 2" & D$trial=="control" & D$condition=="BB"]

BB.C.control = D$choice[D$object=="C" & D$phase=="Phase 1" & D$trial=="control" & D$condition=="BB"]+
  D$choice[D$object=="C" & D$phase=="Phase 2" & D$trial=="control" & D$condition=="BB"]

BB.D.control = D$choice[D$object=="D" & D$phase=="Phase 1" & D$trial=="control" & D$condition=="BB"]+
  D$choice[D$object=="D" & D$phase=="Phase 2" & D$trial=="control" & D$condition=="BB"]



## ISO trial objects ##
# main
ISO.A.main = D$choice[D$object=="A" & D$phase=="Phase 1" & D$trial=="main" & D$condition=="ISO"]+
  D$choice[D$object=="A" & D$phase=="Phase 2" & D$trial=="main" & D$condition=="ISO"]

ISO.B.main = D$choice[D$object=="B" & D$phase=="Phase 1" & D$trial=="main" & D$condition=="ISO"]+
  D$choice[D$object=="B" & D$phase=="Phase 2" & D$trial=="main" & D$condition=="ISO"]

ISO.C.main = D$choice[D$object=="C" & D$phase=="Phase 1" & D$trial=="main" & D$condition=="ISO"]+
  D$choice[D$object=="C" & D$phase=="Phase 2" & D$trial=="main" & D$condition=="ISO"]


# control
ISO.A.control = D$choice[D$object=="A" & D$phase=="Phase 1" & D$trial=="control" & D$condition=="ISO"]+
  D$choice[D$object=="A" & D$phase=="Phase 2" & D$trial=="control" & D$condition=="ISO"]

ISO.B.control = D$choice[D$object=="B" & D$phase=="Phase 1" & D$trial=="control" & D$condition=="ISO"]+
  D$choice[D$object=="B" & D$phase=="Phase 2" & D$trial=="control" & D$condition=="ISO"]

ISO.C.control = D$choice[D$object=="C" & D$phase=="Phase 1" & D$trial=="control" & D$condition=="ISO"]+
  D$choice[D$object=="C" & D$phase=="Phase 2" & D$trial=="control" & D$condition=="ISO"]

ISO.D.control = D$choice[D$object=="D" & D$phase=="Phase 1" & D$trial=="control" & D$condition=="ISO"]+
  D$choice[D$object=="D" & D$phase=="Phase 2" & D$trial=="control" & D$condition=="ISO"]

# create new data frame from new columns 
D.new.main = data.frame(BB.A.main = BB.A.main, BB.B.main = BB.B.main,
                        BB.C.main = BB.C.main, ISO.A.main = ISO.A.main, 
                        ISO.B.main = ISO.B.main, ISO.C.main = ISO.C.main)

D_main_tall =  reshape(D.new.main, varying = c(1:6), v.names = "measure", 
                       timevar = "condition",   direction = "long")
D_main_tall$ID = rep(c(1:16), times = 6)

# remove 'id' column from the reshape() function
D_main_tall$id = NULL

# organize the dataframe by ID
D_main_tall = D_main_tall[order(D_main_tall$ID),] 

# replace the 'condition' column with a more appropriate one
D_main_tall$condition = rep(c("BB","ISO"), each = 3, times = 16)

# create a 'trial' column
D_main_tall$trial = rep(c("main"), times = 96)

# create 'object' column
D_main_tall$objects = rep(c("A","B","C"), times = 32)

# remove 'row.names' column 
D_main_tall$row.names = NULL

# reorder columns 
D_main_tall = D_main_tall[,c(3,4,1,5,2)]



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
D_tall = rbind(D_main_tall,
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
  
  coord_cartesian(ylim=c(0, 2.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))