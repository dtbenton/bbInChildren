################################################################
################################################################
################################################################
### COMPARE THE NN AND BAYESIAN MODEL FOR THEIR OVERALL FITS ###
################################################################
################################################################
################################################################
# LOAD REQUIRED PACKAGE
library(caret)

#### ----> BEHAVIORAL DATA <---- ####

# NOTE: This assumes you've loaded the data from the 'exp2RscriptNEW.R' datafile

# create objects for the means of different objects across the BB and ISO conditions
# BB
A.main.bb = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$trialType=="main" & D_tall$Condition=="Backwards Blocking"])
B.main.bb = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$trialType=="main" & D_tall$Condition=="Backwards Blocking"])
C.main.bb = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$trialType=="main" & D_tall$Condition=="Backwards Blocking"])

A.control.bb = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$trialType=="control" & D_tall$Condition=="Backwards Blocking"])
B.control.bb = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$trialType=="control" & D_tall$Condition=="Backwards Blocking"])
C.control.bb = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$trialType=="control" & D_tall$Condition=="Backwards Blocking"])
D.control.bb = mean(D_tall$choice[D_tall$objectType=="D" & D_tall$trialType=="control" & D_tall$Condition=="Backwards Blocking"])
E.control.bb = mean(D_tall$choice[D_tall$objectType=="E" & D_tall$trialType=="control" & D_tall$Condition=="Backwards Blocking"])


# ISO
A.main.iso = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$trialType=="main" & D_tall$Condition=="Indirect Screening-Off"])
B.main.iso = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$trialType=="main" & D_tall$Condition=="Indirect Screening-Off"])
C.main.iso = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$trialType=="main" & D_tall$Condition=="Indirect Screening-Off"])

A.control.iso = mean(D_tall$choice[D_tall$objectType=="A" & D_tall$trialType=="control" & D_tall$Condition=="Indirect Screening-Off"])
B.control.iso = mean(D_tall$choice[D_tall$objectType=="B" & D_tall$trialType=="control" & D_tall$Condition=="Indirect Screening-Off"])
C.control.iso = mean(D_tall$choice[D_tall$objectType=="C" & D_tall$trialType=="control" & D_tall$Condition=="Indirect Screening-Off"])
D.control.iso = mean(D_tall$choice[D_tall$objectType=="D" & D_tall$trialType=="control" & D_tall$Condition=="Indirect Screening-Off"])
E.control.iso = mean(D_tall$choice[D_tall$objectType=="E" & D_tall$trialType=="control" & D_tall$Condition=="Indirect Screening-Off"])

# create an object called 'behavioral_data' that stores the means across columns

behavioral_data = c(A.main.bb, B.main.bb, C.main.bb, A.control.bb, B.control.bb, C.control.bb, D.control.bb, E.control.bb,
                    A.main.iso, B.main.iso, C.main.iso, A.control.iso, B.control.iso, C.control.iso, D.control.iso, E.control.iso)


#### ----> NEURAL NETWORK AND MODEL DATA COMPARISON <---- ####
folder_path <- "C:/Users/bentod2/Documents/projects/current/bbInChildren/model/NNModelWithRandomWeights/exp2/modelData"

files <- list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

# Initialize a data frame to hold the results
results <- data.frame(RMSE = numeric(), MAE = numeric())

for (file in files) {
  data <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  data$condition <- rep(c("Backwards Blocking", "Indirect Screening-Off"), each = 1600)
  data$objects <- data$V2
  data$trialtype <- rep(c("main", "control", "main", "control"), times = c(600, 1000, 600, 1000))
  
  # BB
  A.BB.main <- mean(data$V3[data$trialtype=="main" & data$objects=="A" & data$condition=="Backwards Blocking"])
  B.BB.main <- mean(data$V3[data$trialtype=="main" & data$objects=="B" & data$condition=="Backwards Blocking"])
  C.BB.main <- mean(data$V3[data$trialtype=="main" & data$objects=="C" & data$condition=="Backwards Blocking"])
  
  A.BB.control <- mean(data$V3[data$trialtype=="control" & data$objects=="A" & data$condition=="Backwards Blocking"])
  B.BB.control <- mean(data$V3[data$trialtype=="control" & data$objects=="B" & data$condition=="Backwards Blocking"])
  C.BB.control <- mean(data$V3[data$trialtype=="control" & data$objects=="C" & data$condition=="Backwards Blocking"])
  D.BB.control <- mean(data$V3[data$trialtype=="control" & data$objects=="D" & data$condition=="Backwards Blocking"])
  E.BB.control <- mean(data$V3[data$trialtype=="control" & data$objects=="E" & data$condition=="Backwards Blocking"])
  
  # ISO
  A.ISO.main <- mean(data$V3[data$trialtype=="main" & data$objects=="A" & data$condition=="Indirect Screening-Off"])
  B.ISO.main <- mean(data$V3[data$trialtype=="main" & data$objects=="B" & data$condition=="Indirect Screening-Off"])
  C.ISO.main <- mean(data$V3[data$trialtype=="main" & data$objects=="C" & data$condition=="Indirect Screening-Off"])
  
  A.ISO.control <- mean(data$V3[data$trialtype=="control" & data$objects=="A" & data$condition=="Indirect Screening-Off"])
  B.ISO.control <- mean(data$V3[data$trialtype=="control" & data$objects=="B" & data$condition=="Indirect Screening-Off"])
  C.ISO.control <- mean(data$V3[data$trialtype=="control" & data$objects=="C" & data$condition=="Indirect Screening-Off"])
  D.ISO.control <- mean(data$V3[data$trialtype=="control" & data$objects=="D" & data$condition=="Indirect Screening-Off"])
  E.ISO.control <- mean(data$V3[data$trialtype=="control" & data$objects=="E" & data$condition=="Indirect Screening-Off"])
  
  model_predictions <- c(A.BB.main, B.BB.main, C.BB.main, A.BB.control, B.BB.control, C.BB.control, D.BB.control, E.BB.control,
                         A.ISO.main, B.ISO.main, C.ISO.main, A.ISO.control, B.ISO.control, C.ISO.control, D.ISO.control, E.ISO.control)
  
  # Assuming you have the `behavioral_data` variable defined somewhere
  
  res <- postResample(pred = model_predictions, obs = behavioral_data)
  
  epoch_number <- tools::file_path_sans_ext(basename(file))
  
  results <- rbind(epoch_number, RMSE = res[["RMSE"]], MAE = res[["MAE"]])
  
  print(results)
}
