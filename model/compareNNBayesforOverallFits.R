##############################################################
##############################################################
## COMPARE THE NN AND BAYESIAN MODEL FOR THEIR OVERALL FITS ##
##############################################################
##############################################################

# Overall fit # 

# RMSE
nn_rmse = c(.1,.13,.15,.17,.18)
mean(nn_rmse)

bayes_rmse = c(.21,.16,.15,.22,.25)
mean(bayes_rmse)

t.test(nn_rmse, bayes_rmse, paired = FALSE,
       alternative = "two.sided")

# MAE
nn_mae = c(.07,.09,.11,.14,.15)
mean(nn_mae)

bayes_mae = c(.19,.13,.10,.19,.22)
mean(bayes_mae)

t.test(nn_mae, bayes_mae, paired = FALSE,
       alternative = "two.sided")




# BB Condition fit # 

# RMSE
nn_rmse = c(.14,.18,.19,.22,.23)
mean(nn_rmse)

bayes_rmse = c(.19,.10,.14,.26,.31)
mean(bayes_rmse)

t.test(nn_rmse, bayes_rmse, paired = FALSE,
       alternative = "two.sided")

# MAE
nn_mae = c(.09,.15,.16,.19,.20)
mean(nn_mae)

bayes_mae = c(.17,.10,.09,.24,.29)
mean(bayes_mae)

t.test(nn_mae, bayes_mae, paired = FALSE,
       alternative = "two.sided")



# ISO Condition fit # 

# RMSE
nn_rmse = c(.04,.06,.08,.09,.11)
mean(nn_rmse)

bayes_rmse = c(.22,.13,.14,.22,.19)
mean(bayes_rmse)

t.test(nn_rmse, bayes_rmse, paired = FALSE,
       alternative = "two.sided")

# MAE
nn_mae = c(.04,.05,.06,.09,.10)
mean(nn_mae)

bayes_mae = c(.21,.11,.13,.19,.18)
mean(bayes_mae)

t.test(nn_mae, bayes_mae, paired = FALSE,
       alternative = "two.sided")



# Experimental Condition fit # 

# RMSE
nn_rmse = c(.13,.18,.19,.21,.22)
mean(nn_rmse)

bayes_rmse = c(.13,.12,.17,.26,.29)
mean(bayes_rmse)

t.test(nn_rmse, bayes_rmse, paired = FALSE,
       alternative = "two.sided")

# MAE
nn_mae = c(.11,.15,.16,.18,.19)
mean(nn_mae)

bayes_mae = c(.11,.08,.14,.23,.26)
mean(bayes_mae)

t.test(nn_mae, bayes_mae, paired = FALSE,
       alternative = "two.sided")


# Control Condition fit # 

# RMSE
nn_rmse = c(.07,.08,.10,.12,.13)
mean(nn_rmse)

bayes_rmse = c(.26,.19,.14,.18,.21)
mean(bayes_rmse)

t.test(nn_rmse, bayes_rmse, paired = FALSE,
       alternative = "two.sided")

# MAE
nn_mae = c(.05,.06,.07,.09,.11)
mean(nn_mae)

bayes_mae = c(.25,.17,.08,.16,.19)
mean(bayes_mae)

t.test(nn_mae, bayes_mae, paired = FALSE,
       alternative = "two.sided")