# helper functions #
refLevFunc = function(object,condition){
  D_tall$objectType = relevel(D_tall$objectType, ref = object)
  lmer.fit = lmer(choice~(AgeNum+objectType)^3+(1|ID), 
                                 data=D_tall[D_tall$Condition==condition,])
  coefs = data.frame(coef(summary(lmer.fit)))
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value))) # a column where the p-value is computed from the z-distribution
  print(coefs) 
  print(summary(lmer.fit))
}