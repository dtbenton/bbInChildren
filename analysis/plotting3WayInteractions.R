D.5.bb = subset(D_tall, ! Age %in% c("6"))
D.5.bb = subset(D_tall, ! Condition %in% c("Indirect Screening-Off"))

ggplot(D.5.bb, aes(x = AgeNum, y = choice, color = objectType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

D.5.iso = subset(D_tall, ! Age %in% c("6"))
D.5.iso = subset(D_tall, ! Condition %in% c("Backwards Blocking"))

ggplot(D.5.iso, aes(x = AgeNum, y = choice, color = objectType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# try to see if two-way interaction between condition and object type is 
# significant for the two different ages
D.5 = subset(D_tall, ! Age %in% c("6"))
D.6 = subset(D_tall, ! Age %in% c("5"))

D.6.lmer = lmer(choice~(AgeNum+Condition+trialType+phaseOrder+objectType)^5+(1|ID), 
     data=D_tall)


ggplot(D_tall, aes(x = AgeNum, y = choice, color = objectType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Condition)


D_tall$objectType = relevel(D_tall$objectType, ref = "B")
first.three.way.BB.lmer = lmer(choice~(AgeNum+objectType)^3+(1|ID), 
                               data=D_tall[D_tall$Condition=="Backwards Blocking",])
summary(first.three.way.BB.lmer)
Anova(first.three.way.BB.lmer)