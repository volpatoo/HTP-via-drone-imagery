setwd()

#######PH adjusted according to the flight means#######
eBee1Y<-read.csv("Y1_A7_blues_eBee_FLY.csv") ## reads the raw plant height data
str(eBee1Y)

library(lme4)
library(emmeans)
library(dplyr)


eBee1Y$FLY<-factor(eBee1Y$FLY)
eBee1Y$PLOT<-factor(eBee1Y$PLOT)

fit0 <- lm(PH_25_DTA66_eBee_Yi_A7 ~ PLOT + FLY, data = eBee1Y)
summary(fit0)
print(fit0)
plot(Mean_FLY)

anova(fit0)

Mean_FLY<-emmeans(fit0, specs = ~ PLOT)

write.csv(Mean_FLY,"Mean_FLY.csv", row.names = T)


eBee1Y<-read.csv("lmFLY_Y1_A7_eBee.csv")
str(eBee1Y)

eBee1Y$GID<-factor(eBee1Y$GID) ## these need to be set as factors for asreml
eBee1Y$REP<-factor(eBee1Y$REP)
eBee1Y$SUB<-factor(eBee1Y$SUB)
eBee1Y$FLY<-factor(eBee1Y$FLY)

eBee1Y<-read.csv("Y1_A7_blues_eBee_FLY.csv")
fit <- lmer(PH_25_DTA66_eBee_Yi_A7 ~ GID + (1|FLY) + (1|SUB %in% REP), data= eBee1Y) #Means adjusted to the number of flights (input to the prediction model)
summary(fit)

summary(fit)
isSingular(fit, tol = 1e-05)

emmeans(fit, specs = ~ GID) # marginal
emmeans(fit, specs = ~ GID:SITE) # aninhada