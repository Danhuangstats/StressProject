

library(readxl)
library(lattice)
library(corrplot)
library(MASS)
library(lme4)
library(car)


data=read_xlsx('/Users/danhuang/Desktop/Desktop/Upwork/April2022/King 0/Dataset.xlsx')


### linear mixed model with random effect TIME and fixed effect LOS
mix.mod1=lmer(ALI ~ LOS +  (1 |TIME),data=data)

summary(mix.mod1)

Anova(mix.mod1)


### linear mixed model with random effect TIME and fixed effect SOOS

mix.mod2=lmer(ALI ~ SS +  (1 |TIME),data=data)

summary(mix.mod2)

Anova(mix.mod2)


### linear mixed model with random effect TIME and fixed effect PSS

mix.mod3=lmer(ALI ~ PS +  (1 |TIME),data=data)

summary(mix.mod3)

Anova(mix.mod3)


### linear mixed model with random effect TIME and fixed effect SOOS
### PSS and LOS

mix.mod4=lmer(ALI ~ PS+ LOS+SS +  (1 |TIME),data=data)

summary(mix.mod4)

Anova(mix.mod4)



