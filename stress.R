
library(readxl)
library(lattice)
library(corrplot)
library(MASS)
library(lme4)
library(car)


#### load the data 



data=read_xlsx('/Users/danhuang/Desktop/Desktop/Upwork/King 0/Final Dataset.xlsx')

head(data)

##################### First stage ###########################

### data preprocessing & data clean

## basic descriptive statistic: min, max, median, mean and quantiles
## for each variable in the dataset
summary(data)

### A correlation matrix is a table showing correlation coefficients
### between variables. In out correlation matrix including variables
## ALI in time 1, Age, LOS, MS1, SF1, SS1, PS1, it shows 
### there is positive linear relationship between Age and LOS because
### the correlation value is  0.8170164 as it is shown in correlation
### plot as well

corr=cor(data[,c('ALI1','Age','LOS','MS1','SF1',
                'SS1','PS1')])
corrplot(corr)


sum(is.na(data)) ## number of missing values in the dataset


## count missing values in each column in the dataset 

count_miss=rep(0,length(data))
for(i in 1:length(data)){
  count_miss[i]=sum(is.na(data[,i]))
}

index=which(count_miss>0)
miss_colname=rep(0,length(index))
for(i in 1:length(index)){
  miss_colname[i]=colnames(data[,index[i]])
}

### now we notice that there are some missing values in rank, Ed, 
## Rel.Stat, C19, MJ, PA, ALI2, MS2, SF2, SS2, PS2, AIF2, Slp2

### data visualizations for ALI 1 and ALI 2 across three categories
### C19, PA, and Rank

dotplot(data$ALI1~data$C19,xlab='C19',ylab='ALI in time 1')

dotplot(data$ALI2~data$C19,xlab='C19',ylab='ALI in time 2')

dotplot(data$ALI1~data$PA,xlab='PA',ylab='ALI in time 1')

dotplot(data$ALI2~data$PA,xlab='PA',ylab='ALI in time 2')

dotplot(data$ALI1~data$Rank,xlab='Rank',ylab='ALI in time 1')

dotplot(data$ALI2~data$Rank,xlab='Rank',ylab='ALI in time 2')


## check normality for ALI in time 1

### historgam of ALI in time 1 indicates ALI is not normally distributed 
hist(data$ALI1,col='steelblue', main='Non-normal',xlab = 'ALI in time 1')

### Q-Q plot exhibits ALI in time 1 that is not normally distributed.
qqnorm(data$ALI1, main='Non-normal')
qqline(data$ALI1)

## Shapiro test: null hypothesis: the data is normally distributed
### p-value is less than 0.05, which indicates that the ALI in time 1 
### is not normally distributed.
shapiro.test(data$ALI1)

### check normality for ALI in time 2

### historgam of ALI in time 2 indicates ALI is not normally distributed 
hist(data$ALI2,col='steelblue', main='Non-normal',xlab = 'ALI in time 2')

### Q-Q plot exhibits ALI in time 2 that is not normally distributed.
qqnorm(data$ALI2, main='Non-normal')
qqline(data$ALI2)

## Shapiro test: null hypothesis: the data is normally distributed
### p-value is less than 0.05, which indicates that the ALI in time 2 
### is not normally distributed.
shapiro.test(data$ALI2)

######################### Second stage ####################


               #### Anovas for ALI in time 1 for C19, PA, Rank

## One-way Anova
# we are modeling ALI in time 1 as a function with C19 or
# without C19.

# The p-value of the C19 variable is high (p > 0.05),
# so it appears that the C19 does not have a real 
# impact on the ALI in time 1.
one.way <- aov(ALI1 ~ C19, data = data)

summary(one.way)

## two-way anova

# Adding physical activity to the model seems to have made the model a
# little bit better:
# it reduced the residual variance (the residual sum of squares 
# went from 72.76 to 72.38), and both C19 and
# PA are not statistically significant (p-values > 0.05).

two.way <- aov(ALI1 ~ C19 + PA, data = data)

summary(two.way)

## Adding interactions between variables

### All variables C19, PA and interaction term are 
### not statistically significant (p-values > 0.05)

interaction <- aov(ALI1 ~ C19*PA, data = data)

summary(interaction)


### Multiple ANOVA


### All variables C19, PA and Rank are 
### not statistically significant (p-values > 0.05) 
### in predicting ALI in time 1
three.way <- aov(ALI1 ~ C19 + PA + Rank, data = data)

summary(three.way)



one.way <- aov(ALI1 ~ C19, data = data)

summary(one.way)

                #### Anovas for ALI in time 2 for C19, PA, Rank

### All variables C19, PA , Rank are not
## statistically significant in predicting ALI in time 2
one.way2 <- aov(ALI2 ~ C19, data = data)

summary(one.way2)

two.way2 <- aov(ALI2 ~ C19 + PA, data = data)

summary(two.way2)

interaction2 <- aov(ALI2 ~ C19*PA, data = data)

summary(interaction2)

three.way2 <- aov(ALI2 ~ C19 + PA + Rank, data = data)

summary(three.way2)


##### create dataframe for ALI in time 1

stress1=data[,c("ALI1","SS1","PS1","S","Age","LOS","Rank",
                "Ed","Rel.Stat","C19","MJ","PA","MS1","SF1",
                "AIF","Slp1")]

#### We model the ALI in time 1 as the function of all variables
## and found out no variable is statistically signicant to predict
## ALI in time 1

mod1=lm(ALI1~.,data = stress1)
summary(mod1)

### We use the backward method for variable selection and 
## found the "best" model with lowest AIC value that includes
### PS1, Age, Ed, MJ, SF1
stepAIC(mod1,direction="backward")


### We model the ALI in time 1 as the function of variables
### PS1, Age, Ed, MJ, SF1 and found out only PS1 is statistically 
## signicant to predict ALI in time 1

mod2=lm(ALI1 ~ PS1 + Age + Ed + MJ + SF1, data = stress1)

summary(mod2)


### We model the ALI in time 1 as the function of variables
### SS1, PS1, Age, LOS, MS1 and SF1 and found out only Age is
### statistically signicant to predict ALI in time 1

mod3=lm(ALI1 ~ SS1+PS1+Age+LOS+MS1+SF1, data=stress1)

summary(mod3)



##### create dataframe for ALI in time 2

# We did same precedure for ALI in time 2 and found out 
# in the "best" model with the lowest AIC value, SS2, S, Age, Ed,
# C19 and MS2 are statistically signicant to predict ALI in time 2.

stress2=data[,c("ALI2","SS2","PS2","S","Age","LOS","Rank",
                "Ed","Rel.Stat","C19","MJ","PA","MS2","SF2",
                "AIF2","Slp2")]

mod4=lm(ALI2~.,data = stress2)
summary(mod4)

stepAIC(mod4,direction="backward")

mod5=lm(ALI2 ~ SS2 + S + Age + Ed + C19 + MS2 + SF2, data = stress2)
summary(mod5)


################# Third Stage #############################

### create liner mixed model for ALI in time 1

# Based on the previous analysis, we notice that LOS is not statistically
# significant to predict ALI. And only few variables are statistically
# significant to predict ALI using multiple regression models. The PS1
# can predict the ALI in time 1 and SS2 can predict ALI in time 2.
# Because there are many outliers in this small sample size dataset. 
# In addition, the ALI is not normally distributed. 

# In this stage, we run the analysis for between-subject and with-subject.

# In this step,we were interested in ALI, the differences whether phycial 
# activity matters, how ranks work. Our random effect was with C19 and
# without C19. 

# it generated that warning because your random effects 
# are very small.


# Let's look at these results.We get an estimate of the variance explained by the random 
# effect. This number is important, because if it's indistinguishable 
# from zero, then your random effect probably doesn't matter and we 
# can go ahead and do a regular linear model instead. The variance of
# C19 is 0 and therefore the C19 is not statistically significant.
# Next we have estimates of the fixed effects, 
# with standard errors, t-value. And we get the p-values for the fixed
# effect and they are not statistically significant.

mix.mod1=lmer(ALI1 ~ PA + Rank + (1 |C19),data=stress1)

summary(mix.mod1)

Anova(mix.mod1)

## next we set the PA as the random effect and found out
# PA is not statistically significant. 

mix.mod2=lmer(ALI1 ~ C19 + Rank + (1 |PA),data=stress1)

summary(mix.mod2)

Anova(mix.mod2)

## next we set the Rank as the random effect and got the same
# result

mix.mod3=lmer(ALI1 ~ C19 + PA + (1 |Rank),data=stress1)

summary(mix.mod3)

Anova(mix.mod3)

## we add the SS1, PS1, Age as the fixed-effects and
## C19, PA, Rank as the random effects

# Let's look at these results.We get an estimate of the variance explained by the random 
# effect.  The variance of
# Rank and C19 is 0 and therefore the C19 and Rank are not statistically significant.
## The variance pf PA is also relatively small. 
# Next we have estimates of the fixed effects, 
# with standard errors, t-value. And we get the p-values for the fixed
# effect and only PS1 is statistically significant because p-value <0.05.



mix.mod4=lmer(ALI1 ~ SS1+PS1+ Age+(1 |C19) + (1 |PA) + (1 |Rank),data=stress1)

summary(mix.mod4)

Anova(mix.mod4)

### Next we add the interaction terms and dropped
## the random effects C19, PA, Rank based on the previous result

## we run the linear regression model with interaction terms
## and found out all variables are not statistically significant

mix.mod5=lm(ALI1 ~ PS1+PS1*SF1 ,data=stress1)

summary(mix.mod5)

### The results above are not ideal for us because our data does
# not follow normal distribution. The REML and maximum likelihood 
# methods for estimating the effect sizes in the model make 
# assumptions of normality that don't apply to our data.
# 
# we have to use a different method for parameter estimation. 

# Penalized quasilikelihood (PQL) is a flexible technique that 
# can deal with non-normal data, unbalanced design, and crossed 
# random effects. However, it produces biased estimates if your
# response variable fits a discrete count distribution, 
# like Poisson or binomial, and the mean is less than 5 - or
# if your response variable is binary.

# The ALI variable fits the log-normal distribution,which
# is not a discretized distribution. That means we can 
# proceed with the PQL method. But before we proceed, 
# let's return to the matter of transformation to normality. 
# The reason we want to use a GLMM for this is that if we
# imagine a stastical method as E(x), E(ln(x)) is not the same
# as ln(E(x)). The former is performing a LMM on a transformed
# variable, while the latter is performing a GLMM on an
# untransformed variable. The latter is better because 
# it better captures the variance of x.


# This model suggests that PS has an effect on ALI,
# that is, PS can predict the stress change over time. 


PQL=glmmPQL(ALI1 ~ SS1+PS1, ~1 | C19,
            family = gaussian(link = "log"),
        data = data, verbose = FALSE,start=c(0,0,0))

summary(PQL)

### set PA as random effect, get the same result

PQL=glmmPQL(ALI1 ~ SS1+PS1, ~1 | PA,
            family = gaussian(link = "log"),
            data = data, verbose = FALSE,start=c(0,0,0))

summary(PQL)

### set Rank as random effect, get the same result

PQL=glmmPQL(ALI1 ~ SS1+PS1, ~1 | Rank,
            family = gaussian(link = "log"),
            data = data, verbose = FALSE,start=c(0,0,0))

summary(PQL)

### add interaction term, no variables are statistically
## significant 

PQL=glmmPQL(ALI1 ~ SS1+PS1 + PS1*SF1, ~1 | Rank,
            family = gaussian(link = "log"),
            data = data, verbose = FALSE,start=c(0,0,0,0,0))

summary(PQL)

