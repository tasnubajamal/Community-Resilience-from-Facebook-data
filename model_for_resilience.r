df <- read.csv("C:\\Users\\ta444503\\OneDrive - Knights - University of Central Florida\\Desktop\\Spring 2022\\Algorithm\\ida\\data\\merged dataset\\data_for_R3.csv")
#df <- read.csv("C:\\Users\\ta444503\\OneDrive - Knights - University of Central Florida\\Desktop\\Spring 2022\\Algorithm\\ida\\data\\merged dataset\\data_trial.csv")
#df <- read.csv("C:\\Users\\ta444503\\OneDrive - Knights - University of Central Florida\\Desktop\\Spring 2022\\Algorithm\\ida\\data\\merged dataset\\fema_remove_zero.csv")
attach(df)
names(df)


#seven.rename(columns={'Loss of Resilience':'Y'}, inplace=True)
#seven.rename(columns={'Property Damage':'c'}, inplace=True)
#seven.rename(columns={'Duration of Road Blockage':'a'}, inplace=True)
#seven.rename(columns={'Distance to Hurricane Path':'d'}, inplace=True)
#seven.rename(columns={'Restoration Time for Power Outage': 'b'}, inplace=True)
#seven.rename(columns={'Median Household Income':'e'}, inplace=True)
#seven.rename(columns={'% of Black population': 'f'}, inplace=True)

library(nlme)
library(lme4)

df$a2 <- scale(df$a, center = TRUE, scale = TRUE)
df$b2 <- scale(df$b, center = TRUE, scale = TRUE)
df$c2 <- scale(df$c, center = TRUE, scale = TRUE)
df$d2 <- scale(df$d, center = TRUE, scale = TRUE)
df$e2 <- scale(df$e, center = TRUE, scale = TRUE)
df$f2 <- scale(df$f, center = TRUE, scale = TRUE)
df$h2 <- scale(df$Housings, center = TRUE, scale = TRUE)
df$ha2 <- scale(df$House_age, center = TRUE, scale = TRUE)
df$His2 <- scale(df$Hispanic, center = TRUE, scale = TRUE)
df$A <- scale(df$RPL_THEME1, center = TRUE, scale = TRUE)
df$B <- scale(df$RPL_THEME2, center = TRUE, scale = TRUE)
df$C <- scale(df$RPL_THEME3, center = TRUE, scale = TRUE)
df$D <- scale(df$RPL_THEME4, center = TRUE, scale = TRUE)
df$E <- scale(df$RPL_THEMES, center = TRUE, scale = TRUE)
#df$A <- scale(df$Not_jobless, center = TRUE, scale = TRUE)
#df$B <- scale(df$Commute_time, center = TRUE, scale = TRUE)
#df$C <- scale(df$Below_poverty, center = TRUE, scale = TRUE)
#df$D <- scale(df$Bachelor, center = TRUE, scale = TRUE)
#df$E <- scale(df$Disability, center = TRUE, scale = TRUE)
#df$F <- scale(df$Internet, center = TRUE, scale = TRUE)
#df$G <- scale(df$No_vehicle, center = TRUE, scale = TRUE)
#df$H <- scale(df$Mobile_home, center = TRUE, scale = TRUE)
#df$D <- scale(df$RPL_THEME4, center = TRUE, scale = TRUE)
#df$E <- scale(df$RPL_THEMES, center = TRUE, scale = TRUE)

library(car)
#Anova(lmm)

library(MuMIn)
#plot(density(df$Y))
lmm2 <- glmer(Y ~ a2 +b2 +c2+ d2 + e2 +f2 +ha2 +His2 + (1 | coun), data = df, family = Gamma(link = 'log'))

#lmm2 <- glmer(Y ~ a2 +b2 +c2+f2 +His2 + A+B+C+D+(1 | coun), data = df, family = Gamma(link = 'log')) #when use svi


#lmm2 <- glmer(Y ~ a2  +  b2 +c2+ d2 + e2 +f2 +ha2 +His2 + (1 | coun), data = df, family = Gamma(link = 'log'),
#              control = glmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
#model <- lmer(log(Y) ~ a2  +  b2 +c2+ d2 + e2 +f2 +ha2 +His2 + (1|coun), data = df)

summary(lmm2)

r.squaredGLMM(lmm2)

library(sjPlot)
library(ggplot2)
plot_model(lmm2, type = "pred", terms = "b2")
plot_model(lmm2, type = "pred", terms = "a2")
plot_model(lmm2, type = "pred", terms = "d2")
plot_model(lmm2, type = "pred", terms = "c2")

# Fractional logit model
#install.packages("scales")                              # Install & load scales
library(scales)

df$stY <- rescale(df$Y)
#myfrm <- frm(y, x, linkfrac = 'logit')

library(foreign)
library(frm)
library(sandwich)
library(lmtest)

# Fractional logit model
x <- df[,c('a2', 'b2', 'c2', 'd2','e2','f2','ha2','His2')]

library("multiColl")
CN(x)
y <- df$stY
myfrm <- frm(y,x, data = df, linkfrac = 'logit')

library("multiColl")
CN(x)

# SUR model

# using systemfit
library(systemfit)

x1 <- cbind(df$a2,df$b2,df$c2,df$d2)
x2 <- cbind(df$a2,df$c2,df$d2)
#x3 <- cbind(df$a2,df$c2,df$d2)

Y1 <- df$Y
Y2 <- df$b2

eq1 <- Y1 ~ x1
eq2 <- Y1 ~ x2
eq3 <- Y2 ~ x2

system <- list(eq1 = eq1, eq2 = eq2)
system2 <- list(eq1 = eq1, eq3 = eq3)

sur <- systemfit(system, method = 'SUR', data = df)
sur2 <- systemfit(system2, method = 'SUR', data = df)

summary(sur)
summary(sur2)

library(car)

restriction <- "eq1_x14 - eq2_x23"
linearHypothesis(sur,restriction, test = "Chisq")


#with new dataset with 146 observations


df <- read.csv("C:\\Users\\ta444503\\OneDrive - Knights - University of Central Florida\\Desktop\\Spring 2022\\Algorithm\\ida\\data\\merged dataset\\fema_remove_zero.csv")
attach(df)
names(df)


#seven.rename(columns={'Loss of Resilience':'Y'}, inplace=True)
#seven.rename(columns={'Property Damage':'c'}, inplace=True)
#seven.rename(columns={'Duration of Road Blockage':'a'}, inplace=True)
#seven.rename(columns={'Distance to Hurricane Path':'d'}, inplace=True)
#seven.rename(columns={'Restoration Time for Power Outage': 'b'}, inplace=True)
#seven.rename(columns={'Median Household Income':'e'}, inplace=True)
#seven.rename(columns={'% of Black population': 'f'}, inplace=True)

library(nlme)
library(lme4)

df$a2 <- scale(df$a, center = TRUE, scale = TRUE)
df$b2 <- scale(df$b, center = TRUE, scale = TRUE)
df$c2 <- scale(df$c, center = TRUE, scale = TRUE)
df$d2 <- scale(df$d, center = TRUE, scale = TRUE)
df$e2 <- scale(df$e, center = TRUE, scale = TRUE)
df$f2 <- scale(df$f, center = TRUE, scale = TRUE)
df$h2 <- scale(df$Housings, center = TRUE, scale = TRUE)
df$ha2 <- scale(df$House_age, center = TRUE, scale = TRUE)
df$His2 <- scale(df$Hispanic, center = TRUE, scale = TRUE)

library(car)
#Anova(lmm)

library(MuMIn)

lmm2 <- glmer(Y ~ a2  +  b2 +c2+ d2 + e2 +f2 +ha2 +His2 + (1 | coun), data = df, family = Gamma(link = "log"))

summary(lmm2)

r.squaredGLMM(lmm2)

library(corrplot)
cor.table = cor(df)
corrplot(cor.table)

