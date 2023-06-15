rm(list = ls())

#Libraries
library(MASS)
library(nlme)
library(car)
library(lme4)
library(psycho)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(nortest)
library(Hmisc)
library(brant)
library(effects)

#Data download

dades<-spss.get("HAGOS_DATABASE_CLEAN.sav",lowernames=T)

summary(dades)
head(dades)


##Subset data 
dades_pre <- subset(dades, dades$week.c == "Pre-Season")
dades_in <- subset(dades, dades$week.c == "In-Season")

#Normality test

qqnorm(dades$sportrec.hagos)
qqline(dades$sportrec.hagos)
hist(dades$sportrec.hagos)
lillie.test(x=dades$sportrec.hagos)
#ks.test(dades$Dist.m.min)


###Classic Transformations like log, square and others. 

##-- Transformacions alternatives
dades$sportrec.hagos_t <- dades$sportrec.hagos+0.11
BC <- boxCox(lm(sportrec.hagos_t~1,dades))
lambda <- BC$x[which.max(BC$y)]
dades$sportrec.hagos_bc <- (dades$sportrec.hagos_t^(lambda)-1)/lambda
par(mfrow=c(1,2))
qqnorm(dades$sportrec.hagos_t)
qqline(dades$sportrec.hagos_t)
qqnorm(dades$sportrec.hagos_bc)
qqline(dades$sportrec.hagos_bc)


###IN-SEASON (1 week) 
###Ordinal Model (Proportional Odds Model) - Cross-Sectional

#Subset for players in-season and 1 week 

as.factor(dades$week)
as.factor(dades$id.player)
#which week
dades_in$week[max(table(dades_in$week))]
max(table(dades_in$week))

#subset 
dades_in_1week <- subset(dades_in, dades_in$week == 10)
#table(dades_in_1week$id.player)
table(dades_in_1week$sportrec.hagos)

##Classification HAGOS scores in subgroups based on the Copenhagen five-second squeeze score.
dades_in_1week$NRS <- cut(dades_in_1week$sportrec.hagos, breaks = c(0, 61, 84, 100), labels = c("stop", "attention", "go"))
dades_in_1week$NRS_num <- cut(dades_in_1week$sportrec.hagos, breaks = c(0, 61, 84, 100), labels = c("3", "2", "1"))
#class(dades_in_1week$NRS_num)

###Using covariable groin.problem.prev

mod1 <- polr(NRS ~ groin.problem.prev, data = dades_in_1week, Hess = TRUE)
summary(mod1)
brant(mod1)
#value: 0.07 --> satisfies this proportional odds assumption because value > alpha (0.05)

## Odds ratio and 95% CI
c(exp(coef(mod1)), exp(confint(mod1)))

## Likelihood ratio test
Anova(mod1, type = 3)

##Plot predicted probabilities of each outcome
plot(effect("groin.problem.prev", mod1), style = "stacked", colors = c("red", "yellow", "green"), 
     main = FALSE, xlab = "Groin Problem Prevalence", ylab = "Probability")

#The categorical covariate GP Yes has a coefficient of -19.53 (CI 95%:-20.66 - -18.40); which means,
#the mean HAGOS score of GP Yes is -19.53 lower than the mean HAGOS score of the GP NO.


###Using covariable tl.vs.no.tl.gproblem

mod2 <- polr(NRS ~ tl.vs.no.tl.gproblem, data = dades_in_1week, Hess = TRUE)
summary(mod2)
brant(mod2)
#value: 0.96 --> satisfies this proportional odds assumption because value > alpha (0.05)

## Odds ratio and 95% CI
c(exp(coef(mod2)), exp(confint(mod2)))

##Likelihood ratio test
Anova(mod2, type = 3)
##Plot predicted probabilities of each outcome
plot(effect("tl.vs.no.tl.gproblem", mod2), style = "stacked", colors = c("red", "yellow", "green"), 
     main = FALSE, xlab = "Time-loss Groin Problem", ylab = "Probability")



