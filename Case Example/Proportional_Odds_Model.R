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
dades_in_1week$IS <- cut(dades_in_1week$sportrec.hagos, breaks = c(0, 61, 84, 100), labels = c("stop", "attention", "go"))
dades_in_1week$IS_num <- cut(dades_in_1week$sportrec.hagos, breaks = c(0, 61, 84, 100), labels = c("3", "2", "1"))
#class(dades_in_1week$IS_num)

###Using covariable groin.problem.prev

mod1 <- polr(IS ~ groin.problem.prev, data = dades_in_1week, Hess = TRUE)
brant(mod1)
#value: 0.07 --> satisfies this proportional odds assumption because value > alpha (0.05)
summary(mod1)
#pvalues for all intercepts
summary_table <- coef(summary(mod1))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = pval)
summary_table


## Odds ratio and 95% CI
c(exp(coef(mod1)), exp(confint(mod1)))

## Likelihood ratio test
Anova(mod1, type = 3)

##Plot predicted probabilities of each outcome
plot(effect("groin.problem.prev", mod1), style = "stacked", colors = c("red", "yellow", "green"), 
     main = FALSE, xlab = "Groin Problem Prevalence", ylab = "Probability")

#predicted probabilities
predict(mod1,newdata = data.frame(groin.problem.prev="Yes, Groin Problem"),type="p")
predict(mod1,newdata = data.frame(groin.problem.prev="No groin problem"),type="p")

#Groin problem yes cumulative probabilities
exp(mod1$zeta - mod1$coefficients)/(1 + exp(mod1$zeta - mod1$coefficients))
#Groin problem no cumulative probabilities
exp(mod1$zeta)/(1 + exp(mod1$zeta))

#Odds for all J-1 levels for both Groin Problem categories
# GP yes cumulative probabilities
gpycp <- exp(mod1$zeta - mod1$coefficients)/(1 + exp(mod1$zeta - mod1$coefficients))
# GP no cumulative probabilities
gpncp <- exp(mod1$zeta)/(1 + exp(mod1$zeta))
# GP yes odds
(gpycp/(1-gpycp))
# GP no odds 
(gpncp/(1-gpncp))

#Now let's take the ratio of the GP yes  odds to the GP no  odds:
(gpycp/(1-gpycp))/(gpncp/(1-gpncp))
#(gpncp/(1-gpncp))/(gpycp/(1-gpycp))
#Players reporting groing problems have higher odds of having to stop.

#The categorical covariate GP Yes has a coefficient of -19.53 (CI 95%:-20.66 - -18.40); which means,
#the mean HAGOS score of GP Yes is -19.53 lower than the mean HAGOS score of the GP NO.


###Using covariable tl.vs.no.tl.gproblem

mod2 <- polr(IS ~ tl.vs.no.tl.gproblem, data = dades_in_1week, Hess = TRUE)
brant(mod2)
#value: 0.96 --> satisfies this proportional odds assumption because value > alpha (0.05)
summary(mod2)
#pvalues for all intercepts
summary_table2 <- coef(summary(mod2))
pval2 <- pnorm(abs(summary_table2[, "t value"]),lower.tail = FALSE)* 2
summary_table2 <- cbind(summary_table2, "p value" = pval2)
summary_table2


## Odds ratio and 95% CI
c(exp(coef(mod2)), exp(confint(mod2)))

##Likelihood ratio test
Anova(mod2, type = 3)
##Plot predicted probabilities of each outcome
plot(effect("tl.vs.no.tl.gproblem", mod2), style = "stacked", colors = c("red", "yellow", "green"), 
     main = FALSE, xlab = "Time-loss Groin Problem", ylab = "Probability")

#predicted probabilities
predict(mod2,newdata = data.frame(tl.vs.no.tl.gproblem="TL Groin Problem"),type="p")
predict(mod2,newdata = data.frame(tl.vs.no.tl.gproblem="No TL Groin Problem"),type="p")

#time loss Groin problem yes cumulative probabilities
exp(mod2$zeta - mod2$coefficients)/(1 + exp(mod2$zeta - mod2$coefficients))
#no time loss Groin problem yes cumulative probabilities
exp(mod2$zeta)/(1 + exp(mod2$zeta))

#Odds for all J-1 levels for both Groin Problem categories
# tl yes cumulative probabilities
tlgpycp <- exp(mod2$zeta - mod2$coefficients)/(1 + exp(mod2$zeta - mod2$coefficients))
# tl no cumulative probabilities
tlgpncp <- exp(mod2$zeta)/(1 + exp(mod2$zeta))
# tl yes odds
(tlgpycp/(1-tlgpycp))
# tl no odds 
(tlgpncp/(1-tlgpncp))

#Now let's take the ratio of the tk yes  odds to the tl no  odds:
(tlgpycp/(1-tlgpycp))/(tlgpncp/(1-tlgpncp))
#(tlgpncp/(1-tlgpncp))/(tlgpycp/(1-tlgpycp))
#Players reporting groing problems have higher odds of having to stop.

