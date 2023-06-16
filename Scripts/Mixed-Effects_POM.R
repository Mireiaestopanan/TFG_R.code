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
library(ordinal)
library(tidyverse)
library(ggeffects)
library(lsmeans)
library(sjPlot)
library(rms)

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


###IN-SEASON  
###Ordinal Model (Mixed-Effects Proportional Odds Model) - Longitudinal

#Subset for players in-season  

as.factor(dades$week)
as.factor(dades$id.player)


##Classification HAGOS scores in subgroups based on the Copenhagen five-second squeeze score.
dades_in$NRS <- cut(dades_in$sportrec.hagos, breaks = c(0, 61, 84, 100), labels = c("stop", "attention", "go"))
dades_in$NRS_num <- cut(dades_in$sportrec.hagos, breaks = c(0, 61, 84, 100), labels = c(3, 2, 1))
dades_in$NRS_num <- as.numeric(dades_in$NRS_num)
#class(dades_in$NRS_num)

################# MODELLING Using covariable groin.problem.prev ##############################################
mod3 <- clmm(NRS ~ groin.problem.prev + (week) + (1 | week) + (1 | id.player), data = dades_in)
summary(mod3)

#Checking proportional odds assumption for time
f <- lrm(NRS ~ rcs(week, 5), data=dades_in)
g <- Function(f)   # derive predicted log odds as a function of week
# Make another function that gets the OR vs. week=1
dor <- function(day) exp(g(day) - g(1))
windows()
propsPO(NRS ~ week, odds.ratio=dor, data=dades_in) +
  theme(legend.position='bottom')
#Proportional odds is reasonably well satisfied with regard to time 


################### CHECKING PROPORTIONAL ODDS ASSUMPTION FOR GROIN.PROBLEM.PREV ###########################

## 1) plotting the observed mean of the covariate against the expected mean implied by the proportional odds model
ql <- function(y) {
  p <- min(0.9999, max(0.0001, mean(y, na.rm=TRUE)))
  qlogis(p)
}
sf <- function(y) c(
  'y>=2'=ql(y >= 2),  # box      (attention)
  'y>=3'=ql(y >= 3))  # circle   (go)
s <- summary(NRS_num ~ groin.problem.prev + week, fun=sf, continuous=99, data=dades_in)
windows()
plot(s, which=1:2, pch=0:4, xlab='logit Cumulative Prob', main='')
# If the proportional odds assumption holds, for each predictor variable, 
#distance between the symbols for each set of categories of the dependent variable, should remain similar. 
#Therefore, in this case the proportional odds assumption holds
with(dades_in, print(table(week, NRS)))


## 2) Running two separate logistic regressions. Calculate the ORs for the differemt collapsing levels
# Collapse first and second categories of NRS
dades_in$NRS_collapsed1 <- ifelse(dades_in$NRS == "stop", 0, 1)
dades_in$NRS_collapsed1 <- ifelse(dades_in$NRS == "attention", 0, dades_in$NRS_collapsed1)
# Fit model with collapsed categories
dades_in$NRS_collapsed1<- factor(dades_in$NRS_collapsed1)
mod_collapsed1 <- clmm(NRS_collapsed1 ~ groin.problem.prev + (week) + (1 | week) + (1 | id.player), data = dades_in)
# Obtain coefficient estimates and standard errors
summary1 <- summary(mod_collapsed1)
# Calculate odds ratio and confidence interval for groin.problem.prev
OR1 <- exp(summary1$coefficients[2,1])
SE1 <- summary1$coefficients[2,2]
CI1 <- c(OR1 - 1.96*SE1, OR1 + 1.96*SE1)
# Collapse second and third categories of NRS
dades_in$NRS_collapsed2 <- ifelse(dades_in$NRS == "attention", 1, 0)
dades_in$NRS_collapsed2 <- ifelse(dades_in$NRS == "go", 1, dades_in$NRS_collapsed2)
# Fit model with collapsed categories
dades_in$NRS_collapsed2<- factor(dades_in$NRS_collapsed2)
mod_collapsed2 <- clmm(NRS_collapsed2 ~ groin.problem.prev + (week) + (1 | week) + (1 | id.player), data = dades_in)
# Obtain coefficient estimates and standard errors
summary2 <- summary(mod_collapsed2)
# Calculate odds ratio and confidence interval for groin.problem.prev
OR2 <- exp(summary2$coefficients[2,1])
SE2 <- summary2$coefficients[2,2]
CI2 <- c(OR2 - 1.96*SE2, OR2 + 1.96*SE2)
# Check if confidence intervals overlap
if (CI1[1] < CI2[2] & CI1[2] > CI2[1]) {
  print("Proportional odds assumption holds for groin.problem.prev")
} else {
  print("Proportional odds assumption does not hold for groin.problem.prev")
}
#Therefore, proportional odds assumption is met for groin.problem.prev

########################################### TEST FOR DIFFERENCES ##########################################
library(sjPlot)
#Post-Hoc tests to see if the differences are significant
lsmeans(mod3, pairwise~groin.problem.prev, adjust="tukey")
###################################### summary of the model ##################################################
sjPlot::tab_model(mod3)
#Predicted probabilities
plot_model(mod3, type = "pred", terms = c("groin.problem.prev"))


################################## MODELLING Using covariable tl.vs.no.tl.gproblem #############################################

mod4 <- clmm(NRS ~ tl.vs.no.tl.gproblem + (1 |week) +  (1 | id.player), data = dades_in)
summary(mod4)

############################################ Checking proportional odds assumption ################################################3

## 1)
s2 <- summary(NRS_num ~ tl.vs.no.tl.gproblem + factor(week), fun=sf, continuous=99, data=dades_in)
windows()
plot(s2, which=1:2, pch=0:4, xlab='logit Cumulative Prob', main='')

## 2)
mod_collapsed11 <- clmm(NRS_collapsed1 ~ tl.vs.no.tl.gproblem + (week) + (1 | week) + (1 | id.player), data = dades_in)
# Obtain coefficient estimates and standard errors
summary11 <- summary(mod_collapsed11)
# Calculate odds ratio and confidence interval for tl.vs.no.tl.gproblem
OR11 <- exp(summary11$coefficients[2,1])
SE11 <- summary11$coefficients[2,2]
CI11 <- c(OR11 - 1.96*SE11, OR11 + 1.96*SE11)
# Collapse second and third categories of NRS
# Fit model with collapsed categories
mod_collapsed22 <- clmm(NRS_collapsed2 ~ tl.vs.no.tl.gproblem + (week) + (1 | week) + (1 | id.player), data = dades_in)
# Obtain coefficient estimates and standard errors
summary22 <- summary(mod_collapsed22)
# Calculate odds ratio and confidence interval for tl.vs.no.tl.gproblem
OR22 <- exp(summary22$coefficients[2,1])
SE22 <- summary22$coefficients[2,2]
CI22 <- c(OR22 - 1.96*SE22, OR22 + 1.96*SE22)
# Check if confidence intervals overlap
if (CI11[1] < CI22[2] & CI11[2] > CI22[1]) {
  print("Proportional odds assumption holds for tl.vs.no.tl.gproblem")
} else {
  print("Proportional odds assumption does not hold for tl.vs.no.tl.gproblem")
}
#Therefore, proportional odds assumption is NOT met for tl.vs.no.tl.gproblem

########################################### TEST FOR DIFFERENCES ##########################################
#Post-Hoc tests to see if the differences are significant
lsmeans(mod4, pairwise~tl.vs.no.tl.gproblem, adjust="tukey")
###################################### summary of the model ##################################################
sjPlot::tab_model(mod4)
#Predicted probabilities
plot_model(mod4, type = "pred", terms = c("tl.vs.no.tl.gproblem"))


