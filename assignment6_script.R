##Loading Packages
library(tidyverse)
library(stargazer)

#loading data
#mac
NESdata <- read_excel("assign6data.xlsx")
summary(NESdata)
#pc data <- read_excel("~/Classes/POLS205/Assignment6/assign6data.xlsx")

##Part 1: Is an adult American's political ideology related to his/her feelings of warmth 
##towards Barak Obama? Answer this question by estimating and interpreting the 
##appropriate bivariate linear regression model.
modelA <- lm(libcons ~ obamaft, data = NESdata, na.action=na.exclude)
summary(modelA)
stargazer(modelA, title = "Political Ideology & Warmth Towards President Obama", align = TRUE)

##What about an adult American's opinion about the environment-jobs tradeoff? Again,
#estimate and interpret the appropriate (bivariate) linear regression model.


modelB <- lm(libcons ~ environ, data = NESdata, na.action=na.exclude)
summary(modelB)
stargazer(modelA, title = "Political Ideology & Warmth Towards President Obama", align = TRUE)


##Now consider the relationship between political ideology and feelings of warmth towards
##Obama when controlling for the environment-jobs tradeoff variable. Estimate and interpret 
##the appropriate multivariate linear regression model. Then compare your results
##to those in (1a) and (1b). What do you think might explain these findings? In your
##discussion, consider the possible causal relationships between the variables that would
##be consistent with your findings.


#Code from Class
#run multivariate linear regression model and save results as object model2
model2 <- lm(bdeadbest ~ eth + durest, data = data.s1)
summary(model2)

#############
#Additional Explorations:  F-test for Joint Significance of All Coefficients 
#############
#verify F-test results (hand-calculate) appearing in model summary
anova(model2)
#calculate ESS (R doesn't do that directly; it calculates the ESS attributed
#to each IV in the model
318354023+1593682374
#then calculate F-statistic
(1912036397/2)/(7649750885/27)
#finally, calculate (one-tailed) p-value of F-statistic
1-pf(3.37, 2, 27)