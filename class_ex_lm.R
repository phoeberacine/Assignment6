############################################################
#
#Descriptive Statistics
#
#POLS 205
#
#############################################################

#############
#Workspaces
#############

#Set desired working directory
setwd("G:\\My Drive\\Work\\Teaching\\18-19\\205\\lectures\\")

#############
#Data Setup
#############

data.s1 <- read.table("t5_civilwarregress_lacinasamp.txt", sep="", header=T, na.string=".")

#look at first ten cases of sample data
data.s1[1:10,]
#see dimensions of data frame, with rows being number of cases (first number listed) 
#and columns being the number of variables (second number listed)
dim(data.s1)

#############
#Bivariate Regression Model
#############

#run regression and print key results to screen
summary(lm(bdeadbest ~ eth, data = data.s1, na.action=na.exclude))

#run regression and save results as object model1
model1 <- lm(bdeadbest ~ eth, data = data.s1, na.action=na.exclude)
#then summarize key info and print to screen using summary command
summary(model1)

#look at fitted values (yhat) and residuals (e), save fitted values and residuals as new "variables" in data set
data.s1$yhat <- fitted(model1)
data.s1$e <- resid(model1)
data.s1[1:10,]

#create scatterplot of IV and DV and overlay regression line
plot(data.s1$eth, data.s1$bdeadbest, ylab="Battle Deaths", xlab="Ethnic Fractionalization", main="Scatterplot
     of Civil War Battle Deaths vs. Ethnic Fractionalization")
abline(a = 7426, b = 12366)
#alternatively, extracting the regression line directly from the model output...
abline(a=coef(model1)[1], b=coef(model1)[2])

#calculate 95% confidence interval for slope coefficient
#first find appropriate t-value with 28 (30-2) df
qt(.975, 28)
#then calculate lower bound of CI by hand
12366 - 2.05*12592
#equivalently, extracting the estimated coefficients directly from the model object etc.:
coef(model1)[2] - qt(.975, 28)*sqrt(diag(vcov(model1)))[2]
#finally calculate upper bound of CI by hand
12366 + 2.05*12592
#or equivalently,...
coef(model1)[2] + qt(.975, 28)*sqrt(diag(vcov(model1)))[2]

#anova and R2
anova(model1)
#calculate R2
318354023/(318354023 + 9243433259)


