install.packages("alr3")
require(alr3)

# Problem 1.
# A:
attach(salary)
Rank = as.factor(Rank)
fit0 = lm(Salary ~ Rank - 1) # Regress Salary on Rank without intercept
summary(fit0)
# The coefficients beta1, beta2 and beta3 represent the average salary 
# of people in Rank1, Rank2 and Rank3 respectively.

# B:
Sex = as.factor(Sex)
fit1 = lm(Salary ~ Rank + Sex - 1) # Add variable Sex to the model
summary(fit1)
# The coefficient of Sex is -869.5, which means that the average salary 
# of women is 869.5 less than that of men for each rank group(or when the rank is fixed).

# C:
Degree = as.factor(Degree)
fit2 = lm(Salary ~ Rank + Sex + Degree - 1) # Add variable Degree to the model
summary(fit2)
# The coefficient of Degree is -1038.0, which means that the average salary 
# of people with Degree=1 is 1038.0 less than that with Degree = 0, for each group of same rank-sex combination (or when rank and sex are fixed).

# D:
anova(fit2)
# First row compares the model with only intercept and the model with Rank added,
# showing that the variable Rank is significant.
# Second row compares the model with Rank and the model with Sex added,
# showing that the variable Sex is not significant.
# Third row compares the model with Rank and Sex and the model with Degree added,
# showing that the variable Degree is not significant.

anova(fit1, fit0)
# Compare the model regressed on Rank and sex with the model on Rank only.
# Result of ANOVA F test indicates the new variable Sex is not significant.
anova(fit2, fit1)
# Compare the model regressed on Rank, sex and Degree with the model on Rank and Sex only.
# Result of ANOVA F test indicates the new variable Degree is not significant.


# Problem 2.
# A:
ind0 = (Sex == 0)
salary0 = as.data.frame(salary[ind0,])
plot(salary0$YSdeg, salary0$Salary, xlab="YSdeg", ylab="Salary",pch=15, col="blue", cex=1)
mod0 <- lm(Salary ~ YSdeg, salary0)
abline(mod0, col="blue")

ind1 = (Sex == 1)
salary1 = as.data.frame(salary[ind1,])
points(salary1$YSdeg, salary1$Salary, xlab="YSdeg", ylab="Salary",pch=15, col="red", cex=1)
mod1 <- lm(Salary ~ YSdeg, salary1)
abline(mod1, col="red")

# B:
mod = lm(Salary ~ Sex*YSdeg)
summary(mod)
anova(mod)
# The ANOVA test shows that there is no significant interaction between Sex and YSdeg.
# This suggests that the slope of the regression model is similar for both males and females.
# So there is no evidence of discrimination based on gender.


# Problem 3.
Rank = as.factor(Rank)
Sex = as.factor(Sex)
Degree = as.factor(Degree)
fit = lm(Salary ~ Rank + Sex + Degree + Year + YSdeg)
summary(fit)

# checking model accuracy and heteroscedasticity via residual plots
plot(fit, which=1, pch=16) # shows a hint of curvature
residualPlots(fit)

# checking normality via q-q plot 
plot(fit, which=2)

# checking for outliers in predictor via hatvalues
plot(hatvalues(fit), type='h', col="blue", ylab="Hat Values", main="Hat values")
p = 6; n = length(Salary)
abline(h = 2*p/n, lty=2) # threshold for suspects

# checking outliers in response via externally studentized residuals
plot(abs(rstudent(fit)), type='h', col="blue", ylab="Externally Studentized Residuals (in absolute value)", main="Externally Studentized Residuals (in absolute value)")
abline(h = qt(.95, n-p-2), lty=2) # threshold for suspects

# Checking influential points
# Cook's distances
plot(fit, which=4, col="blue")
abline(h = qf(0.05, p+1, n-p-1), lty=2) # threshold for suspects 

# DFBETAS
par(mfrow=c(2,2))
for (j in 1:7){
  plot(abs(dfbetas(fit)[,j]), col=4, type='h', ylab='DFBETAS')
  abline(h = 2/sqrt(n), lty=2) # threshold for suspects
}

# DFFITS  
par(mfrow=c(1,1))
plot(abs(dffits(fit)), typ='h', col=4, ylab='DFFITS')
abline(h = 2*sqrt(p/n), lty=2) # threshold for suspects

# checking for multicolinearity via pairwise correlations b/w predictors
cor(salary[, -6])
require(ellipse)
plotcorr(cor(salary[, -6]))
# There is no evidence for multicolinearity, but we still need other methods to check further.

# checking for multicolinearity via variance inflation factors (VIF)
require(car)
vif(fit)
# VIF are all less than 10, which means that they are not linealy dependent.

# checking for multicolinearity via condition indices
C = cor(salary[, -6]) # correlation matrix for the predictors
L = eigen(C) # eigenvalues  
K = max(L$val)/L$val # condition indices
plot(K, type='h', col=4)
abline(h = 1000, lty=2) # threshold for suspect condition indices 
# Condition indices are all less than 1000, which means that they are not linealy dependent.


# checking interations with 2 predictors
Rank = as.factor(Rank)
Sex = as.factor(Sex)
Degree = as.factor(Degree)
fit = lm(Salary ~ Rank*YSdeg)
summary(fit)
anova(fit)
# P value is 0.03562 which is less than 0.05, suggesting that there is an interation between Rank and YSdeg.

fit = lm(Salary ~ Degree*YSdeg)
summary(fit)
anova(fit)
# P value is 0.0004212, suggesting that there is a significant interation between Degree and YSdeg.

fit = lm(Salary ~ Degree*Year)
anova(fit)
# P value is 0.04532, suggesting that there is an interaction between Degree and Year.

# checking interations with 3 predictors
fit = lm(Salary ~ Degree*Year*Sex)
anova(fit)
# There is no hint to show interaction within 3 predictors

# checking interations with 4 predictors
fit = lm(Salary ~ Year*Sex*YSdeg*Rank)
anova(fit)
# P value is  0.0003827, which means the new adding variable is significant.
# So it suggests that there is a significant interaction within Year, Sex, YSdeg and Rank.
