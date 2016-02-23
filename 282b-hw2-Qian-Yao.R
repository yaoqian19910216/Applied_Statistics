# Problem 1:
set.seed(123)
n=0
for (i in c(1:500)){
  x = runif(100,-1,1)
  y = 1-2*x+rnorm(100,0,0.2)
  fit = lm(y~x)
  confint(fit)
  n = n + as.numeric((-2>confint(fit)[2,1]) && (-2<confint(fit)[2,2]))
}
n/500 
# n/500=0.954
## Close to the theoretical coverage. Centered with small variance. 


# If the sample size is 10
set.seed(123)
n=0
for (i in c(1:500)){
  x = runif(10,-1,1)
  y = 1-2*x+rnorm(10,0,0.2)
  fit = lm(y~x)
  confint(fit)
  n = n + as.numeric((-2>confint(fit)[2,1]) && (-2<confint(fit)[2,2]))
}
n/500 
#n/500 = 0.958

## Nothing changed dramatically. 


# Problem 2:
install.packages("plot3D")
set.seed(123)
intercept = rep(0,500)
slope = rep(0,500)
for (i in c(1:500)){
  x = runif(100,-1,1)
  y = 1-2*x+rnorm(100,0,0.2)
  fit = lm(y~x)
  intercept[i] = coef(fit)[1]
  slope[i] = coef(fit)[2]
}
par(mfrow = c(1,2))
mu1 = mean(intercept)
s1 = sd(intercept)
i = hist(intercept, 50, FALSE)
x = intercept
curve(dnorm(x, mu1, s1), 0.6, 1.6, add=TRUE, col=2) 
mu2 = mean(slope)
s2 = sd(slope)
s = hist(slope, 50, FALSE)
x = slope
curve(dnorm(x, mu2, s2), -2.1, -1.9, add=TRUE, col=2)
# Compare the histogram and the true density of normal distribution,
# we can notice that intercept and slope are marginally normal distributed.


require(plot3D)
par(mfrow = c(1,1))
f = function(x,y){x*y}
z = outer(i$density, s$density, f)
hist3D(i$mids,s$mids,z, xlab="Intercept", ylab="slope", main="Histogram")
# 3-d histogram of intercept and slope, indicating they are jointly normal distributed. 

# 2-d density plot of intercept and slope.
# using the "gplots" package
install.packages('gplots')
require(gplots)
hist2d(intercept,slope,nbins=20) 

# Problem 3:
install.packages("smoothmest")
require(smoothmest)
set.seed(123)
# If sample size is 100
n=0
for (i in c(1:500)){
  x = runif(100,-1,1)
  y = 1-2*x+rdoublex(100,mu=0,lambda=1)
  fit = lm(y~x)
  summary(fit)
  confint(fit)
  n = n + as.numeric((-2>confint(fit)[2,1]) && (-2<confint(fit)[2,2]))
}
n/500 
#n/500 = 0.936
# The results from Laplace error setting are less robust than that from
# normal error setting. Since Laplace distribution has heavier tails than
# normal distribution and the least square model is sensitive to the outliers,
# the results varies greater than previous case.

# If the sample size is 10
n=0
set.seed(123)
for (i in c(1:500)){
  x = runif(10,-1,1)
  y = 1-2*x+rdoublex(10,mu=0,lambda=1)
  fit = lm(y~x)
  summary(fit)
  confint(fit)
  n = n + as.numeric((-2>confint(fit)[2,1]) && (-2<confint(fit)[2,2]))
}
n/500 
#n/500 = 0.96
# The conclusion is same as above. The percentage obtained by small sample 
# varies greater than that of large sample, and also is less robust than normal
# error setting.

set.seed(123)
intercept = rep(0,500)
slope = rep(0,500)
for (i in c(1:500)){
  x = runif(100,-1,1)
  y = 1-2*x+rdoublex(100,mu=0,lambda=1)
  fit = lm(y~x)
  intercept[i] = coef(fit)[1]
  slope[i] = coef(fit)[2]
}
par(mfrow = c(1,2))

mu1 = mean(intercept)
s1 = sd(intercept)
i = hist(intercept, 50, FALSE)
x = intercept
curve(dnorm(x, mu1, s1), 0.6, 1.6, add=TRUE, col=2)
mu2 = mean(slope)
s2 = sd(slope)
s = hist(slope, 50, FALSE)
x = slope
curve(dnorm(x, mu2, s2), -3, -1, add=TRUE, col=2)
# Draw the histogram and the true density of normal distribution.
# Intercept and slope are marginally normal distributed.
# But the standard deviations of intercept and slope in Laplace setting
# are much larger than those in normal setting respectively.
# So it indicates Laplace setting is less robust.

require(plot3D)
par(mfrow = c(1,1))
f = function(x,y){x*y}
z = outer(i$density, s$density, f)
hist3D(i$mids,s$mids,z, xlab="Intercept", ylab="slope", main="Histogram")


# Problem 4:
set.seed(123)
dff = rep(0,496)
for(n in c(5:500)){
  x = runif(n,-1,1)
  y = 1-2*x+rnorm(n,0,0.2)
  fit = lm(y~x)
  dff[n-4] = max(abs(dffits(fit)))
}
n = c(5:500)
plot(n, dff)

# We want to derive the function of maximum absolute value of dff as a*n^p
# first take the log transformation, then fit a linear model.
x = log(n)
df = log(dff)
fit = lm(df~x)
a = exp(fit$coef[1])
p = fit$coef[2]
curve(a*x^p, 0,500, lwd=2)
curve(2*sqrt(1/x), 0, 500, add=TRUE, col=2,lwd=2)
# The two curves are not same, since their test objectives are different.
# For each given sample size n, 2*sqrt(1/n) is to detect whether each data point is an outlier,
# i.e. P(dffits(i) = ti*sqrt(hi/1-hi) > 2*sqrt(1/n)) =0.05.
# However, in this case we use the max(abs(dffits(i)),i=1,...,n) to derive a function w.r.t n, which
# will be obviously greater than 2*sqrt(1/n).

# Problem 5:
set.seed(123)
x1 = rnorm(100, 0, 1)
x2 = rnorm(100, 0, 1)
x3 = rnorm(100, 0, 1)
x4 = rnorm(100, 0, 1)
x5 = x1+x2+x3+x4+rnorm(100, 0, 0.01)
y = x1+x2+x3+x4+rnorm(100, 0, 0.1)
mod = lm(y~x1+x2+x3+x4+x5)
dat = cbind(x1,x2,x3,x4,x5)
cor(dat)[5,]

# The correlation of (x5,xi)=(0.3764618 0.5393662 0.4415942 0.5410269) i=1,2,3,4
# So the correlation are small, and not very indicative for the linear dependence between variables.

# checking for multicolinearity via variance inflation factors (VIF)
require(car)
vif(mod)
# For each i, VIF is greater than 10, which means that they are suspicious to be linealy dependent.  

# checking for multicolinearity via condition indices
C = cor(dat) # correlation matrix for the predictors
L = eigen(C) # eigenvalues  
K = max(L$val)/L$val # condition indices
plot(K, type='h', col=4)
abline(h = 1000, lty=2) 

# For i=5, k5 is greater than 1000. So it is considered suspect.
# In this case, correlations do not help detecting the multicolinearity
# while VIFs and codition indices help.