################## Problem 1 ########################
load('04cars.rda')
tmp = dat[,c(13,15,16,18,19)]
tmp = tmp[complete.cases(tmp),]
tmp = as.data.frame(tmp)
names(tmp) = c("hp","mpg","wt","len","wd")
tmp = scale(tmp)
tmp = as.data.frame(tmp)
attach(tmp)

#selection path of LASSO
install.packages('lars')
require(lars)
par(mfrow = c(2,1))
X = cbind(hp,wt,len,wd)
y = mpg
fit1 = lars(x = X, y = y, normalize = FALSE,intercept = FALSE)
plot(fit1,lty = 1:4, col = 1:4, lwd = 2)
legend('bottomleft',c('hp','wt','len','wed'),col = 1:4, lty = 1:4, lwd = 2, bg = 'white')

#selection path of forward stepwise selection
fit2 = lars(x = X, y = y, type = 'stepwise',normalize = FALSE)
plot(fit2,lty = 1:4, col = 1:4, lwd = 2)
legend('bottomleft',c('hp','wt','len','wed'),col = 1:4, lty = 1:4, lwd = 2, bg = 'white')
#we can that the two method selects predictor in the same order.
par(mfrow = c(1,1))



################## Problem 2 ########################
#define incremental.stagewise function
incremental.stagewise <- function (X,r,eps)
{
	
	beta = array(0,dim = c(1,ncol(X)))
	corrcoef = cor(X,r)
	threshold = 10^-3;
	
	while(abs(max(corrcoef)) > threshold)
	{
		#print(abs(max(corrcoef)))
		#x: predictor most correlated with r
		index = which.max(abs(cor(X,r)))
		#print(index)
		x = X[,index]
		
		#update beta and residual 
		delta = eps*sign(x%*%r)
		beta[index] = beta[index] + delta
		r = r - delta*x
		corrcoef = cor(X,r)	
	}
	return (beta)
}

#apply the function on the car data set
beta = incremental.stagewise(X,y,0.001)
beta
#> beta
#       [,1]   [,2] [,3] [,4]
#[1,] -0.139 -0.514    0    0
#This algorithm selects 'hp' and 'wt' with coefficients -0.139 and -0.514, respective

fit = lm(mpg ~ ., data = tmp)
summary(fit)
step(fit,direction = 'backward')$call
#the step backward algorithm selects 'hp', 'wt' and 'len' corresponding coefficients -0.244, -0.769, 0.2. There two methods generate different results.

#################### Problem 3 #########################
require(MASS)
require(quantreg)

tmp = read.table('dolphin.txt',header = TRUE)
tmp = as.data.frame(tmp)
names(tmp) = c('R','SP')
attach(tmp)

plot(log(R),log(SP),pch = 16)

#lm fit including all the data
fit.ls = lm(log(SP) ~ log(R))
summary(fit.ls)

#get the outlier. There is only one outlier
I = influence.measures(fit.ls)
I = I$is.inf
print(I)
index = which.max(SP)
outlier = 4;

# we remove the obvious outlier
fit.outlier = lm(log(SP[-4]) ~ log(R[-4]))
summary(fit.outlier)

#plot the fit of all data and the fit without outlier
# the fit is very different
abline(fit.ls, lty = 1, col = 1,lwd=2)
abline(fit.outlier, lty = 1,col = 2,lwd = 2)


# fitting M-estimators
fit.huber = rlm(log(SP) ~ log(R), maxit=50)
summary(fit.huber)

fit.hampel = rlm(log(SP) ~ log(R), maxit=50, psi = psi.hampel)
summary(fit.hampel)

fit.tukey = rlm(log(SP) ~ log(R), maxit=50, psi = psi.bisquare)
summary(fit.tukey)

abline(fit.huber, lty = 1, col = 3, lwd=2)
abline(fit.hampel, lty = 1, col = 4, lwd=2)
abline(fit.tukey, lty = 1, col = 5, lwd=2)

# L1 regression
fit.l1 = rq(log(SP) ~ log(R))
abline(fit.l1, lty = 1, col = 6, lwd=2)

# high breakdown point methods
#least median of squares
fit.lms = lmsreg(log(SP) ~ log(R))
#least trimmed sum of squares
fit.lts = ltsreg(log(SP) ~ log(R))

abline(fit.lms, lty = 1, col = 7,lwd=2)
abline(fit.lts, lty = 1, col = 8)
legend('bottomright', legend = c('LS','LS no outlier','Huber','Hampel','Tukey','L1','Least Median','Trimmed Mean'), col = 1:8, lty = 1, lwd=2, bg='white')

#There is only one outlier out of 1634 data points. So, all the regression results are similar regardless of their robustness


