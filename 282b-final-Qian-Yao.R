
##################################  Problem 1 ########################################
rm(list = setdiff(ls(), lsf.str()))
#Here I didn't move the outlier because we have shown in last HW that the influence of this outlier is trivial
#take log transform, tranformed scatter plot
tmp = read.table('dolphin.txt',header = TRUE)
tmp = as.data.frame(tmp)
tmp = tmp[complete.cases(tmp),]
names(tmp) = c('R','SP')
attach(tmp)
plot(log(R),log(SP))

#fit a line
fit.ls = lm(log(SP) ~ log(R))
summary(fit.ls)
abline(fit.ls)
par(mfrow = c(2,2))

#diagnostics: model accuracy,homoscedasticity, normality, outlier detection(cook distance and hat value) 
plot(fit.ls,which = c(1,2,4))
#The model accuracy seems fine but the normality assuption is not quick good. The cook distance also shows that there are few outliers.

# DFFITS (outlier detecton, optional). It's tedious.	 Other methods available for the same purpose, e.g.DEBTAS
par(mfrow=c(1,1))
plot(abs(dffits(fit.ls)), typ='h', col=4, ylab='DFFITS')
abline(h = 2*sqrt((ncol(tmp)-1)/nrow(tmp)), lty=2) # threshold for suspects
#There are a few of outliers.

# Checking for multicolinearity via variance inflation factors (VIF). Other methods available for the same purpose.
#can't perform VIF diagnostic
#VIF > 10 is considered suspect
#require(car)
#vif(fit.ls)

#get the outlier. There is only one outlier
I = influence.measures(fit.ls)
I = I$is.inf
print(I)
index = which.max(SP)
outlier = 4;

#Get scatter polot before transformation and 
plot(R,SP)
coeficience = coef(fit.ls)
predictval = exp(coeficience[1])*sort(R)^coeficience[2]
lines(sort(R),predictval,col = 'red')
CI = predict(fit.ls, interval = 'confidence')
up = sort(exp(CI[,'upr']))
low = sort(exp(CI[,'lwr'])
lines(sort(R),low,col = 'blue')
lines(sort(R),up,col = 'blue')
################################## Problem 1 finished ########################################



################################## Problem 2  ########################################

rm(list = setdiff(ls(), lsf.str()))
#get the data
tmp = read.table('dolphin.txt',header = TRUE)
tmp = as.data.frame(tmp)
tmp = tmp[complete.cases(tmp),]
names(tmp) = c('R','SP')
attach(tmp)
polynomial.select <- function(R,SP,alpha = 0.05, plot = TRUE, conf = 0.95)
{
	# fit polynomial models of dgree 1 through 10
	alpha = 0.05
	dmax = 10;
	conf = 0.95
	for (i in c(1:(dmax - 1)))
	{
		fit1 = lm(SP ~ poly(R,i,raw = TRUE))
		fit2 = lm(SP ~ poly(R,i + 1,raw = TRUE))
		anovatest = anova(fit1,fit2);
		pvalue = anovatest$Pr[2]
		if (pvalue > alpha)
			break;	
	}
	
	fit = lm(SP ~ poly(R,i,raw = TRUE))
	CI = predict(fit,interval = 'confidence',level = conf);
	upper = CI[,'upr']
	lower = CI[,'lwr']
	if (plot)
	{
		plot(R,SP)
		points(R,predict(fit),col = 'red',pch = 16)
		points(R,upper,col = 'blue',pch = 16)
		points(R,lower,col = 'blue',pch = 16)

	}	
}
polynomial.select(R,SP,0.05,TRUE,0.95)
################################## Problem 2 finished ########################################



################################## Problem 3 ########################################
rm(list = setdiff(ls(), lsf.str()))
# install packages 'boot'
install.packages('boot')
require(boot)

#get data frame
tmp = read.table('dolphin.txt',header = TRUE)
tmp = as.data.frame(tmp)
tmp = tmp[complete.cases(tmp),]
names(tmp) = c('R','SP')

#transform the data set
tmplog = data.frame(logR = log(R),logSP = log(SP))
attach(tmplog)

#statstics functions
boot.lss <- function(data,indices)
{
	#select observations in bootstrap sample
	data <- data[indices,];
	fit.ls = lm(logSP ~ logR,data = data);
	coef(fit.ls)
}

#get bootstrap least-squares slope
dolphinSlope.boot = boot(data = tmplog,statistic = boot.lss,R = 999)
jack.after.boot(boot.out = dolphinSlope.boot,index = 2,main = 'jacknife-after-bootstrap-least-squares-slope')
################################## Problem 3 finished ########################################



################################## Problem 4 ########################################
rm(list = setdiff(ls(), lsf.str()))
# install packages 'boot'
install.packages('boot')
require(boot)

#get data frame
tmp = read.table('dolphin.txt',header = TRUE)
tmp = as.data.frame(tmp)
tmp = tmp[complete.cases(tmp),]
names(tmp) = c('R','SP')
attach(tmp)

#transform the data set
tmplog = data.frame(logR = log(R),logSP = log(SP))
attach(tmplog)

#statstics functions
boot.lss <- function(data,indices)
{
	#select observations in bootstrap sample
	data <- data[indices,];
	fit.ls = lm(logSP ~ logR,data = data);
	abs(dffits(fit.ls))
}
#get bootstrap least-squares slope
dolphinSlope.boot = boot(data = tmplog,statistic = boot.lss,R = 999)
boostDFFITS = abs(colMeans(dolphinSlope.boot$t))
plot(boostDFFITS,typ = 'h',col = 'blue')
abline(h = 2*sqrt((ncol(tmplog)-1)/nrow(tmplog)),col = 'black')
#no outliers
#There is no outlier when R = 999
################################## Problem 4 finished ########################################



################################## Problem 5 ########################################
 rm(list = setdiff(ls(), lsf.str()))
 
####### A. Clean Data ######

#read the whole data set
dat = read.table(url("http://www.umass.edu/statdata/statdata/data/aps.dat"));
#dat = read.table("aps.xls",header = TRUE);
dat = as.data.frame(dat);
names(dat) = c('ID','PLACE','PLACE3','AGE','RACE','GENDER','NEURO','EMOT','DANGER','ELOPE','LOS','BEHAV','CUSTD','VIOL')
attach(dat)

#Add binary response as the firt column
n = nrow(dat);
y = rep(1,n)
y[PLACE3 == 0] = 0;
data = cbind(dat,Y = y);
attach(data)

#remove three unnecessary variables,'ID','PLACE','PLACE3'
tmp = data[,c(-1,-2,-3)];
tmp = tmp[complete.cases(tmp),]

#For simplicity, assume only 'AGE', 'LOS','BEHAV' are numeric. (Actually, 'BEHAV' is discrete ordinary with 9 level 9......too much......).
Y = as.factor(Y);tmp[,12] = as.factor(tmp$Y);
RACE = as.factor(RACE);tmp[,2] = as.factor(tmp$RACE);
GENDER = as.factor(GENDER);tmp[,3] = as.factor(tmp$GENDER);
NEURO = as.factor(NEURO);tmp[,4] = as.factor(tmp$NEURO);
EMOT = as.factor(EMOT);tmp[,5] = as.factor(tmp$EMOT);
DANGER = as.factor(DANGER);tmp[,6] = as.factor(tmp$DANGER);
ELOPE = as.factor(ELOPE);tmp[,7] = as.factor(tmp$ELOPE);
CUSTD = as.factor(CUSTD);tmp[,10] = as.factor(tmp$CUSTD);
VIOL = as.factor(VIOL);tmp[,11] = as.factor(tmp$VIOL);

###### B backward stepwise AIC model selection ######
fullmodel = glm(Y ~ AGE + RACE + GENDER + NEURO + EMOT + DANGER + ELOPE + LOS + BEHAV + CUSTD + VIOL,family = binomial)
backwards = step(fullmodel,direction = 'backward');
formula(backwards)
#The backward selection gives the following model:
#Y ~ AGE + RACE + NEURO + LOS + BEHAV + CUSTD

###### C. Apply best subset selection with AIC ######

install.packages('bestglm')
require(bestglm)
#Be patient, it takes one minute
bestglms = bestglm(tmp,family = binomial, IC = 'AIC')
bestglms$BestModel
formula(bestglms$BestModel)
# Based on the results, the best model is : y ~ AGE + RACE + NEURO + LOS + BEHAV + CUSTD, which is consistent with backward stepwise AIC model selection.

#Call:  glm(formula = y ~ ., family = family, data = Xi, weights = weights)

#Coefficients:
#(Intercept)          AGE        RACE1       NEURO1       NEURO2       NEURO3          LOS        BEHAV  
#    -6.9590       0.1717       0.6218       0.3889       0.6874       1.1199       0.0729       0.2606  
#     CUSTD1  
#     3.5924  

#Degrees of Freedom: 507 Total (i.e. Null);  499 Residual
#Null Deviance:	    704 
#Residual Deviance: 364.8 	AIC: 382.8

#y ~ AGE + RACE + NEURO + LOS + BEHAV + CUSTD

###### D. Fit the model found in Part B. ######

#summary
bestmodel = glm(Y ~ AGE + RACE + NEURO +  LOS + BEHAV + CUSTD,family = binomial)
summary(bestmodel)

#diagonosis
par(mfrow = c(2,2))

#diagnostics: model accuracy,homoscedasticity, normality, outlier detection(cook distance and hat value) 
plot(bestmodel,which = c(1,2,4))

# DFFITS (outlier detecton, optional). It's tedious.	 Other methods available for the same purpose, e.g.DEBTAS

par(mfrow=c(1,1))
plot(abs(dffits(bestmodel)), typ='h', col=4, ylab='DFFITS')
abline(h = 2*sqrt((ncol(tmp)-1)/nrow(tmp)), lty=2) # threshold for suspects
#There are a few outliers

# Checking for multicolinearity via variance inflation factors (VIF). Other methods available for the same purpose.
#VIF > 10 is considered suspect
require(car)
vif(bestmodel)
#no significant outliers

###### E. Apply L1-penalized logistic regression ######
#install package
install.packages('glmnet')
require(glmnet)

#fit the model via lasso penalized logistic regression.
#glmnetmode = glmnet(as.matrix(tmp[,c(1:11)]),Y, family = 'binomial',alpha = 1,lambda = seq(0.02,0.5,0.01))
#actual sequence of lambda used 
#lambdas = glmnetmode$lambda

#tuning parameter lambda via corss-validation
cvglmnet = cv.glmnet(data.matrix(tmp[,c(1:11)]),Y,type.measure = 'class',family = 'binomial',alpha = 1)
#the optima lambda is 
optlambda = cvglmnet$lambda.min
# optlambda
# [1] 0.003081583
# the optimal lambda via cross validation is:  0.003081583

#model selection using the optimal alpha
coef(cvglmnet)
#(Intercept) -3.88646679

#(Intercept) -6.87824417
#AGE          0.06379915
#RACE         0.18419051
#GENDER       .         
#NEURO        0.17378748
#EMOT         .         
#DANGER       .         
#ELOPE        .         
#LOS          0.03318939
#BEHAV        0.15885638
#CUSTD        2.83719306
#VIOL       

#it chooses: y ~ AGE + RACE + NEURO + LOS + BEHAV + CUSTD being consistent with previous results.
################################## Problem 5 finished ########################################