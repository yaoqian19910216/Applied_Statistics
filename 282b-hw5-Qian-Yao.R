############################ Data Clean #################################
rm(list = setdiff(ls(), lsf.str()))
#read the data
tmp = read.csv("AmesHousing.csv",header = TRUE)
attach(tmp)

#Clean the dat
ncols = ncol(tmp);
#Add 'NOTEXIST' into the levels of each FACTOR variable
#and replace 'NA' with 'NOTEXIST'. Leave along with numerical variable.

for (i in c(1:ncols))
{
	if(is.factor(tmp[,i]))
	{
		a = tmp[,i];
		#add 'NOTEXIST' into the levels
		tmp[,i] = factor(a,levels = (c(levels(a),'NOTEXIST')));	
		
		#replace NA with NOTEXIST
		NAs = is.na(tmp[,i]);
		tmp[NAs,i] = 'NOTEXIST';
	}
	else
	{
		tmp[,i] = as.numeric(tmp[,i]);
	}
	
}

#Get rid of real missing data
#656 out of 2930 data points are missing. It's ok.
nrow(tmp)
tmp = tmp[complete.cases(tmp),]
nrow(tmp)

#get rid of the ordr and PID column
tmp = tmp[-c(1,2)]

#remove the hourses with more than 4000 square feet
good = (tmp$Gr.Liv.Area <= 4000);
tmp = tmp[good,]
nrows = nrow(tmp)
ncols = ncol(tmp)


############################ Problem 1 #################################
### A
#fit the full model
fit  = lm(log(SalePrice) ~., data = tmp)
summary(fit)

#get rid of perfect linearly related data
tmp$Utilities <- NULL
tmp$Bsmt.Cond <- NULL
tmp$Bsmt.Exposure <- NULL
tmp$BsmtFin.Type.1 <- NULL
tmp$BsmtFin.Type.2 <- NULL
tmp$Total.Bsmt.SF <- NULL
tmp$Gr.Liv.Area <- NULL

fit  = lm(log(SalePrice) ~., data = tmp)
summary(fit)
### B 
# It says "Do not spend too much time on this or on trying to fix any problem you might find. This is just for good measure here. 
#So, I just list some important diagnostics. There are a lot of R commands for implementing similar diagnostics in "diagnostics.R"

#NOTE!!!!!!!!!!
# The following three commands do three diagnostics: model accuracy, normality, outlier detection(cook distance and hat value) 
# !!!!!!!!!!!!
par(mfrow = c(2,2))
plot(fit, which = c(1,2,4))
plot(hatvalues(fit), type ='h', main="Hat values")

# DFFITS (outlier detecton, optional). It's tedious.	
par(mfrow=c(1,1))
plot(abs(dffits(fit)), typ='h', col=4, ylab='DFFITS')
abline(h = 2*sqrt(ncols/nrows), lty=2) # threshold for suspects

# Checking for multicolinearity via variance inflation factors (VIF). Other methods available for the same purpose.
#VIF > 10 is considered suspect
require(car)
vif(fit)

### C
# stepwise methods using F-tests
# doesn't work
install.packages('wle')
require(wle)
mle.stepwise(fit)
mle.stepwise(fit, type="Backward")
mle.stepwise(fit, type="Stepwise")

## stepwise search with AIC 
#set k = 2;
null = lm(log(SalePrice) ~ 1, data = tmp)
full  = lm(log(SalePrice) ~ ., data = tmp)
step(null,scope = list(lower = null,upper = full),direction = 'forward',k =2)
step(null,scope = list(lower = null,upper = full),direction = 'backward',k =2)
step(null,scope = list(lower = null,upper = full),direction = 'both',k =2)

## stepwise search with BIC 
# set k = log(n)
null = lm(log(SalePrice) ~ 1, data = tmp)
full  = lm(log(SalePrice) ~ ., data = tmp)
step(null,scope = list(lower = null,upper = full),direction = 'forward',k = log(nrows))
step(null,scope = list(lower = null,upper = full),direction = 'backward',k = log(nrows))
step(null,scope = list(lower = null,upper = full),direction = 'both',k = log(nrows))

# best subset selection using Mallow's Cp
install.packages('leaps')
require(leaps)
X = model.matrix(fit)[,-1]
#Doesn't allow variable more than 31
#L = leaps(x = X, y = log(tmp$SalePrice),method = 'Cp')
#It takes super long time.
L = regsubsets(x = X, y = log(tmp$SalePrice),really.big = TRUE)
print(L)
ind = which.min(L$cp)
L$which[ind,]  # best model
