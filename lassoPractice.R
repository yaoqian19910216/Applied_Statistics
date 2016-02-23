####This file shows how to use lars package to get the selection path, the optimal fraction value (corresponding to the optimal lambda), and the best coefficients

load('04cars.rda')
tmp = dat[,c(13,15,16,18,19)]
tmp = tmp[complete.cases(tmp),]
tmp = as.data.frame(tmp)
names(tmp) = c("hp","mpg","wt","len","wd")
tmp = scale(tmp)
attach(as.data.frame(tmp))


install.packages('lars')
require(lars)

X = cbind(hp,wt,len,wd)
y = mpg
par(mfrow = c(1,2))
#get selection path and fit the model with lasso
fitlar = lars(X,y,normalize = FALSE, intercept = FALSE)
plot(fitlar,col = 1:4, lty = 1:4, lwd = 2)

#compute K-fold cross-validated mean squared prediction error
#if type = 'lasso', default mode is 'fraction' - |beta|/max|beta|
fitlarcv = cv.lars(X,y,type = 'lasso',mode = 'fraction',plot.it = TRUE)

#get the optimal friction value (corresponding to optimal lambda) and the best mode with corresponding coefficients
#$index is the x-axis value: fraction
#$cv is the prediction error
bestfraction = fitlarcv$index[which.min(fitlarcv$cv)]
bestcoef1 = predict(fitlar,s = bestfraction, mode = 'fraction',type = 'coefficients')
bestcoef2 = coef(fitlar,s = bestfraction,mode = 'fraction')
