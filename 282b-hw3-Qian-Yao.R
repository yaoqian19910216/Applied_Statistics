#Before running this solution, put "04cars.rda" in the current working directory of R.

# Problem 1
n = c(30,50,100,200,500,1000)
par(mfrow=c(2,3))
for(s in 1:6){
    K = c()
    for (p in 1:20){
      x = c(1:n[s])/(n[s]+1)
      x0 = rep(1,n[s])
      x1 = poly(x,p,raw = TRUE)
      X = cbind(x0,x1)
      E = eigen(t(X)%*%X)
      i = max(E$val)/min(E$val)
      K = c(K,i)
    }
    K
    plot(K,main=paste("n=",n[s]),col = 'red')
}
 #clear the par setting
# The condition numbers of design matrice are very large, indicating
# this poly matrix is ill-conditioned. So fitting raw polynomials may
# be unstable.

# Problem 2
# A.
require(car)
load("04cars.rda")
mpg=dat$City_MPG
hp=dat$Horsepower
fit1=lm(mpg~1)
anova.p=0
p=-1
while(anova.p<.05){
    p=p+1
    fit0=fit1
    fit1=lm(mpg~poly(hp,degree=p+1))
    anova.p=anova(fit0,fit1)$P[2]
}
print(p) # The degree set by ANOVA testing is 5.

#B
dev.off()
fit=lm(mpg~poly(hp,degree=p))
newx=seq(min(hp),max(hp),1)
newy=predict(fit,data.frame(hp=newx),interval="confidence")

plot(hp,mpg)
points(newx,newy[,1],type="l",col="red",main="Polynomial Model Fit",
       xlab="Horsepower",ylab="City MPG")

points(newx,newy[,2],type="l",col="blue",lty=2)
points(newx,newy[,3],type="l",col="blue",lty=2)


#Problem 3
#A
require(splines)
Spline <- function(x, y, K, deg=1, plot=TRUE)
{
  knot = quantile(x, seq(1/(K+2), (K+1)/(K+2), len = K+2), type=1)
  fit = lm(y ~ bs(x, degree=deg, knots=knot))
  if (plot==TRUE) #plot a piecewise constant fit
  {
    # K internal knots, thus K+1 intervals, 
    # then quantile is of length K+2
    # and pts and val are of length 2*(K+1)
    plot(x, y, pch = 16, main="Piecewise Constant Fit & Splines Fit")
    pc_k = quantile(x, seq(0, 1, len = K+2), type=1)
    pc_pts = rep(0,2*(K+1))
    pc_val = rep(0,2*(K+1))
    for (j in 1:(K+1)){
      I = (pc_k[j] < x)&(x <= pc_k[j+1])
      pc_fit = lm(y[I] ~ 1)
      pc_pts[2*j-1] = pc_k[j]
      pc_pts[2*j] = pc_k[j+1]
      pc_val[2*j-1] = coef(pc_fit)
      pc_val[2*j] = coef(pc_fit)
    }
    lines(pc_pts,pc_val,col="yellow",lwd=2)
  }
  return(fit)
}

pts = seq(min(hp),max(hp),len=100)
val1 = val2 = val3 = rep(NA,100)

fit1 = Spline(x=hp, y=mpg, K=5, deg=1)
val1 = predict(fit1, data.frame(x=pts))
lines(pts, val1, col="blue", lwd=2)

fit2 = Spline(x=hp, y=mpg, K=4, deg=2, plot=FALSE)
val2 = predict(fit2, data.frame(x=pts))
lines(pts, val2, col="green", lwd=2)

fit3 = Spline(x=hp, y=mpg, K=3, deg=3, plot=FALSE)
val3 = predict(fit3, data.frame(x=pts))
lines(pts, val3, col="red", lwd=2)

#Problem 4
# A:
PiecewiseConstant <- function(x, y, L, plot=TRUE, color="blue"){
  f = rep(0,2^L)
  pts = rep(0,2^L*2)
  val = rep(0,2^L*2)
  K = quantile(x, seq(0, 1, len = 2^L+1), type=1)
  for (j in 1:2^L){
    I = (K[j] < x)&(x <= K[j+1])
    fit = lm(y[I] ~ 1) 
    pts[2*j-1] = K[j]
    pts[2*j] = K[j+1]
    val[2*j-1] = coef(fit)
    val[2*j] = coef(fit)
    f[j] = coef(fit)
  }
  if (plot == TRUE){lines(pts, val, col=color, lwd = 2)}
  return(f)
}

# B:
plot(hp, mpg, pch = 16)
PiecewiseConstant(hp, mpg, 2, TRUE, color="blue") # L=2
PiecewiseConstant(hp, mpg, 3, TRUE, color="green") # L=3
PiecewiseConstant(hp, mpg, 4, TRUE, color="red") # L=4


# C:
BICs = c()
for (L in 1:6){
  B=0
  K = quantile(hp, seq(0, 1, len = 2^L+1), type=1)
  SquaredError = 0
  for (j in 1:2^L){
    I = (K[j] < hp)&(hp <= K[j+1])
    if(length(mpg[I]) > 0){
      fit = lm(mpg[I] ~ 1)
      sm = summary(fit)
      SquaredError = SquaredError + sum(sm$residuals^2) # compute sum of residual squared
    }
    n = length(hp)
    MSE = SquaredError/n
    BIC_of_L = n*log(MSE) + 2^L*log(n) # compute BIC of the 2^L piecewise constant model
  }
  BICs=c(BICs,BIC_of_L)
}
plot(1:6, BICs, xlab = "L", ylab = "BIC", lwd = 2, col = "blue", pch = 5)
lines(1:6, BICs, type='l', lwd = 2, col = "blue")
# The BIC of model when L=3 is the smallest from the graph,
# so we should choose L=3

