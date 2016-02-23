# Problem 1:
# a:
load("04cars.rda")
tmp = dat[,c(14,15)]
tmp = tmp[complete.cases(tmp),] 
tmp = as.data.frame(tmp)
dat = tmp
attach(dat) # creates variables for each column
plot(Highway_MPG, City_MPG, pch=15, col="blue", cex=1)

# b:
mod <- lm(City_MPG ~ Highway_MPG)
abline(mod, col="red", lwd=2)
summary(mod)
# both coefficients are significant, pretty good fit with good R^2

# c:
new = data.frame(Highway_MPG= 36)
predict(mod, new, interval='prediction') 
# 95 percent confidence interval for 2015 Ford Focus:
#  fit      lwr      upr
#1 27.91352 24.40322 31.42383
# actual value of 26 is in the 95% prediction interval

# d:
CI <- predict(mod, interval='confidence') 
x <- dat[, 2]
y1 <- CI[, "lwr"]
y2 <- CI[, "upr"]
lines(x, y1)
lines(x, y2)

# e:
mod <- lm(City_MPG ~ Highway_MPG)
co <- confint(mod, level=1-0.05/2)
plot(co[1,], co[2,], xlab="Intercept", ylab="Slope", xlim=c(-5,-1), ylim=c(0.75,0.95))
rect(co[1,1], co[2,1], co[1,2], co[2,2])# 95% confidence region based on t-test confidence intervals for each.

# f:
install.packages("car")
library(car)
par(new = TRUE)
confidenceEllipse(mod,c(1,2),level = 0.95,col = 'red',add = TRUE)

# g:
mod1 <- lm(City_MPG ~ 0 + Highway_MPG, data = dat) #simple linear model without intercept
summary(mod1)
plot(Highway_MPG, City_MPG, pch=15, col="blue", cex=1)
abline(mod, col="red", lwd=2)
abline(mod1, col="green", lwd=2) 


# h:
#Still keep the intercept. 
#Method 1: The summary for the first model(mod) has a small p-value for intercept, so the test for intercept=0 is rejected. 
#Method 2: 
confint(mod)
# 0 is not in this interval, so we should reject Ho.

# Problem 2:
# a:
plot(Highway_MPG, City_MPG, pch=15, col="blue", cex=1)
for ( i in c(1:10)){
  row <- sample(428, 200, replace = FALSE, prob = NULL)
  City_MPG1 <- City_MPG[row]
  Highway_MPG1 <- Highway_MPG[row]
  sub1 = lm(City_MPG1 ~ Highway_MPG1)
  abline(sub1, col="grey", lwd=1) 
}
# Comments: They take the shape similar to the confidance band. 
# The lines have similar slope and intercept with the fitting model of whole data points.

#b:
for ( i in c(1:10)){
  row <- sample(428, 200, replace = TRUE, prob = NULL) #bootstrap with replacement
  City_MPG1 <- City_MPG[row]
  Highway_MPG1 <- Highway_MPG[row]
  sub1 = lm(City_MPG1 ~ Highway_MPG1)
  abline(sub1, col="brown", lwd=1) 
}
#The regression line based on sampling without replacement is a little better than that with replacement.

# Problem 3:

body <- read.table(file="body.txt", header=FALSE, sep="")
names(body) = c("Biacromial diameter", "Biiliac diameter", "Bitrochanteric diameter", "Chest depth", "Chest diameter", "Elbow diameter", "Wrist diameter", "Knee diameter", "Ankle diameter", "Shoulder girth", "Chest girth", "Waist girth", "Navel", "Hip girth", "Thigh girth", "Bicep girth", "Forearm girth", "Knee girth", "Calf maximum girth", "Ankle minimum girth", "Wrist minimum girth", "Age", "Weight", "Height", "Gender")
attach(body)
hwmen <- body[c(1:247), c("Weight", "Height")]
plot(hwmen$Height, hwmen$Weight, xlab="Height", ylab="Weight",pch=15, col="blue", cex=1)
fit1 <- lm(Weight ~ Height, hwmen)
abline(fit1, col="blue")
summary(fit1)

hwwomen <- body[c(-(1:247)), c("Weight", "Height")]
points(hwwomen$Height, hwwomen$Weight, pch=15, col="red", cex=1)
fit2 <- lm(Weight ~ Height, hwwomen)
abline(fit2, col="red")
summary(fit2)
#Method 1: using t test
# Ho:beta1-beta2=0
# beta1-beta2 ~ N(0,sigma1^2*((X1'X1)^-1)+sigma2^2*((X2'X2)^-1))
# sigma1 and sigma2 are unknown
# so beta1-beta2 ~ t(0,s1^2*((X1'X1)^-1)+s2^2*((X2'X2)^-1))

se <- sqrt((summary(fit1)$coefficients[2,2])^2+(summary(fit2)$coefficients[2,2])^2) #sqrt(se1^2+se2^2)=sqrt(sigma1^2*((X1'X1)^-1)+sigma2^2*((X2'X2)^-1))
t <- qt(0.975, 426) 
lw <- fit1$coef[2]-fit2$coef[2]-se*t   # lower bound of confidence interval:beta1-beta2-se*t
up <- fit1$coef[2]-fit2$coef[2]+se*t   # upper bound of confidence interval:beta1-beta2+se*t
cbind(lw,up)
(0>lw) && (0<up)   # 0 is in confidence interval of [lw, up], so accept the hypothesis Ho.

## Method 2:using ANCOVA
mod1 = aov(weight~height*gender, data = tmp)
summary(mod1)
#               Df Sum Sq Mean Sq F value   Pr(>F)    
#height          1  46370   46370 599.418  < 2e-16 ***
#gender          1   4710    4710  60.880 3.53e-14 ***
#height:gender   1    132     132   1.704    0.192    
#Residuals     503  38912      77                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#The ANCOVA model shows that there is no significant interaction between gender and height.
#This suggests that the slope of the regression is similar for both males and females.