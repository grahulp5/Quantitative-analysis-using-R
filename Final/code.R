#chapter 1

#two means
DIETS <- read.csv("~/Desktop/DANA-Quant/Chapter 1/DIETS.csv")
View(DIETS)
attach(DIETS)
head(DIETS)
summary(DIETS)

#CI_E1.16
t.test(WTLOSS~DIET,conf.level=0.95)

qt(0.002256,193.94,lower.tail = FALSE)

#Summary Statistics
library(mosaic)
favstats(WTLOSS~DIET)

#Test_Independent_Samples

t.test(WTLOSS~DIET,conf.level=0.95,alternative = c("greater"))
#Pooled_Test
READING <- read.csv("~/Desktop/DANA-Quant/Chapter 1/READING.csv")
attach(READING)
View(READING)
head(READING)
summary(READING)
t.test(SCORE~METHOD, conf.level=0.95, alternative = c("two.sided"), var.equal=TRUE)
boxplot(SCORE~METHOD)
favstats(SCORE~METHOD) 

qt(0.1364,20,lower.tail = FALSE)

#Paired_Test
PAIRED <- read.csv("~/Desktop/DANA-Quant/Chapter 1/PAIRED.csv")
View(PAIRED)
attach(PAIRED)
summary(PAIRED)
head(PAIRED)
t.test(NEW, STANDARD, conf.level=0.95, alternative = c("greater"), paired=TRUE)

qt(0.05,7,lower.tail = FALSE)
#F_Test
favstats(SCORE~METHOD) 
var.test(SCORE[METHOD=="STD"],SCORE[METHOD=="NEW"], alternative = "two.sided")

#Z test
BONES <- read.csv("~/Desktop/DANA-Quant/Chapter 1/BONES.csv")
View(BONES)
attach(BONES)
head(BONES)
summary(BONES)

t.test(BONES,mu=8.5, alternative='two.sided')

p_value=2*pnorm(4.0303,lower.tail = FALSE)
p_value
###Rejection Region

z = qnorm((0.05)/2, lower.tail = FALSE)
z
sd(Ratio)
#ztest
install.packages("tseries")
library(tseries)
z.test(Ratio, mu = 8.5, sigma = 1.203565, alternative = 'two.sided', conf.level = 0.95)

####T-Test

BENZENE <- read.csv("~/Desktop/DANA-Quant/Chapter 1/BENZENE.csv")
View(BENZENE)
attach(BENZENE)
head(BENZENE)
summary(BENZENE)

t.test(?..Benzene,mu=1, alternative='greater')

#confidence interval CI
ATTENTIMES <- read.csv("~/Desktop/DANA-Quant/Chapter 1/ATTENTIMES.csv", sep="")
View(ATTENTIMES)
attach(ATTENTIMES)
head(ATTENTIMES)
summary(ATTENTIMES)

t.test(Attention, conf.level = 0.99)
sd(Attention)
AttentionTime=Attention

norm.interval = function(AttentionTime, variance = var(AttentionTime), conf.level = 0.99)
{z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
xbar = mean(AttentionTime)
sdx = sqrt(variance/length(AttentionTime))
c(xbar - z * sdx, xbar + z * sdx)}

norm.interval(AttentionTime)

######
SILICA<- read.csv("~/Desktop/DANA-Quant/Chapter 1/SILICA.csv", sep="") 
View(SILICA)
attach(SILICA)
head(SILICA)
summary(SILICA)
t.test(SiliconDioxide, conf.level = .95)
sd(SiliconDioxide)

#normal distribution
mosaic::xpnorm(65,68,2.5)
library(mosaic)
xpnorm(c(-1.96,1.96))
xqnorm(c(-4.0303,4.0303))
xpt(3.1692,lower.tail = FALSE)
xpt(3.1692,19, lower.tail = FALSE)
###
xcnorm(.9)

###F_DIS
xpf(1.18,11,9)


#chapter 3

#fire dam
FIREDAM <- read.delim("~/Desktop/DANA-Quant/Chapter 3/FIREDAM.txt")
attach(FIREDAM)
View(FIREDAM)
head(FIREDAM)
plot(x=DISTANCE, y=DAMAGE, ylab = "Sales Revenue ($1000s)", xlab = "Advertising Expenditure ($100s)", 
     main = "Scatterplot of Sales Revenue vs. Advertising Expenditure",
     col="blue", pch=19, , ylim = c(2,55), xlim = c(0,8))

abline(lm(SALES_Y~ADVEXP_X), col="Red", lty=1, lwd=2)


#Regression
model=lm(SALES_Y~ADVEXP_X)
summary(model)
#adding Predicted points
points(x=ADVEXP_X, y= predict(model), col="red",pch=19)
legend("topleft", c("Predicted Sales Revenue","Observed Sales Revenue"), 
       cex = 0.7, fill = c("Red","Blue"))
#Prediction
New=data.frame(ADVEXP_X=c(4,5))
predict(model,New)

##ANOVA Tabel
anova(model)
coef(model)
#CV
s=sqrt(sum(model$residuals^2)/(length(SALES_Y) -2))
cv=(s/mean(SALES_Y))*100

#Rejection Region for the slope
library(mosaic)
xct(0.95,3)

#t-value
qt(0.025,3, lower.tail = FALSE)

#95% confidence intervals for the slope and intercept
round(confint(model,level=0.95),3)

##Cor
cor(x=DISTANCE, y=DAMAGE)
cor.test(x=DISTANCE, y=DAMAGE)
#Rho test
CASINO <- read.delim("~/Desktop/DANA-Quant/Chapter 3/CASINO.txt")
attach(CASINO)
head(CASINO)
View(CASINO)
cor(x=EMPLOYEES, y=CRIMERAT)
plot(x=EMPLOYEES, y=CRIMERAT, ylab = "Yearly Crime Rate",
     xlab = "Number of Casino Employees (thousands)", 
     main = "Scatterplot of Crime Rate vs. Number of Casino Employees",
     col="blue", ylim = c(1,4.5), xlim = c(15,40), pch=19)

cor.test(x=EMPLOYEES,y=CRIMERAT)

detach(CASINO)

#Tires
TIRES <- read.delim("~/Desktop/DANA-Quant/Chapter 3/TIRES.txt")
attach(TIRES)
head(TIRES)
plot(x=PRESS_X, y=MILEAGE_Y, ylab = "Mileage (thousands)",
     xlab = "Pressure (pounds per sq. inch)", 
     main = "Scatterplot of Mileage vs. Pressure",
     col="red", ylim = c(24,40), xlim = c(29,38), pch=19)

cor.test(x=PRESS_X, y=MILEAGE_Y)
detach(TIRES)
##95% CI and PI
model=lm(SALES_Y~ADVEXP_X)
predict(model,newdata=data.frame(ADVEXP_X=4),
        interval="confidence",level=0.95)

predict(model,newdata=data.frame(ADVEXP_X=4),
        interval="prediction",level=0.95)

#95% CI and PI Plot
plot(x=ADVEXP_X, y=SALES_Y, ylab = "Sales Revenue ($1000s)", xlab = "Advertising Expenditure ($100s)",
     main="Regression",col="blue", ylim = c(-2,6), xlim = c(-2,7), pch=19)
model=lm(SALES_Y~ADVEXP_X)
abline(lm(SALES_Y~ADVEXP_X), col="Red", lty=1, lwd=2)

newx <- seq(1, 5, by=0.005)
CI=predict(model,newdata=data.frame(ADVEXP_X=newx),
           interval="confidence",level=0.95)

PI=predict(model,newdata=data.frame(ADVEXP_X=newx),
           interval="prediction",level=0.95)

lines(newx, CI[ ,2], col="green", lty=2)
lines(newx, CI[,3], col="green", lty=2)
lines(newx, PI[,2], col="orange", lty=2)
lines(newx, PI[,3], col="orange", lty=2)
legend("topleft", c("95% confidence interval", "95% prediction interval"), 
       cex = 0.7, fill = c("green", "orange"))

##or using ggplot
library("ggplot2")
pred=predict(model, interval = "prediction")
new_df = cbind(ADSALES, pred)

ggplot(new_df, aes(ADVEXP_X, SALES_Y))+
  geom_point()+
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)

#regression sales
setwd("~/Desktop/DANA-Quant/Chapter 3/")
A <-read.table("ADSALES.txt", header=TRUE)
ADSALES <- read.delim("~/Desktop/DANA-Quant/Chapter 3/ADSALES.txt")
attach(ADSALES)
View(ADSALES)
head(ADSALES)
plot(x=ADVEXP_X, y=SALES_Y, ylab = "Sales Revenue ($1000s)", xlab = "Advertising Expenditure ($100s)", 
     main = "Scatterplot of Sales Revenue vs. Advertising Expenditure",
     col="blue", ylim = c(0,5), xlim = c(0,6), pch=19)

abline(lm(SALES_Y~ADVEXP_X), col="Red", lty=1, lwd=2)
#more Lines
abline(a=-0.1, b=0.8, col="green")
abline(a=0, b=.9, col="black")
abline(a=0.1, b=0.5, col="darkviolet")
legend("topleft", c("Regression Line"), cex = 0.7, fill = c("Red"))
text(3, 2.5,expression ("y^ = -0.1 + 0.7x"))
#adding segment and point
segments(4,2, 4, 2.7, col="Black", lty = 2)
points(4, 2.7, col="Black", pch=19)
text(4.2, 3,expression (hat(y) ==2.7 ("Predicted y")))
text(4.1, 1.7,expression (y ==2 ("Observed y")))

#Regression
model=lm(SALES_Y~ADVEXP_X)
summary(model)
#adding Predicted points
points(x=ADVEXP_X, y= predict(model), col="red",pch=19)
legend("topleft", c("Predicted Sales Revenue","Observed Sales Revenue"), 
       cex = 0.7, fill = c("Red","Blue"))
#Prediction
New=data.frame(ADVEXP_X=c(4,5))
predict(model,New)

##ANOVA Tabel
anova(model)
coef(model)
#CV
s=sqrt(sum(model$residuals^2)/(length(SALES_Y) -2))
cv=(s/mean(SALES_Y))*100
cv

cv_1=(sd(SALES_Y)/mean(SALES_Y))*100
cv_1
#Rejection Region for the slope
library(mosaic)
xct(0.95,3)

#t-value
qt(0.025,3, lower.tail = FALSE)

#95% confidence intervals for the slope and intercept
round(confint(model,level=0.95),3)
#Rho test
attach(CASINO)
head(CASINO)
cor(x=EMPLOYEES, y=CRIMERAT)
plot(x=EMPLOYEES, y=CRIMERAT, ylab = "Yearly Crime Rate",
     xlab = "Number of Casino Employees (thousands)", 
     main = "Scatterplot of Crime Rate vs. Number of Casino Employees",
     col="blue", ylim = c(1,4.5), xlim = c(15,40), pch=19)

cor.test(x=EMPLOYEES, y=CRIMERAT)
detach(CASINO)

attach(TIRES)
head(TIRES)
plot(x=PRESS_X, y=MILEAGE_Y, ylab = "Mileage (thousands)",
     xlab = "Pressure (pounds per sq. inch)", 
     main = "Scatterplot of Mileage vs. Pressure",
     col="blue", ylim = c(24,40), xlim = c(29,38), pch=19)

cor.test(x=PRESS_X, y=MILEAGE_Y)
detach(TIRES)
##95% CI and PI
model=lm(SALES_Y~ADVEXP_X)
predict(model,newdata=data.frame(ADVEXP_X=4),
        interval="confidence",level=0.95)

predict(model,newdata=data.frame(ADVEXP_X=4),
        interval="prediction",level=0.95)

#95% CI and PI Plot
plot(x=ADVEXP_X, y=SALES_Y, ylab = "Sales Revenue ($1000s)", xlab = "Advertising Expenditure ($100s)",
     main="Regression",col="blue", ylim = c(-2,6), xlim = c(-2,7), pch=19)
model=lm(SALES_Y~ADVEXP_X)
abline(lm(SALES_Y~ADVEXP_X), col="Red", lty=1, lwd=2)

newx <- seq(1, 5, by=0.005)
CI=predict(model,newdata=data.frame(ADVEXP_X=newx),
           interval="confidence",level=0.95)

PI=predict(model,newdata=data.frame(ADVEXP_X=newx),
           interval="prediction",level=0.95)

lines(newx, CI[ ,2], col="green", lty=2)
lines(newx, CI[,3], col="green", lty=2)
lines(newx, PI[,2], col="orange", lty=2)
lines(newx, PI[,3], col="orange", lty=2)
legend("topleft", c("95% confidence interval", "95% prediction interval"), 
       cex = 0.7, fill = c("green", "orange"))

##or using ggplot
library("ggplot2")
pred=predict(model, interval = "prediction")
new_df = cbind(ADSALES, pred)

ggplot(new_df, aes(ADVEXP_X, SALES_Y))+
  geom_point()+
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)


#heart failure
heart_failure <- read.csv("~/Desktop/DANA-Quant/Chapter 3/heart_failure.csv")
View(heart_failure)

#to check number of variables & number of observations, dim->dimension
dim(heart_failure)
#looking at first 5 columns
head(heart_failure)

#taking a look at first 15 rows and all column
heart_failure[1:15,]

#taking a look at first column and all row
heart_failure[,1]

#setting age column from the dataset using "$" sign
age<-heart_failure$age
age
table(age) #getting summary

#To get summary for categorical column and get the frequency
anaemia_freq<-table(heart_failure$anaemia)
anaemia_freq
#Summary of the dataset
summary(heart_failure)

barplot(5,age,)

#multiple regression boston

library(MASS) #loading packages
install.packages("ISLR") #installing if not readily available
library(ISLR)
?Boston #checking the dataset
attach(Boston)
lm.fit=lm(medv~lstat+age,data=Boston)

summary(lm.fit)

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

install.packages("car")
library(car)
vif(lm.fit) #variance inflation

lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

summary(lm(medv~lstat*age,data=Boston))

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

plot(lstat,medv)
abline(lm.fit2)

abline(lm.fit2,lwd=3)
abline(lm.fit2,lwd=3,col='red')
abline(lm.fit2,lwd=3,pch=20)

par(mfrow=c(2,2))
plot(lm.fit2)

plot(hatvalues(lm.fit2))
which.max(hatvalues(lm.fit2))

#boston
library(MASS) #loading packages
install.packages("ISLR") #installing if not readily available
library(ISLR)
?Boston #checking the dataset

names(Boston) #getting the column names

head(Boston) #getting the first 6 col values of Boston dataset

?fix #checking more on fix function

lm.fit=lm(medv~lstat,data=Boston) #simple linear regression using medv (median values)
#as Y or response variable & lstat (lower status of population) as explanatory or predictor
attach(Boston)
lm.fit #checking the values of intercept and lstat 

summary(lm.fit) #getting the summary of median & IQR values.

names(lm.fit)

coef(lm.fit) #same as lm.fit function

confint(lm.fit) #getting the confidence interval

predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval = 'confidence') 
#the above prediction gives the lines for the max confidence with x values as 5, 10, 15
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval = 'prediction')
#the above prediction gives the lines for the max prediction line with x values as 5, 10, 15
plot(lstat,medv) #plotting the X & Y
abline(lm.fit) #fitting the line using absolute line

abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col='red')
plot(lstat,medv,col='red')
plot(lstat,medv,pch=20)
plot(lstat,medv,pch='+')
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) #checking largest number of leverage statistic


#chapter 4
#gasturbine

attach(GASTURBINE)
head(GASTURBINE)
pairs(~HEATRATE+RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = GASTURBINE, 
      main="Scatterplot Matrix of Gas Turbines")
model413=lm(HEATRATE~RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = GASTURBINE)
summary(model413)
anova(model413)


#Prediction
New=data.frame(RPM=c(7500), CPRATIO=(13.5), INLET.TEMP=(1000), EXH.TEMP=c(525),AIRFLOW=c(10))
predict(model413,New)

##95% CI and PI
predict(model413,New, interval="confidence",level=0.95)

predict(model413,New, interval="prediction",level=0.95)



#Interaction

model=lm(HEATRATE~INLET.TEMP+EXH.TEMP + INLET.TEMP:AIRFLOW+ EXH.TEMP:AIRFLOW,data = GASTURBINE)
summary(model)


model432=lm(HEATRATE~INLET.TEMP+EXH.TEMP+INLET.TEMP*AIRFLOW + EXH.TEMP*AIRFLOW,data = GASTURBINE)
summary(model432)



##CARP
CARP <- read.delim("~/Desktop/DANA-Quant/Chapter 4/CARP.txt")
attach(CARP)
View(CARP)
head(CARP)
plot(x=WEIGHT, y=ENE, ylab = "ENE (in milligrams per 100 grams of body weight per day)",
     xlab = "Body Weight (in in grams)", 
     main = "Scatterplot of ENE vs. Body Weight",
     col="blue", ylim = c(0,20), xlim = c(10,400), pch=19)
abline(lm(ENE~WEIGHT), col="Red", lty=1, lwd=2)

#QuadraticTerm
modelQuad <- lm(ENE~WEIGHT+ I(WEIGHT^2))
summary(modelQuad)


plot(x=WEIGHT, y=ENE, ylab = "ENE (in mg/100 g of body weight per day)",
     xlab = "Body Weight (in in grams)", 
     main = "Scatterplot of ENE vs. Body Weight",
     col="blue", ylim = c(0,20), xlim = c(10,400), pch=19)

QUAD<- seq(0,285.7,0.1)
lines(QUAD,predict(modelQuad,newdata=data.frame(WEIGHT=QUAD)),col="red" )

anova(modelQuad)

#Express dataset
EXPRESS <- read.delim("~/Desktop/DANA-Quant/Chapter 4/EXPRESS.txt")
View(EXPRESS)
attach(EXPRESS)
head(EXPRESS)
pairs(~Cost+Weight+Distance,data = EXPRESS, 
      main=" Matrix Scatterplot")
model=lm(Cost~Weight+Distance+ I(Weight^2)+I(Distance^2)+Distance*Weight, data = EXPRESS)
summary(model)
anova(model)

#Prediction
New=data.frame(Weight=c(5), Distance=(100))
predict(model,New)

##95% CI and PI
predict(model,New, interval="confidence",level=0.95)

predict(model,New, interval="prediction",level=0.95)





####Dummy Variables
#ACCHW dataset
ACCHW <- read.delim("~/Desktop/DANA-Quant/Chapter 4/ACCHW.txt")
View(ACCHW)
attach(ACCHW)
head(ACCHW)
table(ASSIST)
x1=ifelse(ASSIST=="FULL", 1, 0)
x2=ifelse(ASSIST=="CHECK", 1, 0)

##creating a data frame
ACCHWDUMMY=data.frame(IMPROVE=ACCHW$IMPROVE, x1, x2)
modelDUMMY=lm(IMPROVE~x1+x2)
summary(modelDUMMY)

####Partial F Test
attach(GASTURBINE)
head(GASTURBINE)
modelFull=lm(HEATRATE~RPM + CPRATIO + RPM*CPRATIO + I(RPM^2) + I(CPRATIO^2),data = GASTURBINE)
summary(modelFull)
modelReduced=lm(HEATRATE~RPM + CPRATIO+ RPM*CPRATIO  ,data = GASTURBINE)
summary(modelReduced)
anova(modelReduced, modelFull)
qt(0.9,2,61)
library(mosaic)
xqf(.9,2,61)





#Complete Example
#FLAG dataset
FLAG <- read.delim("~/Desktop/DANA-Quant/Chapter 4/FLAG.txt")
View(FLAG)
attach(FLAG)
head(FLAG)
model=lm(COST~DOTEST+I(DOTEST^2)+STATUS+STATUS*DOTEST+STATUS*I(DOTEST^2))
summary(model)
#CV
s=sqrt(sum(model$residuals^2)/((length(COST) - (dim(model.matrix(model))[2]))))
s
cv=(s/mean(COST))*100
cv
#Reduced Model
modelReduced=lm(COST~DOTEST+STATUS+STATUS*DOTEST)
summary(modelReduced)
anova(modelReduced, model)
##Plot
plot(x=DOTEST, y=COST, ylab = "Contract Cost ($1000s)", xlab = "Estimate of the cost of DOT ($1000s)", 
     main = "Scatterplot of Contract Cost vs. DOT engineer’s estimate of the cost"
     ,ylim = c(0,15000), xlim = c(0,12000), pch=19, col=ifelse(FLAG$STATUS=="1", "black", "pink"))
legend("topleft", legend = c("Status: Fixed","Status: Competitive"), 
       col = c("black", "pink"), pch =19:19, cex = 0.8)

Competitive=subset(FLAG, STATUS=="0")
Fixed=subset(FLAG, STATUS=="1")

plot(x=DOTEST, y=COST, ylab = "Contract Cost ($1000s)", xlab = "Estimate of the cost of DOT ($1000s)", 
     main = "Scatterplot of Contract Cost vs. DOT engineer’s estimate of the cost"
     ,ylim = c(0,15000), xlim = c(0,12000), pch=19, col="blue")

abline(lm(Fixed$COST~Fixed$DOTEST), col="Red", lty=1, lwd=2) 
abline(lm(Competitive$COST~Competitive$DOTEST), col="Green", lty=1, lwd=2) 
legend("topleft", legend = c("Least Squares Line for Fixed","Least Squares Line for Competitive"), 
       col = c("red", "Green"), lty=1:1, cex = 0.8)
M1=lm(Fixed$COST~Fixed$DOTEST)
summary(M1)
M2=lm(Competitive$COST~Competitive$DOTEST)
summary(M2)

###s For Reduced model
s1=sqrt(sum(modelReduced$residuals^2)/((length(COST) - (dim(model.matrix(modelReduced))[2]))))
s1
cv1=(s1/mean(COST))*100
cv1
##95% PI
New=data.frame(DOTEST=c(1386.29), STATUS=c(1))
predict(modelReduced, newdata=New, interval="prediction", level=0.95)













##Assessing the models
attach(GAS)
head(GAS)

#Train and Test Data for MAPE
sample <- sample.int(n = nrow(GAS), size = floor(.75*nrow(GAS)), replace = F) #75% of dataset 
train <- GAS[sample, ]
test <- GAS[-sample, ]

nrow(test)
nrow(train)

modelTraningFull=lm(HEATRATE~RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = train)
summary(modelTraningFull)

modelIntrac=lm(HEATRATE~INLET.TEMP+EXH.TEMP+INLET.TEMP*AIRFLOW + EXH.TEMP*AIRFLOW,data = train)
summary(modelIntrac)


# Predictions
yhat1=predict(modelTraningFull, test)
yhat2=predict(modelIntrac,test)
yhat3=predict()
yhat4=predict()
###3/ Calculate MAPE_FULL
mean((abs(test$HEATRATE-yhat1))/test$HEATRATE)*100
#INTRACTION
mean((abs(test$HEATRATE-yhat2))/test$HEATRATE)*100

GASTURBINE <- read.delim("~/Desktop/DANA-Quant/Chapter 4/GASTURBINE.txt")
View(GASTURBINE)
attach(GASTURBINE)
summary(GASTURBINE)

head(GASTURBINE)
pairs(~HEATRATE+RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = GASTURBINE, 
      main="Scatterplot Matrix of Gas Turbines")

model413=lm(HEATRATE~RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = GASTURBINE)
summary(model413)

#calculate model standard deviation

anova(model413) 
sqrt(12841935/61)

#SD is 458.8 for every x when we estimate y, we have variations, but the range of this variation is 2 Std errors.
#95% of prediction is in between this 2 std error.

#test statistic to check if Airflow is useful linear predictor of heartrate
#tc=beta/standard error
#beta for Airflow is -0.848
#standard error is 0.4421
tc<--0.848/0.4421
tc

New=data.frame(RPM=c(7500), CPRATIO=(13.5), INLET.TEMP=(1000), EXH.TEMP=c(525),AIRFLOW=c(10))
predict(model413,New)

predict(model413,New, interval="confidence",level=0.95)

predict(model413,New, interval="prediction",level=0.95)

#Interaction

model=lm(HEATRATE~INLET.TEMP+EXH.TEMP + INLET.TEMP:AIRFLOW+ EXH.TEMP:AIRFLOW,data = GASTURBINE)
summary(model)

model432=lm(HEATRATE~INLET.TEMP+EXH.TEMP+INLET.TEMP*AIRFLOW + EXH.TEMP*AIRFLOW,data = GASTURBINE)
summary(model432)

#chapter 5

#gasturbine
######Parabola

par(mfrow=c(3,2))
#beta1
eq=function(x){1-5*x+x^2}
curve(eq,from=-20, to=20, xlab="x", ylab="y", ylim=c(-5,100), xlim = c(-20,10))
title(main="1-5*x+x^2")
eq=function(x){1+5*x+x^2}
curve(eq,from=-20, to=10, xlab="x", ylab="y", ylim=c(-5,100), xlim = c(-20,10))
title(main="1+5*x+x^2")
#beta2
eq=function(x){x+10*x^2}
curve(eq,from=-10, to=10, xlab="x", ylab="y", ylim=c(0,100), xlim = c(-10,10))
title(main="x+10*x^2")
eq=function(x){x+x^2}
curve(eq,from=-10, to=10, xlab="x", ylab="y", ylim=c(0,100), xlim = c(-10,10))
title(main="x+x^2")
#-Beta2
eq=function(x){x+10*x^2}
curve(eq,from=-10, to=10, xlab="x", ylab="y", ylim=c(0,100), xlim = c(-10,10))
title(main="x+10*x^2")
eq=function(x){x-10*x^2}
curve(eq,from=-10, to=10, xlab="x", ylab="y", ylim=c(-100,10), xlim = c(-10,10))
title(main="x-10*x^2")
#######third order
par(mfrow=c(1,2))
#beta3
eq=function(x){1+x+x^2+x^3}
curve(eq, xlab="x", ylab="y", ylim=c(-5,5), xlim = c(-5,5))
title(main="1+x+x^2+x^3")
eq=function(x){1+x+x^2-x^3}
curve(eq, xlab="x", ylab="y", ylim=c(-5,5), xlim = c(-5,5))
title(main="1+x+x^2-x^3")

####
attach(GASTURBINE)
head(GASTURBINE)
plot( x= AIRFLOW,y= HEATRATE, main="HEAT RATE vs. AIR FLOW", pch=19, col="blue",
      ylim = c(8000, 20000), xlim =c(0, 800), xlab = "Air flow (kilograms per second)", 
      ylab = " Heat rate (kilojoules per kilowatt per hour)" )
model=lm(HEATRATE~AIRFLOW+I(AIRFLOW^2)+I(AIRFLOW^3),data = GASTURBINE)
summary(model)

#####5.19
model519=lm(HEATRATE~RPM+CPRATIO+RPM*CPRATIO+I(RPM^2)+I(CPRATIO^2))
summary(model519)

###RPM is Constant

new1=data.frame(RPM=c(5000), CPRATIO=GASTURBINE$CPRATIO)
y=predict(model519, newdata=new1)
plot(x=CPRATIO,y=y, main = "Scatterplot of predicted Heat Rate and CPRATIO, RPM=5000", 
     ylab = "Predicted Heat Rate", xlab ="CPRATIO", 
     ylim = c(8000, 15000), xlim=c(0,40),pch=19, col="blue")
new2=data.frame(RPM=c(15000), CPRATIO=GASTURBINE$CPRATIO)
y=predict(model519, newdata=new2)
plot(x=CPRATIO,y=y, main = "Scatterplot of predicted Heat Rate and CPRATIO, RPM=15000", 
     ylab = "Predicted Heat Rate", xlab ="CPRATIO", 
     ylim = c(8000, 15000), xlim=c(0,40),pch=19, col="blue")

##DIESEL
DIESEL <- read.delim("~/Desktop/DANA-Quant/Chapter 5/DIESEL.txt")
View(DIESEL)
attach(DIESEL)
head(DIESEL)
table(FUEL)
table(BRAND)
x1=ifelse(FUEL=="F2", 1, 0)
x2=ifelse(FUEL=="F3", 1, 0)
x3=ifelse(BRAND=="B2", 1, 0)
##creating a data frame
DIESELDUMMY=data.frame(FUELBRAND,PERFORM=DIESEL$PERFORM, x1, x2,x3)
modelDUMMY1=lm(PERFORM~x1+x2+x3)
summary(modelDUMMY1)
predict(modelDUMMY1,DIESELDUMMY, interval="confidence",level=0.95)

predict(modelDUMMY1,DIESELDUMMY, interval="prediction",level=0.95)
#%95CI for F3 B2 for Main Model
New=data.frame(x1=c(0), x2=c(1), x3=c(1))
predict(modelDUMMY1,New, interval="confidence",level=0.95)


######Interaction
modelDUMMY2=lm(PERFORM~x1+x2+x3+x1*x3+x2*x3)
summary(modelDUMMY2)

anova(modelDUMMY1,modelDUMMY2)

######Interaction no x1x3
modelDUMMY3=lm(PERFORM~x1+x2+x3+x2*x3)
summary(modelDUMMY3)

anova(modelDUMMY1,modelDUMMY2,modelDUMMY3)
#%95CI for F3 B2 for Complete Model
New=data.frame(x1=c(0), x2=c(1), x3=c(1))
predict(modelDUMMY2,New, interval="confidence",level=0.95)


##Table for a subset of the data set
Table_F3_B2=subset(DIESEL, FUEL=="F3"&BRAND=="B2")
Table_F3_B2
ybar32=mean(Table_F3_B2$PERFORM)
ybar32


###Graph of sample means for engine performance
library(ggplot2)

ggplot(DIESEL,aes(x=factor(FUEL),y=PERFORM,colour=BRAND,group=BRAND))+
  geom_point()+
  stat_summary(aes(y=PERFORM,group=BRAND),fun= mean,geom = "line")+
  ggtitle("RStudio graph of sample means for engine performance")


##Partial F Test

modelDUMMY1=lm(PERFORM~x1+x2+x3)

modelDUMMY2=lm(PERFORM~x1+x2+x3+x1*x3+x2*x3)
anova(modelDUMMY1, modelDUMMY2)

#chapter 6

#stepwise
#EXECAL
EXECSAL2 <- read.csv("/Users/rahulgupta/Desktop/DANA-Quant/Chapter 6/EXECSAL2.txt", sep="")

attach(EXECSAL2)
head(EXECSAL2)
install.packages("leaps")
library(leaps)
library(MASS)

##Model with All variables
M1=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10)
summary(M1)
# stepwise regression
step(lm(Y~.,data=EXECSAL2),direction="both")

##or using library(MASS)

stepAIC(lm(Y~.,data=EXECSAL2),direction="both")

#Forward selection

mint <- lm(Y~1,data=EXECSAL2)
forwardAIC <- step(mint,scope=list(lower=~1, 
                                   upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10),
                   direction="forward", data=EXECSAL2)

#or

forwardAIC1 <- step(mint,scope=formula(M1),
                    direction="forward", data=EXECSAL2)

forwardAIC1$coefficients
forwardAIC$coefficients
forwardAIC1$anova
forwardAIC$anova

#Backward Elimination

step(lm(Y~.,data=EXECSAL2),direction="backward")
##Best Subset

X <- cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10)


Model=regsubsets(as.matrix(X),Y, nvmax = 10)

#or
Model=regsubsets(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,data=EXECSAL2, nvmax = 10)
summary(Model)
SUM=summary(Model)
names(SUM)
Rsq=SUM$rsq
CP=SUM$cp
AdRsq=SUM$adjr2
BIC=SUM$bic
RSS=SUM$rss
#Calculation of AIC
n <- length(EXECSAL2$Y)
p <- apply(SUM$which, 1, sum)
AIC<- SUM$bic - log(n) * p + 2 * p
#number of independent variables in the models
I=p-1
I
MSE1=RSS/(n-I-1)
MSE1


###Plot
par(mfrow=c(2,2))
plot(p,Rsq,xlab="Subset Size",ylab="Adjusted R-squared", ylim=c(0.6,1), pch=19, col="blue")
plot(p,Rsq,xlab="Subset Size",ylab="R-squared", ylim=c(0.6,1), pch=19, col="blue")
plot(p,CP,xlab="Subset Size",ylab="CP", ylim=c(0, 350), pch=19, col="blue")
lines(y=p+1,x=p, col="red")
plot(p,PRESS,xlab="Subset Size",ylab="PRESS", ylim=c(0.5,2.5), pch=19, col="blue")

##PRESS


m1=lm(Y~X1)
s1=summary(m1)
m2=lm(Y~X1+X3)
s2=summary(m2)
m3=lm(Y~X1+X3+X4)
s3=summary(m3)
m4=lm(Y~X1+X2+X3+X4)
s4=summary(m4)
m5=lm(Y~X1+X2+X3+X4+X5)
s5=summary(m5)
m6=lm(Y~X1+X2+X3+X4+X5+X9)
s6=summary(m6)
m7=lm(Y~X1+X2+X3+X4+X5+X6+X9)
s7=summary(m7)
m8=lm(Y~X1+X2+X3+X4+X5+X6+X8+X9)
s8=summary(m8)
m9=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9)
s9=summary(m9)
m10=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10)
s10=summary(m10)

library(qpcR)
n1=qpcR::PRESS(m1)
a1=n1$stat
b1=s1$sigma
n2=qpcR::PRESS(m2)
a2=n2$stat
b2=s2$sigma
n3=qpcR::PRESS(m3)
a3=n3$stat
b3=s3$sigma
n4=qpcR::PRESS(m4)
a4=n4$stat
b4=s4$sigma
n5=qpcR::PRESS(m5)
a5=n5$stat
b5=s5$sigma
n6=qpcR::PRESS(m6)
a6=n6$stat
b6=s6$sigma
n7=qpcR::PRESS(m7)
a7=n7$stat
b7=s7$sigma
n8=qpcR::PRESS(m8)
a8=n8$stat
b8=s8$sigma
n9=qpcR::PRESS(m9)
a9=n9$stat
b9=s9$sigma
n10=qpcR::PRESS(m10)
a10=n10$stat
b10=s10$sigma
PRESS=c(a1,a2,a3,a4,a5,a6,a7,a8,a9, a10)
MSE=(c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10))^2
##Result
cbind(SUM$which,round(cbind(Rsq,AdRsq,CP,BIC,RSS,AIC, PRESS, MSE,MSE1),4))

#chapter 7
#coffee
FTCCIGAR <- read.delim("~/Desktop/DANA-Quant/Chapter 7/FTCCIGAR.txt")
attach(FTCCIGAR)
head(FTCCIGAR)
pairs(~CO+TAR+NICOTINE+WEIGHT,data = FTCCIGAR, 
      main="Scatterplot Matrix of FTC cigarette data")
model1=lm(CO~TAR+NICOTINE+WEIGHT,data = FTCCIGAR)
summary(model1)
anova(model1)

#vif(model1)
install.packages("car")
install.packages("carData")
library(car)

vif(model1)

#Pearson Correlation
X=cbind(TAR,NICOTINE,WEIGHT)

install.packages("Hmisc")
library(Hmisc)

rcorr(X, type="pearson")

COFFEE <- read.delim("~/Desktop/DANA-Quant/Chapter 7/COFFEE.txt")

detach(FTCCIGAR)
attach(COFFEE)
View(COFFEE)
head(COFFEE)
plot(x=PRICE, y=DEMAND, ylim = c(600,1300), xlim = c(3, 3.9), pch=19, col="blue", 
     main="Demand vs. Price", xlab = "Price (in dollars)", ylab="Weekly Demand (in pounds)")

##Adding the curve

QUAD<- seq(3,3.7,0.001)


X=1/PRICE

model2=lm(DEMAND~X)

lines(QUAD,predict(model2,newdata=data.frame(X=QUAD^(-1))),col="red" )
summary(model2)
##95% CI
New=data.frame(X=c(1/3.2))
predict(model2, newdata=New, interval="confidence", level=0.95)


#chapter 8

#fastfood coffee olympic socwork
attach(OLYMPIC)
head(OLYMPIC)
plot( x= FAT,y= CHOLESTEROL, main="Cholesterol  vs. Saturated fat",
      pch=19, col="blue",
      xlab = "Average daily intake of saturated fat (in milligrams)", 
      ylab = " Level of cholesterol (in milligrams per liter)",  ylim = c(800, 2200),  
      xlim = c(1000, 3000))

model1=lm(CHOLESTEROL~FAT,data = OLYMPIC)
summary(model1)
residuals(model1)
summary(residuals(model1))
anova(model1)
model2=lm(CHOLESTEROL~FAT + I(FAT^2),data = OLYMPIC)
summary(model2)
summary(residuals(model2))
anova(model2)

residuals(model2)
summary(residuals(model1))

abline(lm(CHOLESTEROL~FAT), col="Green", lty=1, lwd=2)
range(FAT)
QUAD<- seq(1200,2930,0.1)

lines(QUAD,predict(model2,newdata=data.frame(FAT=QUAD)),col="red")
legend("topleft", legend = c("Linear", "Quadratic"),pch=19, col = c("Green", "Red"))

##Residuals plot
plot(x=FAT, y=residuals(model1), 
     xlab = "Average daily intake of saturated fat (in milligrams)", ylab = "Residulas", 
     main = "Residual Plot",xlim=c(1100, 3000), ylim=c(-300, 200),pch=19, col="blue" )
abline(h=0, lty="dashed", col="red")

plot(x=FAT, y=residuals(model2), 
     xlab = "Average daily intake of saturated fat (in milligrams)", ylab = "Residulas", 
     main = "Residual Plot",xlim=c(1100, 3000), ylim=c(-100, 70),pch=19, col="blue" )
abline(h=0, lty="dashed", col="red")

s=summary(model2)$sigma

abline(h=2*s, lty="dashed", col="red")
abline(h=-2*s, lty="dashed", col="red")

text(x = 900, 
     y = 2*s, 
     "2*s", cex=1.3, col = "red", las = 1,
     xpd = TRUE)
text(x = 900, 
     y = -2*s, 
     "-2*s", cex=1.3, col = "red", las = 1,
     xpd = TRUE)

#Standardized Residual
plot(x=FAT, y=rstandard(model2), 
     xlab = "Average daily intake of saturated fat (in milligrams)", 
     ylab = "Standardized Residulas",
     main = "Standardized Residual Plot",xlim=c(1100, 3000), ylim=c(-4, 4),pch=19, col="blue" ) 
abline(h=2, lty="dashed", col="red")
abline(h=-2, lty="dashed", col="red")

### Diagnostic plots including normal Q–Q plot, standardized residuals against leverage   
par(mfrow=c(2,2))
plot(model2)
##Partial Residual plots
library(car)

crPlots(model1)
##Example Coffee2
attach(COFFEE2)
head(COFFEE2)
Model1=lm(DEMAND~PRICE+AD)
summary(Model1)
anova(Model1)
##Residuals plot
plot(x=PRICE, y=residuals(Model1), 
     xlab = "Price (in dollars/pound) ", ylab = "Residuals", 
     main = "Residuals vs. Price",xlim=c(2.5, 5.5), ylim=c(-60, 100),pch=19, col="blue" )
abline(h=0, lty="dashed", col="red")
#Scatter plot
plot( x= PRICE,y= DEMAND, main="Weekly demand  vs. Price",
      pch=19, col="blue",
      xlab = " Price (in dollars/pound)",
      ylab = "Demand (in pounds)", ylim = c(190, 1300), xlim = c(2.9, 5.5))
##Partial Residuals

partialRes=resid(Model1,type="partial")
head(partialRes)

###Partial Residual plots
termplot(Model1, partial.resid = TRUE,terms = "PRICE", col.res = "blue")
lm.P=lm(partialRes[,1]~PRICE)
coef(summary(lm.P))

##or
library(car)

crPlots(Model1)

##Transformation
x1=1/PRICE

Model2=lm(DEMAND~x1+AD)
summary(Model2)
###Partial Residual plots
termplot(Model2, partial.resid = TRUE,terms = "x", col.res = "blue")
crPlots(Model2)

###Example Salary
attach(SOCWORK)
head(SOCWORK)
par(mfrow=c(1,2))
plot( x= EXP,y= SALARY, main="Salary  vs. Years of Experience",
      pch=19, col="blue",
      xlab = "Experience (in years)", 
      ylab = " Salary (in dollars)",  ylim = c(10000, 110000),  
      xlim = c(0, 30))
Model4=lm(SALARY~EXP+I(EXP^2))
summary(Model4)

plot(x=fitted(Model4), y=residuals(Model4), 
     xlab = "Fitted Values ", ylab = "Residuals", 
     main = "Residuals vs. Fitted values",xlim=c(20000, 85000), ylim=c(-20000, 25000),
     pch=19, col="blue" )
abline(h=0, lty="dashed", col="red")
##ln(y)
y=log(SALARY)
Model5=lm(y~EXP+I(EXP^2))
summary(Model5)

plot(x=fitted(Model5), y=residuals(Model5), 
     xlab = "Fitted Values ", ylab = "Residuals", 
     main = "Residuals vs. Fitted values (y=log(salary))",xlim=c(9.8, 11.4), ylim=c(-0.5, 0.5),
     pch=19, col="blue" )
abline(h=0, lty="dashed", col="red")

##ln(y) first order model
Model6=lm(y~EXP)
summary(Model6)
plot(x=fitted(Model6), y=residuals(Model6), 
     xlab = "Fitted Values ", ylab = "Residuals", 
     main = "Residuals vs. Fitted values (y=log(salary))",
     pch=19, col="blue",xlim=c(9.8, 11.3), ylim=c(-0.5, 0.5) )
abline(h=0, lty="dashed", col="red")

###Normality

par(mfrow=c(1,2))
hist(Model5$residuals)
stem(Model5$residuals,scale=2)
qqnorm(Model5$residuals, main = "Normal Q-Q Plot for Residuals")
qqline(residuals(Model5))
####FASTFOOD
FASTFOOD <- read.delim("~/Desktop/DANA-Quant/Chapter 8/FASTFOOD.txt")
attach(FASTFOOD)
head(FASTFOOD)
table(CITY)
x1=ifelse(FASTFOOD$CITY=="1", 1, 0)
x2=ifelse(FASTFOOD$CITY=="2", 1, 0)
x3=ifelse(FASTFOOD$CITY=="3", 1, 0)
FASTDUMMY=data.frame(SALES=FASTFOOD$SALES,TRAFFIC=FASTFOOD$TRAFFIC,x1, x2,x3)
attach(FASTDUMMY)
ModelFast=lm(SALES~TRAFFIC+x1 +x2+x3)
summary(ModelFast)
##Residuals plot
plot(y=rstandard(ModelFast), x=TRAFFIC,
     xlab = "Traffic flow (in thousands of cars)", ylab = "Standardized Residual", 
     main = "Standardized Residuals vs. TRAFFIC",ylim=c(-4, 6), xlim=c(20, 100), pch=19, col="blue" )
abline(h=-3, lty="dashed", col="red")
abline(h=3, lty="dashed", col="red")
abline(h=0, lty="dashed", col="red")
identify(y=rstandard(ModelFast), x=TRAFFIC) #Click on outlier and click escape button to see it on graph. Cool?
FASTDUMMY[13,]

plot(y=rstandard(ModelFast), x=FASTFOOD$CITY,
     xlab = "CITY", ylab = "Standardized Residual", 
     main = "Standardized Residuals vs. CITY",ylim=c(-4, 6), xlim=c(1,5), pch=19, col="blue" )
abline(h=-3, lty="dashed", col="red")
abline(h=3, lty="dashed", col="red")
abline(h=0, lty="dashed", col="red")
identify(y=rstandard(ModelFast), x=FASTFOOD$CITY)


####correct y-value

FASTDUMMY[13,1]=8.2
FASTDUMMY[13,]
ModelFastC=lm(FASTDUMMY$SALES~FASTDUMMY$TRAFFIC+FASTDUMMY$x1 +FASTDUMMY$x2+FASTDUMMY$x3)
summary(ModelFastC)
##Residuals plot

plot(y=rstandard(ModelFastC), x=FASTDUMMY$TRAFFIC,
     xlab = "Traffic flow (in thousands of cars)", ylab = "Standardized Residual", 
     main = "Standardized Residuals vs. TRAFFIC",ylim=c(-4, 4), xlim=c(20, 100), pch=19, col="blue" )
abline(h=-3, lty="dashed", col="red")
abline(h=3, lty="dashed", col="red")
abline(h=0, lty="dashed", col="red")


plot(y=rstandard(ModelFastC), x=FASTFOOD$CITY,
     xlab = "CITY", ylab = "Standardized Residual", 
     main = "Standardized Residuals vs. CITY",ylim=c(-4, 4), xlim=c(1,5), pch=19, col="blue" )
abline(h=-3, lty="dashed", col="red")
abline(h=3, lty="dashed", col="red")
abline(h=0, lty="dashed", col="red")

####


FASTDUMMY=data.frame(SALES=FASTFOOD$SALES,TRAFFIC=FASTFOOD$TRAFFIC,x1, x2,x3)
attach(FASTDUMMY)
ModelFast=lm(SALES~TRAFFIC+x1 +x2+x3)
summary(ModelFast)

leverage <- round(hatvalues(ModelFast),3)
StanRes <- round(rstandard(ModelFast),3)
residual <- round(ModelFast$residuals,3)
cd <- round(cooks.distance(ModelFast),3)
Rstudent=round(rstudent(ModelFast),3)
cbind(SALES,TRAFFIC,leverage,residual,StanRes, cd,Rstudent )

library(mosaic)
xpf(1.196,5,19)

###Diagnostic plots 
par(mfrow=c(2,4))
plot(ModelFast)
plot(ModelFastC)

exp(-0.000652)



