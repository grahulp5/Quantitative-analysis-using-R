#chapter 1

ATTENTIMES <- read.csv("~/Desktop/DANA-Quant/Chapter 1/ATTENTIMES.csv", sep="")
View(ATTENTIMES)
attach(ATTENTIMES)
head(ATTENTIMES)
summary(ATTENTIMES)

t.test(?..AttentionTime, conf.level = 0.99)


norm.interval = function(?..AttentionTime, variance = var(?..AttentionTime), conf.level = 0.99)
{z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
xbar = mean(?..AttentionTime)
sdx = sqrt(variance/length(?..AttentionTime))
c(xbar - z * sdx, xbar + z * sdx)}

norm.interval(?..AttentionTime)

######
SILICA<- read.csv("~/Desktop/DANA-Quant/Chapter 1/SILICA.csv", sep="") 
View(SILICA)
attach(SILICA)
head(SILICA)
summary(SILICA)
t.test(SiliconDioxide, conf.level = .95)
sd(SiliconDioxide)

#normal.R
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


#2meansR
DIETS <- read.csv("~/Desktop/DANA-Quant/Chapter 1/DIETS.csv")
View(DIETS)
attach(DIETS)
head(DIETS)
summary(DIETS)

#CI_E1.16
t.test(WTLOSS~DIET,conf.level=0.95)



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

#Paired_Test
PAIRED <- read.csv("~/Desktop/DANA-Quant/Chapter 1/PAIRED.csv")
View(PAIRED)
attach(PAIRED)
summary(PAIRED)
head(PAIRED)
t.test(NEW, STANDARD, conf.level=0.95, alternative = c("greater"), paired=TRUE)
#F_Test
favstats(SCORE~METHOD) 
var.test(SCORE[METHOD=="STD"],SCORE[METHOD=="NEW"], alternative = "two.sided")

#ztest
BONES <- read.csv("~/Desktop/DANA-Quant/Chapter 1/BONES.csv")
View(BONES)
attach(BONES)
head(BONES)
summary(BONES)

t.test(BONES,mu=8.5, alternative='two.sided')

p_value

###Rejection Region

z = qnorm((0.05)/2, lower.tail = FALSE)
z
####T-Test

BENZENE <- read.csv("~/Desktop/DANA-Quant/Chapter 1/BENZENE.csv")
View(BENZENE)
attach(BENZENE)
head(BENZENE)
summary(BENZENE)

t.test(?..Benzene,mu=1, alternative='greater')

#chapter 3
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


#firedam
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
cor(x=EMPLOYEES, y=CRIMERAT)
plot(x=EMPLOYEES, y=CRIMERAT, ylab = "Yearly Crime Rate",
     xlab = "Number of Casino Employees (thousands)", 
     main = "Scatterplot of Crime Rate vs. Number of Casino Employees",
     col="blue", ylim = c(1,4.5), xlim = c(15,40), pch=19)


detach(CASINO)
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

#heartfailure
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

#multiple regression
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

#regression adsales
setwd("~//Desktop/Langara/DANA 4810/Nooshin/Chapters/Ch3/Data")
A <-read.table("ADSALES.txt", header=TRUE)
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


#chapter 4
#heartrate
GASTURBINE <- read.delim("~/Desktop/DANA-Quant/Chapter 4/GASTURBINE.txt")
View(GASTURBINE)
attach(GASTURBINE)
summary(GASTURBINE)

attach(GASTURBINE)
head(GASTURBINE)
pairs(~HEATRATE+RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = GASTURBINE, 
      main="Scatterplot Matrix of Gas Turbines")

model413=lm(HEATRATE~RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = GASTURBINE)
summary(model413)

anova(model413) 
sqrt(12841935/61)

#SD is 458.8 for every x when we estimate y, we have variations, but the range of this variation is 2 Std errors.
#95% of prediction is in between this 2 std error.

New=data.frame(RPM=c(7500), CPRATIO=(13.5), INLET.TEMP=(1000), EXH.TEMP=c(525),AIRFLOW=c(10))
predict(model413,New)

predict(model413,New, interval="confidence",level=0.95)

predict(model413,New, interval="prediction",level=0.95)

#Interaction

model=lm(HEATRATE~INLET.TEMP+EXH.TEMP + INLET.TEMP:AIRFLOW+ EXH.TEMP:AIRFLOW,data = GASTURBINE)
summary(model)

model432=lm(HEATRATE~INLET.TEMP+EXH.TEMP+INLET.TEMP*AIRFLOW + EXH.TEMP*AIRFLOW,data = GASTURBINE)
summary(model432)

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

attach(CARP)
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


attach(EXPRESS)
head(EXPRESS)
pairs(~Cost+Weight+Distance,data = EXPRESS, 
      main=" Matrix Scatterplot")
model=lm(Cost~Weight+Distance+ I(Weight^2)+I(Distance^2)+Distance*Weight, data = EXPRESS)
summary(model)
#anova(model413)

#Prediction
New=data.frame(Weight=c(5), Distance=(100))
predict(model,New)

##95% CI and PI
predict(model,New, interval="confidence",level=0.95)

predict(model,New, interval="prediction",level=0.95)





####Dummy Variables
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


install.packages('tseries')
library(tseries)


z.test(x=73.5,stdev=6,conf.level=0.95)