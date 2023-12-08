load("~/Desktop/DANA-Quant/Final/antibiotics_2010.Rdata")
head(antibiotics.2010)
View(antibiotics.2010)
attach(antibiotics.2010)

model1=lm(consumption~edu+factor(metro))
summary(model1)

model2=lm(consumption~edu+metro+metro:edu)
summary(model2)

anova(model1,model2)

model3=lm(consumption~edu+metro+metro*edu)
summary(model3)

anova(model1,model3)

new1=data.frame(edu=c(10),metro="Yes")
predict(model3,newdata=new1,interval="prediction",level=0.95)


model4=lm(consumption~popdensity)
summary(model4)
#independent
#scatter
plot(x=popdensity, y=consumption, ylab = "Average monthly antibiotic consumption"
     , xlab = "Population density (1,000 people/km^2)", 
     main = "Scatterplot",
     col="blue")

residuals(model4)

#residual
plot(x=popdensity, y=residuals(model4), 
     xlab = "Population density (1000 people/km^2)", ylab = "Residulas", 
     main = "Residual Plot",)
s=summary(model4)$sigma
summary(model4)$sigma
abline(h=2*s, lty="dashed", col="red")
abline(h=-2*s, lty="dashed", col="red")


#log of independent
model4_2=lm(consumption~log(popdensity))
summary(model4_2)
#independent
#scatter
plot(x=log(popdensity), y=consumption, ylab = "Average monthly antibiotic consumption"
     , xlab = "Population density (1,000 people/km^2)", 
     main = "Log Scatterplot",
     col="blue")

residuals(model4)

#residual
plot(x=log(popdensity), y=residuals(model4_2), 
     xlab = "Population density (1000 people/km^2)", ylab = "Residulas", 
     main = "Log Residual Plot",)
s_2=summary(model4_2)$sigma
summary(model4)$sigma
abline(h=2*s_2, lty="dashed", col="red")
abline(h=-2*s_2, lty="dashed", col="red")



#sqrt of independent
model4_3=lm(consumption~sqrt(popdensity))
summary(model4_3)
#independent
#scatter
plot(x=sqrt(popdensity), y=consumption, ylab = "Average monthly antibiotic consumption"
     , xlab = "Population density (1,000 people/km^2)", 
     main = "square root Scatterplot",
     col="blue")

residuals(model4)

#residual
plot(x=sqrt(popdensity), y=residuals(model4_3), 
     xlab = "Population density (1000 people/km^2)", ylab = "Residulas", 
     main = "Sqrt Residual Plot",)
s_3=summary(model4_3)$sigma
summary(model4)$sigma
abline(h=2*s_3, lty="dashed", col="red")
abline(h=-2*s_3, lty="dashed", col="red")



