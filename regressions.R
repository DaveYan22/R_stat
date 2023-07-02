library(lattice)
library(tidyverse)
library(car)
library(caret)

library(rsample)
library(ROCR)
library(pROC)
library(randomForest)
library(ResourceSelection)
library(pwr)

library(MASS)
library(ISLR2)

getwd()

setwd("C:/Users/user/Desktop/ClinSoft")
df <- read.csv('merc.csv')
# linear regression

head(df)
view(df)        

lm.fit <- lm(price ~ mileage, data= df)
summary(lm.fit)

names(lm.fit)

coef(lm.fit)
confint(lm.fit)
plot(lm.fit$residuals, df$price)
plot(lm.fit$fitted.values, lm.fit$residuals)

predict(lm.fit, data.frame(mileage = (c(5,10,20))),
        interval="confidence")

predict(lm.fit, data.frame(mileage = (c(5,10,20))),
        interval="prediction")
plot(df$mileage, df$price)
abline(lm.fit)

abline (lm.fit , lwd = 3)
abline (lm.fit , lwd = 3, col = " red ")
plot (df$mileage, df$price, col = " red ")
plot (df$mileage, df$price, pch = 20)
plot (df$mileage, df$price, pch = "+")
abline(lm.fit, lwd = 3, col = 'blue')
plot (1:20, 1:20, pch = 1:20)

par(mfrow = c(1,1))
plot (lm.fit,2)

plot( predict (lm.fit), residuals (lm.fit))
plot( predict (lm.fit), rstudent (lm.fit))

plot ( hatvalues (lm.fit))
which.max ( hatvalues (lm.fit))

vif(lm.fit)

lm.fit <- lm(price ~ mileage + mpg, data = df)
summary(lm.fit1)

lm.fit1 <- lm(price ~ ., - mpg, data = df)
summary(lm.fit1)

lm.fit1 <- update (lm.fit , ~ . - age)

summary(lm(price ~ mileage*mpg , data = df))


lm.fit2 <- lm(price ~ mileage + I(mileage^2), data=df)
summary (lm.fit2)

anova(lm.fit, lm.fit2)

par(mfrow = c(1,1))
plot(lm.fit2,4)

lm.fit3 <- lm(price ~ poly (mileage , 5), data = df)
summary (lm.fit3)

lm.fit4 <- lm(price ~ mileage:mpg, data = df)
summary(lm.fit4)

# logistic regression
df2 <- read.csv('healthcare-dataset-stroke-data.csv')
view(df2)

glm.fits <- glm(stroke~ heart_disease+age+smoking_status,
                data=df2, family = binomial)
summary(glm.fits)

roccurve <- roc(df2$heart_disease, df2$age)
roccurve

plot(roccurve)

roccurve1 <- roc(df2$hypertension, df2$avg_glucose_level)
roccurve1

plot(roccurve1)


coef(glm.fits)
                
summary(glm.fits)$coef

glm.probs <- predict(glm.fits, type = "response")
glm.probs

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

table (glm.pred, df2$smoking_status)



glm.probs <- predict(glm.fits, newdata=data.frame
        (df$heart_disease=c(10,20)), type="response")


# comment add to change