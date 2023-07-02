library(tidyverse)
install.packages('car')

library('car')
setwd("C:/Users/user/Desktop/ChlinSoft")
data <- read.csv("audi.csv")

names(data)
str(data)

summary(data)

view(data)

plot(data$year,data$tax)

ggplot(data, aes(x=price, y=mileage, color=fuelType)) + 
  geom_point(alpha=0.3)+
  geom_density(aes(y=price))

ggplot()+
  geom_density(data=data, mapping=aes(x=price,fill = fuelType), alpha = 0.3)

ggplot()+
  geom_histogram(data=data, mapping=aes(x=price,fill = fuelType), alpha = 0.3)

# first observation 

plot(data$price, data$mileage)

data %>%
  filter(mileage < 180000) %>%
  ggplot(data, mapping=aes(y=price, x=mileage, color=transmission)) + 
  geom_point() +
  facet_wrap(~transmission)

FirstData <- data %>%
  filter(mileage < 180000)
  
ggplot(FirstData, aes(y=log(price), x=mileage, color=transmission)) + 
  geom_point()


model11 <- lm(log(price) ~ mileage + tax, FirstData)
summary(model11)

residualPlot(model11)
densityPlot(model11$residuals)
durbinWatsonTest(model11)

plot(model11$residuals)
hist(model11$residuals)


# second observation

SecondData <- data %>%
  filter(mileage < 180000 & mpg < 100)

ggplot(SecondData, aes(x=mpg, y = mileage)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

model21 <- lm(mpg ~ mileage, SecondData)
summary(model21)

table(data$transmission)

ggplot(data, mapping = aes(x=model, y=tax)) + 
  geom_boxplot()

# a good MPG is around 50 to 60
# 1 water of galon is 3.79 litr

summary(data$mpg)

data %>%
  mutate(good_mpg = ifelse(mpg >= 50 & mpg <=60, 'Good MPG', ' '))

# can not make it add to columns
view(data)

model1 <- lm(price ~ mileage, data)
summary(model1)
residualPlot(model1)


# It is heteroscadastic because the blue line is far away from the black
plot(model1$residuals)
hist(model1$residuals)
par(mfrow = c(2, 2))
par(mar = c(1, 1, 1, 1))
plot(model1)

data$pred <- model1$fitted.values

data %>%
  filter(mileage < 180000) %>%
  ggplot(mapping=aes(y=price, x=mileage, color='black')) + 
  geom_point(mapping=aes(x=price, y=pred, color='skyblue3'))
dev.off()

# remove outlier
# why is model1 large ? and can not plot

plot(x=data$price, y=data$mileage)

data %>%
  filter(mileage < 180000 & price > 20000) %>%
  ggplot(data, mapping=aes(x=mileage, y=1/price, color=transmission)) + 
  geom_point() +
  facet_wrap(~transmission)


boxplot(data$type, data$year)

hist(data$year)

# day2

data$pred <- model1$fitted.values

plot(data$mpg, data$price)



data1 <- data %>%
  filter(mpg < 100)

data2 <- data %>%
  filter(mpg < 100 & price < 90000)

model1 <- lm(log(price) ~ mpg, data1)
summary(model1)

residualPlot(model1)
densityPlot(model1$residuals)
durbinWatsonTest(model1)

plot(model1$residuals)
hist(model1$residuals)


model2 <- lm(log(price) ~ mpg, data2)
summary(model2)
residualPlot(model2)
densityPlot(model2$residuals)
durbinWatsonTest(model2)


model3 <- glm(log(mpg) ~ price, data2, family = poisson(link='log'))
summary(model3)
residualPlot(model3)
densityPlot(model3$residuals)
durbinWatsonTest(model3)

install.packages('rsq')
library(rsq)

ggplot(data, aes(log(mpg), price)) +
  geom_point()+
  geom_smooth(method = 'glm', formula = y~x, method.args=list(family='poisson'))


data4 <- data %>%
  filter(price < 100000 & log(mpg) < 4.5) 
  
  ggplot(data4, aes(log(mpg), price)) +
  geom_point()+
  geom_smooth(method = 'glm', formula = y~x, method.args=list(family='poisson'))

model4 <- lm(log(mpg)~ price, data4)
summary(model4)
residualPlot(model4)
densityPlot(model4$residuals)
durbinWatsonTest(model4)

plot(model4$residuals)
hist(model4$residuals)
  
# It is heteroscadastic because the blue line is far away from the black
# r squared shows that the residuals are quite far from the linear model
# if the mpg is between 50 and 60 the car mpg is efficient and economical


par(mfrow = c(1, 1))
par(mar = c(1, 1,1,1))
plot(model1,2)
dev.off()
# remove outlier in lm, the one that is bigger 300.000
