
#residuals, levarage of the model,descriptive statistic
#of the variables, ggplot with history, business report
#final model - residual, levarage plot, levenetest, shapirotest
#homoscedasticity, plot with residuals titles in x y axis labels

library(tidyverse)
library(car)
library(moments)
library(vegan)
library(BiodiversityR)

setwd("C:/Users/user/Desktop/ChlinSoft")

bmw <- read.csv('bmw.csv')
hyundi <- read.csv('hyundi.csv')
merc <- read.csv('merc.csv')
toyota <- read.csv('toyota.csv')

view(bmw)
view(hyundi)
view(merc)
view(toyota)


bmw$manufacturer <- "BMW"
bmw$country <- "Germany"
merc$manufacturer <- "Mercedes"
merc$country <- "Germany"
hyundi$manufacturer <- "Hyundai"
hyundi$country <- "Korea"
toyota$manufacturer <- "Toyota"
toyota$country <- "Japan"


colnames(hyundi)[7] <- "tax"
df <- rbind(bmw, merc, hyundi, toyota)

df$country <- as.factor(df$country)
df$age <- abs(df$year-2020)

df$km <- 1.6 * df$mileage
df$km <- round(df$km, digits=0)
df <- df %>% relocate(km, .after=mileage)
view(df)

summary(df)
# we have a car that is 15 years old car
# we have mpg that is 470
# we have a mileage that is 60000
# price is 50000
# tax is 580
# engineSize is 6.6 at max
view(df)

# make the qq plot and r squared better



ggplot(data=df, aes(y=log(price), x=mileage, color=country)) + 
  geom_point()+
  facet_wrap(~country)


ggplot(df4, aes(y=price, x=age, color=manufacturer)) + 
  geom_point()+
  facet_wrap(~manufacturer)+
  xlab("Car Price") + ylab("Years Used") + 
  theme(axis.title.x = element_text(colour="DarkGreen", size=10),
        axis.title.y = element_text(colour="DarkRed", size=10),
        axis.text.x = element_text(size = 5),
        axis.text.x = element_text(size = 5))

  
ggplot(data=df, aes(y=price, x=year, color=country)) + 
  geom_point() + 
  facet_grid(~country)


ggplot(data=df4, aes(y=price, x=year, color=country)) + 
  geom_boxplot() + 
  facet_grid(~manufacturer)


ggplot(data=df, aes(y=price, x=age, color= country))+
  geom_point()+
  stat_smooth(method = 'lm', color = 'black') + 
  facet_wrap(~country)+
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0.5,1.5))


view(df)
df2 <- df %>%
  mutate(df_outlier = df$price < 50000 
         & df$engineSize>0 & df$mileage < 60000 & df$age < 15)

df3 <- df2 %>%
  filter(df_outlier == T)

df4 <- df[-c(22854, 31219,8836, 23544, 28662,
             22790, 22640, 2233, 28148, 8698, 3639, 35455, 20432, 28640, 9982),]
view(df4)

summary(log(df$price))
grubbs.test(df$price)

model1 <-lm(age ~ mileage +
              engineSize+
              price +
              transmission+
              fuelType, df4)
summary(model1)

model2 <- lm(age ~ log(mileage)+
               engineSize+transmission+fuelType +
               manufacturer+
               log(price), df4)
summary(model2)

plot(df4$price, df4$age)
plot(log(df4$price), df4$age)

model3 <- lm(log(price) ~ log(mileage)+
               engineSize +transmission+fuelType +
               manufacturer+
               age, df4)
summary(model3)

model4 <- lm(log(price) ~ mileage +
               engineSize + age+
               transmission + fuelType +
               manufacturer, df4)
summary(model4)

hist(model4$residuals)
plot(model4$residuals, log(df4$price))

par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
dev.off()
plot(model4, 2)
# how to make an interval for qq plot
residualPlot(model4)
densityPlot(model4$residuals)
plot(model4$residuals, model4$age)
plot(model4$residuals, model4$mileage)
# removing outliers effects negativly on R squared

boxplot(log(df$price))

cor.test(log(df4$price), df4$age)
cor.test(log(df$price^0.5), df$age^0.8)

install.packages('outliers')
library(outliers)
dixon.test(df4$age)



