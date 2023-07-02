my_data <- read.csv("Real estate.csv")

library(tidyverse)


names(my_data) <- c("ID", "Deal_Date", "House_Age", "Station_Distance", "Nearby_Stores","Latitude","Longitude","Price_Per_Meter")
names(my_data)


View(my_data)


summary(my_data)

par(mfrow=c(1,1))

plot(my_data$House_Age, my_data$Price_Per_Meter)



my_data %>%
  filter(House_Age < 24 & Price_Per_Meter < 82) %>%
  ggplot(aes(x=House_Age, y=Price_Per_Meter)) +
  geom_abline(intercept = 60, slope = -2.1)+
  geom_point() +
  geom_smooth(method = "lm")



my_data %>%
  filter(House_Age > 24 & Price_Per_Meter < 82) %>%
  ggplot(aes(x=House_Age, y=Price_Per_Meter)) + 
  geom_abline(intercept = -28, slope = 1.9)+
  geom_point() +
  geom_smooth(method = "lm")

my_data %>%
  ggplot(aes(x=House_Age, y=Price_Per_Meter))+
  geom_abline(intercept = -28, slope = 1.9)+
  geom_abline(intercept = 62, slope = -2)+
  geom_point()


rm(my_data2)


my_data2 <- my_data

model1 <- lm(Price_Per_Meter~ House_Age, my_data2)
summary(model1)

plot(model1$residuals)
hist(model1$residuals)


my_data2$pred <- model1$fitted.values

ggplot(my_data2[1:414,])+ geom_point(mapping = aes(x=House_Age, y=Price_Per_Meter), color='skyblue3')+
  geom_point(mapping = aes(x=House_Age, pred), color='orangered')

my_data2 %>%
  filter(House_Age > 24 & Price_Per_Meter < 82) %>%
  ggplot() + geom_point(mapping=aes(x=House_Age, y=Price_Per_Meter), color='skyblue3')+
  geom_point(mapping = aes(x=House_Age, pred), color='orangered')

model1 <- lm(House_Age ~ Price_Per_Meter, my_data2[1:414,])

model2 <- lm(House_Age ~ Price_Per_Meter, my_data2[30:200,])

summary(model1)
par(mfrow = c(2, 2))
par(mfrow=c(1,1))
plot(model1, 2)
plot(model2, 2)



summary(model11)
view(my_data2)

rm(my_data2)

# my_data2 <- add_row(415, 2012, 400, 1000,1000,1000,1000, 1000,1000) couldn't make this work




my_data2 <- my_data2[-415,]
my_data2[415,] <- c(415, 2012, 200, 1000,600,1000,1000, 1000,1000)
my_data2 <- as.data.frame(rbind(my_data2[415,], my_data2))
str(my_data2)

model11 <- lm(House_Age ~ Price_Per_Meter, my_data2)
plot(model11,2)


#  with if statement, color > 50 < 50 per meter




my_data2 %>%
  ggplot(aes(x=log(House_Age), y=log(Price_Per_Meter))) +
  geom_point()

my_data2$Deal_Date <- as.integer(my_data2$Deal_Date)
str(my_data2)


ggplot(my_data2, aes(x=House_Age, fill=Deal_Date))+
  geom_boxplot()+
  jitter(x=Price_Per_Meter)

boxplot(my_data2$House_Age, my_data2$Price_Per_Meter)

boxplot(my_data$House_Age, my_data$Price_Per_Meter)

ggplot(my_data2, aes(x=House_Age,y=Price_Per_Meter, color=Deal_Date))+
  geom_point()

barplot(table(my_data$House_Age))

#how to color the group the color you want

# difference between df and df[1:100,] ggplots


my_data3 <- my_data

my_data3$old_bin <- ifelse(my_data3$House_Age >24, 1,0)
my_data3


model_1 <- lm(Price_Per_Meter~House_Age+old_bin, my_data3)
summary(model_1)

# description of models, coeffecient
# r squared improvements
# pred 
# residuals from the model


df <- read.csv("Real estate.csv")


df$old_bin <- ifelse(df$House_Age>24, 1, 0)
df$age_new <- ifelse(df$House_Age>24, 0, df$House_Age)
df$age_old <- ifelse(df$House_Age>24, df$House_Age, 0)

df_n <- df[-271,]
model_1 <- lm(Price_Per_Meter~age_new+old_bin+age_old, df_n)
summary(model_1)

df_n$pred <- model_1$fitted.values

