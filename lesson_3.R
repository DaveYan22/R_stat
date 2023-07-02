#distributions
#smoker/age and charges
#scatterplot for continous
#boxplot, continuoes and categorical
#histogram for one variable
#density + histogram 
library(ggplot2)
library(dplyr)
install.packages("datarium")
library(datarium)
  
data = read.csv(file = "expenses.csv")

head(data)


plot(data$age, data$charges)  # how to group using plot

ggplot(data) + 
  geom_point(aes(x=data$age, y=data$charges, colour = data$sex))+
  facet_wrap(~sex)+geom_smooth(aes(x=data$age, y=data$charges))

ggplot(data)+geom_boxplot(aes(x=data$sex, y=data$charges, colour=sex))

plot(data[data$age > 30, "age"],  data[data$age > 30, "charge"] )

hist(data$charge)

boxplot(x = data$age, y = data$charges)


attach(data)

ggplot(data) + 
  geom_bar(aes(y=age,  fill= sex), position = "fill")

ggplot(data, aes(x = age, y=charges, colour= sex)) + 
  geom_point()

addmargins(table(data$charges, data$sex))


table(sex)
ggplot(data, aes(x = charges, y=age)) + 
  geom_boxplot() +
  geom_jitter()


plot(data[data$charges < 18000, "age"], data[data$charges < 18000, "charges"])


data %>%
  filter(data$charges < 18000) %>%
  ggplot(aes(x = age, y =charges, colour = sex)) + 
  geom_point()+geom_smooth(aes(x = age, y =charges, colour = sex))


data %>%
  filter(data$charges < 30000 & data$charges > 10000) %>%
  ggplot(aes(x = age, y =charges, colour = sex)) + 
  geom_point()


hist(rnorm(data)) %>%
  density(x= age, y=charges)

ggplot()+geom_boxplot(data = data, mapping = aes(x=as.factor(children), 
                                                 y=charges))

ggplot() + geom_boxplot(data, mapping=aes(x=as.factor(children), y=charges, color = smoker))

ggplot()+geom_boxplot(data = data, mapping = aes(x=region, y=charges))
                                                       

ggplot(data) + 
  geom_point(aes(x=age^1.5, y=charges), color = 'orangered')+
  geom_abline(intercept = 3000, slope = 32)+
  geom_abline(intercept = 23000, slope = 31)


ggplot(data) + 
  geom_point(aes(x=age^1.5, y=charges), color = 'orangered') +
  geom_abline(intercept = 2500, slope = 32)+
  geom_abline(intercept = 22000, slope = 32)

data <- data %>% mutate(line1=ifelse(charges<3000+32*age^1.5, T, F),
                        line2=ifelse(line1 == F & charges<23000+31*age^1.5, T, F),
                        line3=ifelse(line1==F & line2==F, T, F),
                        age_scale = age^1.5,
                        line_index = line1 + 2*line2 + 3*line3,
                        overweight = ifelse(bmi>30, "Overweight", "Not Overweight"),
                        line_group = ifelse(smoker =="no", 1 , 
                                            ifelse(smoker == "yes" & overweight=="Overweight", 3, 2)))


data2 <- data %>% mutate(line1 =ifelse(charges<2500+32*age^1.5, T, F),
                         line2 =ifelse(line1 == F & charges<22000+32*age^1.5, T, F),
                         line3 =ifelse(line2 ==F & charges >22000+32*age^1.5, T, F),
                         age_scale = age^1.5,
                         line_index = line1 + 2*line2 +3*line3,
                         overweight = ifelse(bmi>30, "overweight", "not overweight"),
                         line_group = ifelse(smoker == "no", 1, 
                                             ifelse(smoker =="yes" & overweight=="overweight", 3,2)))




ggplot(data = data2, mapping = aes(x=age, y=charges, color = as.factor(line_index)))+
  geom_point()

ggplot(data = data2, mapping = aes(x=smoker, y=charges))+geom_boxplot()

ggplot()+geom_point(data2,mapping = aes(x=age, y=charges, color=smoker, shape=overweight))

ggplot()+geom_point(data2,mapping = aes(x=age, y=charges, shape=smoker, color=as.factor(children)))

ggplot()+geom_point(data2,mapping = aes(x=age, y=charges, color = as.factor(line_group)))


data2 %>%
  ggplot(aes(x = age, y =charges, colour = sex)) + 
  geom_point()+geom_smooth(aes(x = age, y =charges, colour = sex)) +
  facet_wrap(~sex)


#1. Pick/create 3 graphs write description ( patterns, relationships if any)
#2. Perform transformations
#3. Try to fit a Linear Regression using transformed data
#4. We will discuss the diagnostics and assumptions, Go through if you can 

ggplot(data2, aes(x=age, y=charges, color=as.factor(line_index)))+
  geom_point()

ggplot(data2, aes(x=age, y=charges, color =as.factor(line_index), shape=smoker)) +
  geom_point()

ggplot() + geom_boxplot(data, mapping=aes(x=age, y=charges, color = smoker)) +
  facet_wrap(~sex)

ggplot() + geom_point(data, mapping=aes(x=age, y=charges, color=smoker)) +
  facet_wrap(smoker~sex) 

ggplot() + geom_point(data2, mapping=aes(x=age, y=charges, color=smoker)) +
  facet_wrap(~region) 

data2 %>%
  filter(smoker =='yes') %>%
  ggplot() + geom_bar(mapping=aes(x=region, fill=region, color=charges))

data2 %>%
  filter(smoker =='yes') %>%
  ggplot() + geom_point(mapping=aes(x=age, y=charges, color=sex)) +
  facet_wrap(~region)
  

# more than 3 children and less, 2 colours

ggplot() + geom_point(data, mapping=aes(y=charges, x=age, color=sex))+
  geom_smooth(method = "lm", se=FALSE)


# It is clear that we have 3 different parts in the graph.


ggplot(data) + 
  geom_point(aes(x=age^1.5, y=charges), color = 'orangered') +
  geom_abline(intercept = 2500, slope = 32)+
  geom_abline(intercept = 22000, slope = 32)


ggplot()+geom_point(data2,mapping = aes(x=age, y=charges, color=smoker, shape=overweight))

model <- lm(age ~ charges, data=data)
summary(model)

plot(model, 4)
plot(model, 2)

model2 <- lm(charges^1.5~ age, data=data)
model2

model3 <- lm(charges ~ age + I(age^2), data=data)
model3


plot(model3, 3)


model4 <- lm(age ~ charges + bmi+ smoker, data=data2)
model4

plot(model4, 4)


plot(model4, 2)


model5<- lm(bmi ~ age, data)
model5


plot(model5, 4)
summary(model3)



#The plot shows the relationship between age and charges.
#According to the graph you can see 3 lines of grouped variables going up.
#You can see 3 groupings colored as blue, green, orange. They were separated to make a clear visualization that we have 3 different levels.
#In the next graph we can see that people who smoke are charged the most than non-smokers, which we see from the blue group.
#Also in each group, with age, the charge goes up, so as older the more charge.
#Also you can see that people belonging to the blue group have a high rate of obesity, they are overweight.
#Interestinf fact that in the second group people who smoke almost no one have the overweight proplem, there are more people in the 3rd group with overweight propblems, but they do not smoke.
#According to the boxplot there are quite a lot who are charged alot but do not smoke. It is because of the overweight.
#Males and females have the same level of overweight problem, the plots are almost the same for both genders.
#Also it is clear that in southeast there are more smoker


table(data$smoker, data$region)
addmargins(table(data$smoker, data$region))
addmargins(table(data$smoker, data$region))/1338*100



x<-seq(1,100)
e <- rnorm(100, 0, 25)
hist(e)
y <- x*3+8 + e
df <- as.data.frame(cbind(y,x))


ggplot() + geom_point(df, mapping=aes(x=x, y=y)) +
  geom_smooth(df, mapping=aes(x=x, y=y), method='lm', se=F)


model1 <- lm(y~x, df)
summary(model1)


plot(model1$residuals)
hist(model1$residuals)
par(mfrow = c(2, 2))


df$pred <- model1$fitted.values

predict(model1,1)
ggplot() + geom_point(df, mapping=aes(x,y), color='skyblue3') +
  geom_point(mapping=aes(x,pred), color='oranged')

# qq plot mean - sd = 1