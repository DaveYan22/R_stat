#distributions
#smoker/age and charges
#scatterplot for continous
#boxplot, continuoes and categorical
#histogram for one variable
#density + histogram 

library(tidyverse)

view(data)
data <- read.csv("expenses.csv")

head(data)


plot(data$age, data$charges)  # how to group using plot



ggplot(data) + 
  geom_point(aes(x=data$age, y=data$charges, colour = data$sex))+
  facet_wrap(~sex)

plot(data[data$age > 30, "age"],  data[data$age > 30, "charge"] )


boxplot(x = data$age, y = data$charges)


attach(data)

ggplot(data) + 
  geom_bar(aes(y=age,  fill= sex))

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
  geom_point()


data %>%
  filter(data$charges < 30000 & data$charges > 10000) %>%
  ggplot(aes(x = age, y =charges, colour = sex)) + 
  geom_point()


hist(rnorm(data)) %>%
  density(x= age, y=charges)



# 3 gic, if else, diagonal gic



ggplot() + 
  geom_boxplot(data, mapping =aes(x=as.factor(children), y=charges))


ggplot() + 
  geom_boxplot(data, mapping =aes(x=region, y=charges))


ggplot() + 
  geom_boxplot(data, mapping =aes(x=region, y=charges))



data %>%
  #filter(bmi >30) %>%
  ggplot()+
  geom_point(aes(x=age, y = charges, color = sex)) +
  facet_wrap(~sex) + 
  geom_abline(intercept = 800, slope = 300)+
  geom_abline(intercept = 20000, slope = 300) +
  scale_y_sqrt()



ggplot(data)+
  geom_point(data=data, mapping=aes(x=age, y = charges^0.8, color="oranged")) +
  geom_abline(intercept = 250, slope = 39)+
  geom_abline(intercept = 2700, slope = 34) 

data %>%
  mutate(line1 =ifelse(charges^0.8 < 250+39*age, T, F)) %>%
  filter(line1) %>%
  ggplot() + geom_point(mapping =aes(x=age, y=charges))

#changing x from y


ggplot(data)+
  geom_point(data=data, mapping=aes(x=age^1.5, y = charges, color="oranged")) +
  geom_abline(intercept = 3000, slope = 32)+
  geom_abline(intercept = 23000, slope = 32) 

data %>%
  mutate(line1 =ifelse(charges < 3000+39*age^1.5, T, F)) %>%
  filter(line1) %>%
  ggplot() + geom_point(mapping =aes(x=age, y=charges))


data %>%
  mutate(line1 =ifelse(charges < 3000+39*age^1.5, T, F),
         line2 =ifelse(charges < 23000+31*age^1.5, T, F),
         line3 =ifelse(charges < 3000+39*age^1.5, T, F)
         age_scale = age^1.5) %>% filter(line1) %>%
  ggplot() + geom_point(mapping =aes(x=age, y=charges))


# x for categorical and use boxplot


ggplot(data=data, aes(x=smoker, y=charges))+
  geom_boxplot()



ggplot() + geom_point(data=data, mapping = aes(x=age, y=charges, color=smoker,shape=overweight))


