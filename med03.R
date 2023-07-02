#distributions
#smoker/age and charges
#scatterplot for continous
#boxplot, continuoes and categorical
#histogram for one variable
#density + histogram 
library(ggplot2)
library(dplyr)

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

ggplot()+geom_boxplot(data = data[data], mapping = aes(x=region, 
                                                 y=charges))

ggplot(data) + 
  geom_point(aes(x=age^1.5, y=charges), color = 'orangered')+
  geom_abline(intercept = 3000, slope = 32)+
  geom_abline(intercept = 23000, slope = 31)

data <- data %>% mutate(line1=ifelse(charges<3000+32*age^1.5, T, F),
                line2=ifelse(line1 == F & charges<23000+31*age^1.5, T, F),
                line3=ifelse(line1==F & line2==F, T, F),
                age_scale = age^1.5,
                line_index = line1 + 2*line2 + 3*line3,
                overweight = ifelse(bmi>30, "Overweight", "Not Overweight"),
                line_group = ifelse(smoker =="no", 1 , 
                               ifelse(smoker == "yes" & overweight=="Overweight", 3, 2)))

ggplot(data = data, mapping = aes(x=age, y=charges, color = as.factor(line_index)))+
  geom_point()

ggplot(data = data, mapping = aes(x=smoker, y=charges))+geom_boxplot()

ggplot()+geom_point(data,mapping = aes(x=age, y=charges, color=smoker, shape=overweight))

ggplot()+geom_point(data,mapping = aes(x=age, y=charges, shape=smoker, color=as.factor(children)))

ggplot()+geom_point(data,mapping = aes(x=age, y=charges, color = as.factor(line_group)))


#1. Pick/create 3 graphs write description ( patterns, relationships if any)
#2. Perform transformations
#3. Try to fit a Linear Regression using transformed data
#4. We will discuss the diagnostics and assumptions, Go through if you can 