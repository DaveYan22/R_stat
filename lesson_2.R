#distributions
#smoker/age and charges
#scatterplot for continous
#boxplot, continuoes and categorical
#histogram for one variable
#density + histogram 


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