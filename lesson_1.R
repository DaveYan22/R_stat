

data <- read.csv("expenses.csv")


head(data)

summary(data)

boxplot(data$children)

boxplot(data$charges)

boxplot(data$bmi)

# artistic staff - outliers - writing NA or mean or removing

plot(data$bmi, data$charges) 


plot(data[data$charges>15000, "bmi"], data[data$charges>15000, "charges"])


plot(data[data$charges>32000, "bmi"], data[data$charges>32000, "charges"])

plot(data[data$charges<32000 & data$charges>15000, "bmi"], 
     data[data$charges<32000 & data$charges>15000, "charges"])
                                                               


cor.test(data$bmi, data$charges)

library(ggplot2)

data$overweight <- ifelse(data$bmi > 30, "Over30", "Under30")

summary(data$overweight)

addmargins(table(data$overweight))

addmargins(table(data$children, data$smoker))


addmargins(table(data$children, data$smoker, data$region))

data$exp <- ifelse(data$charges > 15000, "High_Charge", "Low_Charge")

table(data$exp)

summary(data$exp)

prop.table(data$exp)


ggplot() + geom_bar(data = data, mapping = aes(
  y=charges, x=overweight),
  stat = "summary") +
  facet_wrap(~exp)


ggplot() + geom_bar(data = data, mapping = aes(
  y=charges, x=overweight),
  stat = "summary") +
  facet_wrap(~overweight)


ggplot() + geom_bar(data = data, mapping = aes(
  y=charges, x=overweight, fill = exp),
  stat = "summary")


hist(log(data$charges))


#distributions
#smoker/age and charges
#scatterplot for continous
#boxplot, continuoes and categorical
#histogram for one variable
#density + histogram