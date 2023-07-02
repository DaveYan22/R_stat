data <- read.csv("expenses.csv")

head(data)

summary(data)

boxplot(data$bmi)

boxplot(data$charges)

plot(data$bmi, data$charges)

plot(data[data$charges>15000, "bmi"], data[data$charges>15000, "charges"])

plot(data[data$charges<15000, "bmi"], data[data$charges<15000, "charges"])

plot(data[data$charges>32000, "bmi"], data[data$charges>32000, "charges"])

plot(data[data$charges<32000 & data$charges>15000, "bmi"], 
     data[data$charges<32000 & data$charges>15000, "charges"])

cor.test(data$bmi, data$charges)

library(ggplot2)

data$overweight <- ifelse(data$bmi > 30 , "Over30" , "Under30")

summary(data$overweight)

addmargins(table(data$children, data$smoker, data$region))

data$exp <- ifelse(data$charges > 15000, "High_Charge", "Low_Charge")

summary(data$exp)

ggplot()+geom_bar(data = data, 
                  mapping = aes(y = charges, x=overweight),
                  stat = "summary")+facet_wrap(~exp)

ggplot()+geom_bar(data = data, 
                  mapping = aes(y = charges, x=exp),
                  stat = "summary")+facet_wrap(~overweight)



ggplot()+geom_bar(data = data, 
                  mapping = aes(y = charges, x=overweight, fill = exp),
                  stat = "summary")

boxplot(data$smoker, data$charges)

ggplot()+geom_boxplot(data = data, mapping = aes(x = smoker, y = charges))


