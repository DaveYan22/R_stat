# new data - use BMW Mercedes Toyota Hyundai
# assign manufacturer and manufacturing_country variables
# plots to examine relationship, do transformations, develop model, assess model performance, residuals
# write down hypothesis, answer those hypothesis
# try to write a function with gg plot to create residual plot



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




ggplot(bmw, aes(x=year, y=price)) +
  geom_point()
ggplot(bmw, aes(x=transmission, y=price)) +
  geom_point()
ggplot(bmw, aes(x=mileage, y=log(price))) +
  geom_point()
ggplot(bmw, aes(x=fuelType, y=price)) +
  geom_point()
ggplot(bmw, aes(x=tax, y=price)) +
  geom_point()
ggplot(bmw, aes(x=mpg, y=price)) +
  geom_point()
ggplot(bmw, aes(x=engineSize, y=price)) +
  geom_point()

# year and price 
# mileage and price 

model1 <- lm(log(price) ~ engineSize + mpg + tax +
               fuelType + mileage + transmission + year, data=bmw)




model1 <- lm(price~mileage, data=bmw)
summary(model1)
plot(model1$residuals)
hist(model1$residuals)
# right skewed
model11 <- lm(log(price)~mileage, data=bmw)
summary(model11)
plot(model11$residuals)
hist(model11$residuals)
plot(log(bmw$price), model11$residuals)
# after log of y, it looks like normal distribution

# we can do shapiro test to see how it differs

shapiro.test(log(bmw$price)) 


residualPlot(model1)
densityPlot(model1$residuals)


cor.test(bmw$mileage,bmw$price)

par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
plot(model1,2)

plot1 <- function(data, x, y){
  ggplot(data, aes(x, y)) +
    geom_point()
}

plot1(bmw, bmw$mileage, bmw$price)
plot1(bmw, bmw$year, log(bmw$price))


plot2 <- function(data, x){
  ggplot(data, aes(x)) +
    geom_bar()
}
plot2(bmw, bmw$transmission)
plot2(bmw, bmw$fuelType)

plot3 <- function(data, x,y){
  ggplot(data, aes(x, y, colour=transmission))+
    geom_boxplot()
}

plot3(bmw, bmw$fuelType, bmw$price)
plot3(bmw, bmw$engineSize, bmw$price)



# joining the data

colnames(hyundi)[7] <- "tax" # the column name wasn't the same

all <- rbind(bmw, merc, toyota, hyundi)
View(all)
str(all)
summary(all)

all[17172, 'model']

#t test

# one sided t-test
all %>%
  filter(model == " C Class") %>%
  select(price) %>%
  t.test(mu = 50)


# two sided t-test
all %>%
  filter(model %in% c(" 3 Series", " C Class")) %>%
  t.test(price ~ model, data = .,
         alternative = "two.sided")

# one sided test for different means
all %>%
  filter(model %in% c(" 3 Series", " C Class")) %>%
  t.test(price ~ model, data = .,
         alternative = "less",
         conf.level = 0.95)

all %>%
  filter(year %in% c(2019, 2020) &
           model == " C Class") %>%
  mutate(year = factor(year, levels = c(2019, 2020))) %>%
  t.test(price ~ year, data = .)

#chi-squared test

all$transmission %>%
  table() %>%
  chisq.test()

all$fuelType %>%
  table() %>%
  chisq.test()

plot(bmw$price, bmw$mileage)

# make the qq plot and r squared better
bmw2 <- bmw %>%
  mutate(bmw_outlier = bmw$price < 80000 & bmw$mpg < 200 
         & bmw$engineSize>0 & bmw$mileage < 150000)

view(bmw2)

model_bmw <- lm(log(price) ~ mpg+engineSize + mileage, bmw2)
summary(model_bmw)
plot(model_bmw$residuals)
hist(model_bmw$residuals)

leveneTest(log(price) ~ factor(fuelType), data=bmw2)


par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
plot(model_bmw,2)
residualPlot(model_bmw)
densityPlot(model_bmw$residuals)
dev.off()

# statistical tests
# statistical tests
# statistical tests
# statistical tests
hist(bmw$price)

shapiro.test(bmw$price)
shapiro.test(log(bmw$price))
   # if p is smaller that 5% than significant
   # differences between normal distribution and your data

anscombe.test(bmw$price)
   # if kurtos bigger than 3 left skeyed

agostino.test(bmw$price)
   # positive or negative skey

table(bmw$model, bmw$year)


chisq.test(table(bmw$model, bmw$year))
# if p is high that the difference is small
# if p is less than 0.05 thanstatistically significant
ggplot(bmw, aes(year, price))+
  geom_bar(stat = 'identity')+
  facet_grid(~year)

ggplot(bmw, aes(mileage, price)) +
  geom_violin()

leveneTest(price ~ factor(transmission), data=bmw)
# test for homogenecity of variances

t1 <- oneway.test(price ~ transmission, data=bmw, var.equal = T)
t1

t2 <- aov(price~transmission, bmw)
t2

summary(t2)

TukeyHSD(t2)

# more than 1 tests
# pairwise difference


kruskal.test(price~mileage, bmw)
#more than 2 means

install.packages('FSA')
library(FSA)

dunnTest(price~mileage, data=bmw)

durbinWatsonTest(model1)
#check for linear relationship

bmw$manufacturer <- "BMw"
bmw$country <- "Germany"
merc$manufacturer <- "Mercedes"
merc$country <- "Germany"
hyundi$manufacturer <- "Hyundi"
hyundi$country <- "Korea"
toyota$manufacturer <- "toyota"
toyota$country <- "Japan"



df <- rbind(bmw, merc, hyundi, toyota)

view(df)

df$age <- abs(df$year - 2020)

ggplot() + 
  geom_point(data=df, mapping=aes(y=log(price), x=age))


ggplot() + 
  geom_point(data=df, mapping=aes(y=1/(price+1), x=mileage))


