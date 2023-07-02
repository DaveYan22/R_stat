

library(tidyverse)
library(car)
library(caret)

getwd()
setwd("C:/Users/user/Desktop/ChlinSoft")

df <- read.csv("healthcare-dataset-stroke-data.csv")
view(df)

#BMI is less than 18.5 -underweight
#BMI is 18.5 to 24.9, it falls within the normal or Healthy Weight 
#BMI is 25.0 to 29.9, it falls within the overweight range. 
#BMI is 30.0 or higher, it falls within the obese range.


df2 <- df %>%
  drop_na(bmi) %>%  # does not work
  mutate(bmi_normal = ifelse(bmi == "N/A", NA, 
                              ifelse(bmi>25, "overweight",
                              ifelse(bmi > 30, "obese",
                              ifelse(bmi>18.5, "healthy_weight", "underweight")))))

view(df2)
rm(df2)


# this doesnt work
df2 <- df2 %>%
  filter(!complete.cases(.))

df2 <- df2 %>% mutate_all(na_if,"")
# doesn't work till here
#maybe we have charachter value n/a instead of NA

df2 <- df2 %>%
  mutate(ever_married = recode(ever_married,"yes" = "1",
                         "no" = "0" )) 



df2$bmi[df2$bmi == "N/A"] <- NA
  
df2 <- df2 %>%
  filter(complete.cases('N/A'))

df2 <- df2 %>%
  mutate(smoking_status = recode(smoking_status, 'N/A' = NA))


view(df2)                       

#avg_glucose level <= 99 is normal
#100 - 125 prediabet
# 126 and higher diabet


df2 <- df2 %>%
  mutate(glucose_normal = ifelse(avg_glucose_level > 126, 'diabet',
                                 ifelse(avg_glucose_level<=99,'normal','prediabt')))

ggplot(df, aes(x=smoking_status, fill= gender)) +
  geom_bar()+
  facet_wrap(~gender)

table(df$gender, df$smoking_status)

ggplot(df, aes(x=as.factor(heart_disease), fill= gender)) +
  geom_bar()

ggplot(df, aes(x=as.factor(stroke), fill=Residence_type)) +  #how to add levels 0 and 1
  geom_bar()+
  facet_wrap(~Residence_type)

# for percent : 1- stat, identity, 2 geom_col, 3 position 'dodge' 
df %>%
  filter(gender != "Other") %>%
  ggplot(aes(x=ever_married, fill=gender))+
  geom_bar()

ggplot(df, aes(x=as.factor(stroke), y=avg_glucose_level)) +  #how to add group = ...
  geom_boxplot()

ggplot(df, aes(x=work_type, y=age, color=gender))+
  geom_boxplot()


ggplot(df, aes(x=smoking_status, y=age, color=gender))+
  geom_boxplot()


model1 <- glm(stroke ~ smoking_status + Residence_type + age+
                ever_married+heart_disease+gender, family='binomial', df)
summary(model1)

model11 <- glm(heart_disease~ Residence_type+ stroke+smoking_status+
                 work_type+bmi, family = 'binomial', df)
str(df)

summary(model11)

model2 <- lm(age ~  Residence_type+work_type + avg_glucose_level+
               heart_disease+gender+smoking_status+ bmi + stroke, df)

summary(model2)


plot(residuals.glm(model1))

residualPlot(model1)
residualPlots(model1)

par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
dev.off()
plot(model1, 1)


summary(model1)


view(df2)

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




set.seed(1)
test <- sample(seq(1,nrow(df4)),0.3*nrow(df4))
test_data <- df4[test,]
train_data <- df4[-test,]

model4_tt <- lm(log(price) ~ mileage +
                  engineSize + age+
                  transmission + fuelType +
                  manufacturer, train_data)
summary(model4_tt)

price_pred <- predict(model4_tt, newdata = test_data)

#head(price_pred)

plot(log(test_data$price), price_pred)
abline(a=0,b=1)

ssr <- sum((log(test_data$price)-price_pred)^2)
sst <- sum((log(test_data$price)-mean(log(test_data$price)))^2)
r_sq <- 1-ssr/sst
r_sq

library(caret)



model_caret <- train(log(price) ~ mileage +
                       engineSize + age+
                       transmission + fuelType +
                       manufacturer,  
                     data = train_data,                        
                     trControl = trainControl(method = "cv", number = 5),              
                     method = "lm",             
                     na.action = na.pass) 

model_caret$results
model_caret