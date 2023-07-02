library(lattice)
library(tidyverse)
library(car)
library(caret)

library(rsample)
library(ROCR)
library(pROC)
library(randomForest)
library(ResourceSelection)

getwd()
setwd("C:/Users/user/Desktop/ChlinSoft")

df <- read.csv("healthcare-dataset-stroke-data.csv")
view(df)

rm(df2)

df2 <- df %>%
  mutate(bmi_normal = ifelse(bmi == "N/A", NA, 
                             ifelse(bmi>25, "overweight",
                                    ifelse(bmi > 30, "obese",
                                           ifelse(bmi>18.5, "healthy_weight", "underweight")))))

df2 <- df2 %>%
  mutate(glucose_normal = ifelse(avg_glucose_level > 126, 'diabet',
                                 ifelse(avg_glucose_level<=99,'normal','prediabt')))

df2<- df2 %>%
  mutate(stroke = ifelse(stroke == 1, "YES", "NO"))

df2<- df2 %>%
  mutate(hypertension = ifelse(hypertension == 1, "YES", "NO"))

df2<- df2 %>%
  mutate(heart_disease = ifelse(heart_disease == 1, "YES", "NO"))


df2<- df2 %>%
  mutate(ever_married = ifelse(ever_married == "Yes", "YES", "NO"))

df2$smoking_status[df2$smoking_status == "Unknown"] <- NA
df2$bmi[df2$bmi == "N/A"] <- NA


view(df2)




df2 %>%
  filter(!complete.cases(.)) %>%
  view()


view(df2)                       


df2 %>%
  filter(gender == "Other") %>%
  view()
# removing value equal to 'Other'

df2 <- df2 %>%
  subset(id != 56156)

# second option
df2 <- subset(df2,gender !='Other' )


df2$bmi <- as.double(df2$bmi)

view(df2)

str(df2)


# visuilization

ggplot(df2, aes(x=stroke, y=avg_glucose_level, fill= hypertension)) +
  geom_boxplot()+
  facet_wrap(~hypertension)


ggplot(df2, aes(x=stroke, fill=heart_disease)) +
  geom_bar(position = 'fill') 

ggplot(df2, aes(x=stroke, y=age)) + 
  geom_boxplot()

ggplot(df2, aes(y=stroke, x=age, fill=hypertension)) + 
  geom_boxplot()

ggplot(df2, aes(y=stroke, x=bmi, fill=hypertension)) + 
  stat_boxplot()

ggplot(df2, aes(x=bmi, fy=hypertension)) + 
  stat_boxplot()+
  facet_wrap(~stroke)
# add percentages here

model1 <- glm(stroke ~ smoking_status + Residence_type + age+
                ever_married+heart_disease+gender, family='binomial', df)
summary(model1)

model11 <- glm(heart_disease~ Residence_type+ stroke+smoking_status+
                 work_type+bmi, family = 'binomial', df2)
str(df)


# if < 0.05 than sigificant 
test1 <- hoslem.test(model1$y, fitted(model1), g=10)
test1



summary(model11)

model2 <- lm(age ~  Residence_type+work_type + avg_glucose_level+
               heart_disease+gender+smoking_status+ bmi + stroke, df2)

summary(model2)


table(df2$work_type)
table(df2$smoking_status)

plot(residuals.glm(model1))

residualPlot(model1)
residualPlots(model1)

par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
dev.off()
plot(model1, 1)


summary(model1)


view(df2)


addmargins(prop.table(table(df2$heart_disease, df2$smoking_status)))  

addmargins(round(prop.table(table(df2$stroke, df2$gender),margin = 2)*100,2))


# train and test

my_data <- df2
sample_n(my_data, 4)
set.seed(100)

df3 <- df2 %>%
  filter(complete.cases(.))



training.samples <- df3$bmi %>%
  createDataPartition(p = 0.8, list= FALSE)

train.data <- df3[training.samples,]

test.data <- df3[-training.samples,]

model_train_test <- lm(bmi ~ ., data=train.data)

predictions <- model_train_test %>%
  predict(test.data)
# same thing

predict(model_train_test, test.data)
predictions


compare <- data.frame(actual = test.data,
                      predicted = predictions)

compare

error <- RMSE(predictions, test.data)
error




# logistic - model1

#ROC curve


set.seed(420)

num.samples <- 100

weight <- sort(rnorm(n=num.samples, mean=172, sd=29))

obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/100)),
                yes=1, no=0) 
  
obese 

plot(x=weight, y= obese)

glm.fit = glm(obese~weight, family = 'binomial')
lines(weight, glm.fit$fitted.values, plot=TRUE)

roc(obese, glm.fit$fitted.values, plot=TRUE)


par(pty = "s")
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=T, percent=T,
    xlab='False Positive Percentage', ylab='True Positive Percentage')

roc.info <- roc(obese, glm.fit$fitted.values, legacy.axes=TRUE)

roc.df <- data.frame(tpp=roc.info$sensitivities*100,
                     fpp=(1-roc.info$specificities)*100,
                     thresholds=roc.info$thresholds)

head(roc.df)

tail(roc.df)

roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=T, percent=T,
    xlab='False Positive Percentage', ylab='True Positive Percentage',
    col="#377eb8", lwd=4, print.auc=T)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=T, percent=T,
    xlab='False Positive Percentage', ylab='True Positive Percentage',
    col="#377eb8", lwd=4, print.auc=T, print.auc.x=45, partial.auc=c(100, 90),
    auc.polygon=T, auc.polygon.col="#377eb822")

rf.model <- randomForest(factor(obese)~weight)


roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=T, percent=T,
    xlab='False Positive Percentage', ylab='True Positive Percentage',
    col="#377eb8", lwd=4, print.auc=T)

plot.roc(obese, rf.model$votes[,1], percent=T, col="#4daf4a",lwd=4,
         print.auc=T, add=T, print.auc.y=40)

legend("bottomright", legend = c("Logistic Regression", "Random Forest"),
       col=c("#377eb8", "#4daf4a"),lwd = 4)
par(pty="m")


set.seed(100)
n < 100


#Arman
str(df)

df$bmi <- as.double(df$bmi)
df$stroke <- ifelse(df$stroke == 0, "NO", "YES")
df$heart_disease <- ifelse(df$heart_disease == 0, "NO", "YES")
df$hypertension <- ifelse(df$hypertension == 0, "NO", "YES")
df$gender[df$gender == "Other"] <- NA
summary(df$age)

boxplot(y=df$age, x=df$stroke)

ggplot()+geom_boxplot(data = df, mapping = aes(x=stroke, y=age))
ggplot()+geom_boxplot(data = df, mapping = aes(x=stroke, y=bmi))

table(df$stroke)

addmargins(round(prop.table(table(df$stroke, df$gender),margin = 2)*100,2))

df <- na.omit(df)

model1 <- glm(stroke~bmi+age+hypertension+heart_disease, data = df, family = "binomial")

summary(model1)

library(ROCR)

pred <- predict(model1, newdata = df, 
                type = "response")
pred <- prediction(pred, df$stroke)

roc <- performance(pred, "tpr", "fpr")

plot(roc, colorize = T, lwd = 2)
abline(a=0,b=1)  

auc = performance(pred, measure = "auc")