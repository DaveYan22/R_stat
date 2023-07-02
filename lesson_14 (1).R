library(lattice)
library(tidyverse)
library(car)
library(caret)

library(rsample)
library(ROCR)
library(pROC)
library(randomForest)
library(ResourceSelection)
library(pwr)

getwd()
setwd("C:/Users/user/Desktop/ClinSoft")

# task 3
df <- readRDS("full_df.RDS")
view(df)


summary(df)
summary(df$age)
summary(df$BMI)
summary(df$height)
summary(df$weight_kg)

view(df)

df %>%
  mutate(first_comer = as.factor(first_comer)) %>%
  ggplot(aes(x=age, y=weight, color=first_comer))+
    geom_point()

model1 <- glm(AE ~ BMI+age+gender+first_comer, data=df, family = 'binomial')


AIC(glm(model1, family = binomial))

deviance(model1)
summary(model1)
residualPlots(model1)

roccurve <- roc(df$AE, df$age)
roccurve
plot(roccurve)


df %>% group_by(method) %>% summarise(count=n(), sum_AE = sum(AE), 
                                      prop_AE = sum_AE/count)


p_test <- function(p1, p2, n1, n2, z_lim=1.96){
  p_hat <- (n1*p1 + n2*p2)/(n1+n2)
  z <- (p1-p2)/sqrt(abs(p_hat*(1-p_hat)*(1/n1+1/n2)))
  res <- z<z_lim
  return(res)
}

p_test(p1 = 0.00151, p2 = 0.00151, n1 = 203364, n2 = 202194, z_lim = 1.96)


table(df$method, df$AE)


sample_sizes <- seq(18000,28000,2000)

res_mat <- matrix(nrow = 500, ncol = 6)
colnames(res_mat) <- sample_sizes


for(i in 1:500){
  for(j in 1:length(sample_sizes)){
    ss <- sample_sizes[j]
    smpl1 <- sample_n(df[df$method==0,], ss/2)
    smpl2 <- sample_n(df[df$method==1,], ss/2)
    ae_sum1 <- sum(smpl1$AE)
    ae_sum2 <- sum(smpl2$AE)
    p1 <- ae_sum1/nrow(smpl1)
    p2 <- ae_sum2/nrow(smpl2)
    res_mat[i,j] <- p_test(p1,p2,nrow(smpl1), nrow(smpl2))
  }
}


res_mat2 <- matrix(nrow = 500, ncol = 6)
colnames(res_mat2) <- sample_sizes

for(i in 1:500){
  for(j in 1:length(sample_sizes)){
    ss <- sample_sizes[j]
    smpl <- sample_n(df, ss)
    ae_sum1 <- sum(smpl[smpl$method==0,"AE"])
    ae_sum2 <- sum(smpl[smpl$method==1,"AE"])
    n1 <- nrow(smpl[smpl$method==0,])
    n2 <- nrow(smpl[smpl$method==1,])
    p1 <- ae_sum1/n1
    p2 <- ae_sum2/n2
    res_mat2[i,j] <- p_test(p1,p2,n1,n2)
  }
}

plot(sample_sizes, colMeans(res_mat), type="l")

colMeans(res_mat2)




library(pwr)

df[sample(nrow(df), 2000), ]

seq(18000, 28000, 2000)

pwr.t.test(n=405558, d=0.5, sig.level= 0.025, alternative="greater",
           power=NULL)

t.test(df$age, conf.level = 0.025)

t.test(df$weight, df$height)

wilcox.test(df$BMI, df$age)


cor.test(df$height, df$weight)


t.test(df$weight ~ df$gender)$p.value
t.test(df$weight~ df$AE)$p.value
tapply(df$weight, df$gender, var)
tapply(df$weight, df$age, var)

anova_model1 <- aov(df$BMI~ df$AE)
summary(anova_model1)

AIC(anova_model1)
# task 2
library('drc')
view(vinclozolin)

summary(vinclozolin)
summary(vinclozolin$exper)
summary(vinclozolin$conc)
summary(vinclozolin$effect)

ggplot(vinclozolin, aes(x=conc, y=effect, color=exper))+
  geom_point()

ggplot(vinclozolin, aes(x=conc, y=effect, color=exper))+
  geom_point()+
  facet_wrap(~exper)+
  xlab("Concentration")+
  ylab("Effect")

ggplot(vinclozolin, aes(x=conc, y=1/effect, color=exper))+
  geom_point()


ggplot(vinclozolin, aes(x=exper, y=effect, fill=exper))+
  geom_boxplot()

# this graph shows that we have a 1/x, x being the conc

model1 <- lm(log(effect)^0.4  ~ exper + conc, vinclozolin)
summary(model1)
# why log

model2 <- lm(1/effect^0.3 ~ exper + conc, vinclozolin)
summary(model2)

hist(log(model1$residuals))
plot(model1$fitted.values, model1$residuals)

residualPlot(model1)
residualPlots(model1)

par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
dev.off()
plot(model1, 4)

?pwr.r.test
?pwr.t.test
?pwr.anova.test


p1 <- pwr.r.test(r=0.5, sig.level = 0.05, power = 0.8,
           alternative = "greater")
p1
plot(p1)

p2 <- pwr.t.test(d=0.2, sig.level = 0.05, type='one.sample',
                 power=0.8, alternative = "greater")
p2

p3 <- pwr.anova.test(k=4,
                     f=0.15,
                     sig.level = 0.05,
                     power =0.8)
p3
plot(p3)

p4 <- pwr.t.test(n=40,d=0.5, sig.level = 0.05,
alternative = "greater")

library(lsr)

cohensD(vinclozolin$conc)
p5 <- pwr.t.test(n=52, d=0.7, sig.level = 0.05,
                 power=NULL, type='one.sample')
p5
plot(p5)

wilcox.test(vinclozolin$effect, vinclozolin$conc)



# remove outliers

boxplot(vinclozolin$effect)

vinclozolin %>%
  filter(effect > 2000) %>%
  view()

vinclozolin2 <- vinclozolin %>%
  filter(effect < 2200)


model3 <- lm(log(effect)^0.4  ~ exper + conc, vinclozolin2)
summary(model3)
# why log

model4 <- lm(1/effect^0.3 ~ exper + conc, vinclozolin2)
summary(model4)

# by removing outliers the model gets worse with R squared


hist(log(model3$residuals))
plot(model3$fitted.values, model1$residuals)

residualPlot(model3)
residualPlots(model3)

# do the same for conc < 2

vinclozolin3 <- vinclozolin2 %>%
  filter(conc < 3 & conc  > 0) 

model5 <-lm(log(effect)^0.5  ~ exper + conc, vinclozolin3)

summary(model5)
residualPlots(model5)
plot(model5$residuals, model5$fitted.values)
# conc with bigger 3 is not significant to our data

model6 <-lm(1/effect ~ exper + conc, vinclozolin3)

summary(model6)


#model 5 is the best
# test train ridge regression
training.samples <- vinclozolin %>%
  createDataPartition(p = 0.8, list= FALSE)

train.data <- vinclozolin[training.samples,]

test.data <- vinclozolin[-training.samples,]

model_train_test <- lm(conc ~ effect, data=train.data)

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

# task 1


baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient_1 <- c(baseline, four_days)
patient_1 <- as.data.frame(patient_1)
patient_1 <- t(patient_1)

baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient_2 <- c(baseline, four_days)
patient_2 <- as.data.frame(patient_2)
patient_2 <- t(patient_2)

baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient_3 <- c(baseline, four_days)
patient_3 <- as.data.frame(patient_3)
patient_3 <- t(patient_3)

baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient_4 <- c(baseline, four_days)
patient_4 <- as.data.frame(patient_4)
patient_4 <- t(patient_4)

baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient_5 <- c(baseline, four_days)
patient_5 <- as.data.frame(patient_5)
patient_5 <- t(patient_5)

data <- rbind(patient_1, patient_2, patient_3, patient_4, patient_5)
view(data)

colnames(data) <- c('baseline','Day3','Day10','Day17','Day27')

patient <- c('patient_1', 'patient_2','patient_3','patient_4','patient_5')
patient <- as.data.frame(patient)

data <- cbind(patient, data)

data$avg_four_days <- (data$Day3 +data$Day10+
  data$Day17+data$Day27) /4

ggplot(data, aes(x=baseline, y=avg_four_days, color=patient))+
  geom_point()
ggplot(data, aes(y=avg_four_days, x=baseline,fill=patient))+
  geom_boxplot()

ggplot(data, aes(x=patient,y=avg_four_days))+
  geom_point()

ggplot(data, aes(x=Day3, y=Day27, color=patient))+
  geom_point()

ggplot(data, aes(x=days, y=patient))+
  geom_bar(stat='identity')

view(data)

# days as rows, patient as columns
baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient1 <- c(baseline, four_days)
patient1 <- as.data.frame(patient1)

days <- c('Baseline','Day2','Day3','Day4','Day5')
days <- as.data.frame(days)
cbind(days, patient1)

baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient2 <- c(baseline, four_days)
patient2 <- as.data.frame(patient2)
cbind(days, patient1, patient2)

baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient3 <- c(baseline, four_days)
patient3 <- as.data.frame(patient3)

baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient4 <- c(baseline, four_days)
patient4 <- as.data.frame(patient4)

baseline <- rnorm(n=1, mean = 4, sd = 1)
four_days <- rnorm(n=4, mean=3.5, sd = 0.5)

patient5 <- c(baseline, four_days)
patient5 <- as.data.frame(patient5)

data <- cbind(days, patient1, patient2, patient3, patient4,patient5)
View(data)

ggplot(data, aes(x=days, y=patient1)) + 
  geom_point()

ggplot(data, aes(x=patient2, y=days))+
  geom_bar(stat="identity") 


