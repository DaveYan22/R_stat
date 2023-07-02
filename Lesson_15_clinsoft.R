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

setwd("C:/Users/user/Desktop/ClinSoft")

# task 3
df <- readRDS("full_df.RDS")
view(df)


summary(df)
summary(df$age)
summary(df$BMI)
summary(df$height)


model1 <- glm(AE ~ BMI+age+gender+first_comer, data=df, family = 'binomial')
summary(model1)

roccurve <- roc(df$AE, df$age)
roccurve
plot(roccurve)

table(df$method)
df %>%
  select(method, AE) %>%
  filter(method == 0 & AE == 1) 

sum(df$AE==1 & df$method==0)
table(df$AE, df$method, df$AE/df$method)

df %>%
  group_by(method) %>%
  summarise(count=n(), sum_AE =sum(AE),
            prop_AE =sum_AE/count)

p_test <- function(p1, p2, n1, n2, z_lim=1.96){
  p_hat <- (n1*p1 + n2*p2)/(n1+n2)
  z <- (p1-p2)/sqrt(abs(p_hat*(1-p_hat)*(1/n1+1/n2)))
  res <- z<z_lim
  return(res)
}

p_test(p1 = 0.00151, p2 = 0.00151, n1 = 203364, n2 = 202194, z_lim = 1.96)


sample_sizes <- seq(18000,28000,2000)

res_mat <- matrix(nrow = 500, ncol = 6)
res_mat
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
tapply(df$height, df$gender, var)
a <- tapply(df$weight, df$age, var)
plot(a)

anova_model1 <- aov(df$BMI~ df$AE)
summary(anova_model1)

#task1

set.seed(1980)
patients <- data.frame()
Baseline <- rnorm(5, 4, 1)

patients <- rbind(patients, Baseline)
view(patients)
for (i in 2:5){
  patients <- rbind(patients, rnorm(5, 3.5, 0.5))
}
colnames(patients) <- c("Patient1", "Patient2", "Patient3", "Patient4", "Patient5")
rownames(patients) <- c("Baseline", "Day 3", "Day 10", "Day 17", "Day 27")



plot(0,0, type = "n", xaxt="n", yaxt="n",
     col = rgb(255, 0, 0, maxColorValue = 255),
     ylim = c(min(patients)-0.2, max(patients)+0.2),
     xlim = c(1, 5),
     ylab = "Albumin Level (g/dl)",
     xlab = "",
     main = "Change in Albumin Levels for 5 Patients")
linecol <- c()
for (i in 1:5){
  linecol[i] <- rgb(255, 0, 0,
                    maxColorValue = 255,
                    alpha = 300-i*50)
  lines(patients[i],
        lwd = 2,
        col = linecol[i])
}
axis(side = 1,
     labels = c("Baseline", "Day 3", "Day 10", "Day 17", "Day 27"),
     at = c(1, 2, 3, 4, 5))
axis(side = 2,
     at = c(seq(0, max(patients)+1, 0.25)),
     las = 1)
legend("top", inset = 0.02, legend = c("Patient 1","Patient 2",
                                       "Patient 3",
                                       "Patient 4","Patient 5"),
       col = linecol, text.font = 1, lty = 1,
       horiz = T, cex = 0.5, box.lty = 0)


patient_stat <- data.frame(mean = colMeans(patients))
for (i in 1:5){
  patient_stat[i,"sd"] <- sd(patients[,i])
}
center <- barplot(patient_stat$mean, plot = F)
barplot(patient_stat$mean, col = "indianred2",
        ylim = c(0, max(patient_stat$mean + patient_stat$sd)+0.5),
        yaxt = "n",
        ylab = "Albumin Level (g/dl)",
        border = NA,
        main = "Mean Albumin Level with 1 Standard Deviation Error")
arrows(x0 = center,
       y0 = patient_stat$mean - patient_stat$sd,
       x1 = center,
       y1 = patient_stat$mean + patient_stat$sd,
       code = 3, angle = 90, length = 0.2, lwd = 2)
axis(side = 1, labels = c("Patient 1", "Patient 2", "Patient 3", "Patient 4", "Patient 5"),
     at = center)
axis(side = 2, at = c(seq(0, max(patient_stat$mean + patient_stat$sd)+1, 0.5)), las = 1)


#task2
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
  ylab("Effect")+
  guides(color= FALSE)  

ggplot(vinclozolin, aes(x=conc, y=1/effect, color=exper))+
  geom_point()
# this graph looks y = x^(1/2)

ggplot(vinclozolin, aes(x=exper, y=effect, fill=exper))+
  geom_boxplot()

# this graph shows that we have a 1/x, x being the conc
table(vinclozolin$exper)

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
# when interpreting model try changing x in linear

boxplot(vinclozolin$effect)

vinclozolin %>%
  filter(effect > 2000) %>%
  view()

vinclozolin2 <- vinclozolin %>%
  filter(exper == 10821)

vinclozolin$exper
plot(vinclozolin2$conc^(-1.5), vinclozolin2$effect)


model3 <- lm(effect ~ exp(conc)^2 + exper, vinclozolin)
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

par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
dev.off()
plot(model5, 5)
# It is homoscedastic since the plots are equally 
# distributed near the read line, so the variances are
# constant and the errors does not vary much


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
