library(tidyverse)
library(car)
library(moments)

eda <- read.csv("eda_data.csv")
view(eda)


eda2 <- eda %>%
  select(Job.Title, Salary.Estimate, Rating, Company.Name, Headquarters, Size, Founded, Type.of.ownership, Industry, Sector, Revenue, min_salary, max_salary, avg_salary, job_state, age, seniority)

view(eda2)
str(eda2)

colnames(eda2)[1] <- "job_title"
colnames(eda2)[2] <- "salary_range"
colnames(eda2)[3:5] <- c("rating", "company_name", "headquarter")
colnames(eda2)[6:11] <- c("size", "founded", "ownership_type", "industry", "sector", "revenue")
eda2 <- eda2 %>%
  mutate(company_age = abs(2022-founded))

eda2 <- eda2 %>%
  relocate(job_state, .after = headquarter)

eda2 <- eda2 %>%
  relocate(company_age, .after = founded)

eda2$avg_salary <- eda$avg_salary * 1000
eda2$min_salary <- eda2$min_salary * 1000
eda2$max_salary <- eda2$max_salary * 1000

view(eda2)

eda2 %>%
  filter(job_state %in% c("NY", "CA") & age < 62 & age >17) %>%
  ggplot(aes(x=age, y=avg_salary, color=job_state)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~job_state)

eda2 %>% 
  filter(job_title %in% c("Data Scientist", "Data Engineer")) %>%
  ggplot(aes(y=avg_salary, x=job_title)) +
  geom_boxplot()



model1 <- lm(log(avg_salary) ~ age 
             + revenue + ownership_type+company_age + industry, eda2)

summary(model1)   

model2 <- lm(log(avg_salary) ~ job_state + job_title
               + industry, eda2)

summary(model2)  

unique(eda2$job_title)

hist(model1$residuals)
plot(model1$residuals)
plot(model1$residuals, model1$fitted.values)
plot(model1$residuals, eda2$min_salary)

residualPlot(model1)
residualPlots(model1)

par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
plot(model1, 3)
plot(model1, 5)
dev.off()

eda3 <- eda2[-c(241, 104),]

model3 <- lm(log(avg_salary) ~  age 
             + revenue + ownership_type+company_age + industry, eda3)

summary(model3)   

model4 <- lm(log(avg_salary) ~ job_state + job_title
             + industry, eda3)

summary(model4)  


par(mfrow = c(1, 1))
par(mar = c(1,1,1,1))
plot(model3,3)


eda2 %>%
  ggplot(aes(x=ownership_type, y=max_salary)) +
  geom_boxplot()

table(eda2$ownership_type)
table(eda2$size)
table(eda2$sector)
table(eda2$founded)

ggplot(eda2, aes(y=size)) + 
  geom_bar()


age_grouping <- eda2 %>%
  select(age, job_title, job_state, seniority) %>%
  drop_na(seniority) %>%
  mutate(age_group = if_else(age >=50,
                              "old",
                              "young"))

# clean data from -1 value to NA


view(age_grouping)

# t.test

eda3 %>%
  filter(sector == "Information Technology") %>%
  select(age) %>%
  t.test(mu = 50)

eda3 %>%
  filter(sector %in% c("Information Technology", "Biotech & Pharmaceuticals")) %>%
  t.test(age ~ sector, data = .,
         alternative = "two.sided")


eda3 %>%
  filter(size %in% c("1001 to 5000 employees", "501 to 1000 employees")) %>%
  t.test(age ~ size, data = .,
         alternative = "less",
         conf.level = 0.95)

eda3 %>%
  filter(founded %in% c(2000, 2005) &
           job_state == "NY") %>%
  mutate(founded = factor(founded, levels = c(2000, 2005))) %>%
  t.test(age ~ founded, data = .,
         paired = TRUE)
