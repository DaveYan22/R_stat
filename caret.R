library(caret)
library(lattice)
library(tidyverse)


my_data <- mtcars


sample_n(my_data, 4)


set.seed(100)


training.samples <- mtcars$mpg %>%
  createDataPartition(p = .8, list= FALSE)

train.data <- my_data[training.samples,]

test.data <- my_data[-training.samples,]

model <- lm(mpg ~ ., data=train.data)

predictions <- model %>%
  predict(test.data)
# same thing
predict(model, test.data)
predictions


compare <- data.frame(actual = test.data$mpg,
                      predicted = predictions)

compare

error <- RMSE(predictions, test.data$mpg)
error



library(rsample)

set.seed(123)

car_split <- initial_split(data=mtcars,
                           prop = .8)

training_set <- training(car_split)
testing_set <- training(car_split)
rm(training_set)
rm(testing_set)
