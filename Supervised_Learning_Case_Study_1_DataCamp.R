#Start of the Script

# Predicting Fuel Efficiency From a US Department of Energy Data Set for real cars of 2018;
# Note : Data Taken from Datacamp.com from a Supervised Learning Course and 
# Case Study performed under the guidance of the Tutor Julia Silge Data Scientist at Stack Overflow.

#Loading the Data and Required Packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(RCurl)
library(caret)
library(randomForest)
library(yardstick)
library(caretEnsemble)


url_data <- "https://assets.datacamp.com/production/course_6013/datasets/cars2018.csv"

x <- getURL(url_data)

cars2018 <- read.csv(textConnection(x))

#Exploring the Data
head(cars2018)

str(cars2018)

colnames(cars2018)

# Plotting the histogram for Fuel Efficiency (MPG)

ggplot(cars2018, aes(x = MPG)) +
  geom_histogram(bins = 25) +
  labs(y = "Number of cars",
       x = "Fuel efficiency (mpg)")

#Comment : The Distribution of Fuel Efficiency is not normal.

###Building a simple linear model

colnames(cars2018)

#Deselecting the Columns Model and Model.Index,
#as there are of no use for modeling MPG

cars_vars <- cars2018 %>%
                  select(-Model, -Model.Index)

fit_all <- lm(MPG ~ .,cars_vars) #OR
#fit_all_train <- train(MPG ~ .,cars_vars,method="lm")
summary(fit_all)

###Training and Testing Data

#Dividing the cars data into different types of transmission
set.seed(1234)
in_train <- createDataPartition(cars_vars$Transmission, p = 0.80, list = FALSE)
training <- cars_vars[in_train, ]
nrow(training)
testing <- cars_vars[-in_train, ]
nrow(testing)

###Training models with caret

# Training a linear regression model
fit_lm <- train(log(MPG) ~ ., method = "lm", data = training,
                trControl = trainControl(method = "none"))

# Printing the linear model object
fit_lm

# Training a random_forest model
fit_rf <- train(log(MPG) ~ ., method = "rf", data = training,
                trControl = trainControl(method = "none"))

#Here in both cases trainControl : method="none" ; turns of resampling of data

# Printing the random_forest model object
fit_rf

###Evaulating the Model For Training Data (Linear Model and Random Forest)

# Creating the new columns

results <- training %>%
  mutate(`Linear regression` = predict(fit_lm, training),
         `Random forest` = predict(fit_rf, training))

# # Evaluating the performance in case of Linear Regression
metrics(results, truth = MPG, estimate = `Linear regression`)
# Evaluating the performance in case of Random Forest
metrics(results, truth = MPG, estimate = `Random forest`)


# Here in case of training data from R^2 value (near 1 for rf), 
# we can see that Random Forest is more accurate than linear regression model.

###Evaulating the Model For Testing Data (Linear Model and Random Forest)

# Creating the new columns

results_test <- testing %>%
  mutate(`Linear regression` = predict(fit_lm, testing),
         `Random forest` = predict(fit_rf, testing))

# # Evaluating the performance in case of Linear Regression
metrics(results_test, truth = MPG, estimate = `Linear regression`)
# Evaluating the performance in case of Random Forest
metrics(results_test, truth = MPG, estimate = `Random forest`)

###Training the model wit Resampling

# Fitting the models with cross validation resampling
 
 cars_lm_bt <- train(log(MPG) ~ ., method = "lm", data = training,
                     trControl = trainControl(method = "cv",number = 5,
                                              verboseIter = TRUE))
 
 cars_rf_bt <- train(log(MPG) ~ ., method = "rf", data = training,
                     trControl = trainControl(method = "cv",number = 5,
                                              verboseIter = TRUE))
#summary(cars_lm_bt)

#Visualizing the Models
results %>%
  gather(Method, Result, `Linear regression`:`Random forest`) %>%
  ggplot(aes(log(MPG), Result, color = Method)) +
  geom_point(size = 1.5, alpha = 0.5) +
  facet_wrap(~Method) +
  geom_abline(lty = 2, color = "gray50") +
  geom_smooth(method = "lm")

# Conclusion : We can Predict Fuel Effieincey more 
# accurately with Random Forest Model