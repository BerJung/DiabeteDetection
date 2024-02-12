#Logistic regression with pima diabetes dataset using glm

#Preparations
#Install and load necessary packages
install.packages("Epi")
install.packages("MLmetrics")
install.packages("kableExtra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")

library(dplyr)
library(ggplot2)
library(Epi)
library(MLmetrics)
library(readr)
library(kableExtra)

#Set working directory
setwd("C:/Users/Nathan Jung/Desktop/archive")

#Data
#Load dataset and change the format into data frame 
diabetes <- read.csv("C:/Users/Nathan Jung/Desktop/archive/diabetes.csv")
diabetes2 <- as.data.frame(diabetes)
str(diabetes2) #Check each variable's data type

#Logistic Regression Modeling
#Data Standardization
#Standardize all the variable data except "Outcome" variable to interpret parameters on the same scale
diabetes3 <- scale(diabetes2[-9])
diabetes_s <- data.frame(diabetes2$Outcome, diabetes3)
names(diabetes_s)[1] <- "Outcome"
str(diabetes_s)

#Separate the standardized dataset into training and test data(7 : 3)
set.seed(100)
index <- sample(1:nrow(diabetes_s), size = nrow(diabetes_s)*0.7)
training <- diabetes_s[index, ]
test <- diabetes_s[-index, ]

#Building a model: use glm()
#Estimate a logistic regression model has "Outcome" as a dependent variable
pima <- glm(formula(training), training, family = "binomial")
summary(pima) #The result shows that "SkinThickness" and "Insulin" seem non-significant variable

#Fitting the regression model that best explains the data:
#Use backward stepwise method
pima1 <- step(pima, direction = "backward") #As following the result, the "Insulin" variable must be contained in the model because it increases the value of AIC when it's excluded
summary(pima1)

#Prediction
#Use predict()
#Extract the top 10 people with a high probability of developing diabetes by applying a fitted regression model to the test dataset
test_pred1 <- predict(pima1, newdata = test, type = "response")
head(sort(test_pred1, decreasing = TRUE), n=10) %>% kbl() %>% kable_paper(full_width = F)

#Compare the probability and the "Outcome"
head(arrange(data.frame(test_pred1, test$Outcome), desc(test_pred1)), n = 10) %>% kbl() %>% kable_paper(full_width = F)
