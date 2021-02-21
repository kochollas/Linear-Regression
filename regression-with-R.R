#Implementing linear regression in R
library(dplyr)
library(ggplot2)
#Read the dataset
setwd('~/Documents/ML/Linear-Regression')
dataset = read.csv('student_scores.csv')
#Dimension of dataset
dim(dataset)
#head of dataset
head(dataset)
#Summary of dataset
dataset%>%summary()

#Drawing a scatter plot using ggplot2 library
ggplot(data = dataset, aes(x=Hours, y=Scores))+geom_point()+ggtitle("Hours vs Scores")+labs(y="Scores(%)")

#more ggplot hacks
#IrisBox <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) + geom_boxplot()
#print(IrisBox+ labs(fill = "Iris species"))

# Create the response and predictor features

X = dataset[,1] #R is not zero indexed
y = dataset[,2]

# Train test split 80:20
set.seed(43)
train_index = sample(1:nrow(dataset),0.8*nrow(dataset))
train_index

data_train = dataset[train_index,]
data_test = dataset[-train_index,]

# Perform the regression
mod1 = lm(data = data_train, Scores~Hours)
summary(mod1)



# Testing on the training data for the RMSE
predicted = predict.lm(mod1, data_train)
predicted_data = cbind(data_train, predicted)
predicted_data
# Model Evaluation

#mean squared error for the training data
sse = sum((data_train$Scores-predicted_data$predicted)**2)
mse = sse / 18
rmse = sqrt(mse)

# R-SQUARED 
sst = sum((data_train$Scores-mean(data_train$Scores))**2)
ssr = sst - sse
rsquared = (sst - sse) /sst

Fstatistics = ssr/mse  #(mssr/msse) this will tell whether generally model is significant



