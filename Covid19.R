
#Set Working directory
setwd("J:/ML")

#Reading Covid dataset
covid19 <- read.csv("COVID19_line_list_data.csv", stringsAsFactors = F)

#loading all required libraries
library(dplyr)
library(ggplot2)
library(Boruta)
library(tidyverse)
library(stringr)

#checking the dimension of the dataset
dim(covid19)

#to view the top portion of dataset
head(covid19)

#cheking the structure of datset
str(covid19)

#checking various features of the column symptom
table(covid19$symptom)

#making new columns based on symptoms features
covid19$fever <- str_detect(covid19$symptom, 'fever|cough|headache|sore throat|sputum|malaise|coughing|muscle aches|high fever|physical discomfort')

covid19$gastric <- str_detect(covid19$symptom,'vomiting|diarrhea|pneumonia|abdominal pain')

#selecting the required variables in the dataset
covid19 <- covid19 %>% 
  select(reporting.date, country, gender, age, death, fever, gastric, symptom)

#converting death variable into binomial
covid19$death <- ifelse(covid19$death == '0', 0, 1)

#converting date format
covid19$reporting.date <- as.Date(covid19$reporting.date, "%m/%d/%Y")


#factorizing other categorical variables
covid19$country <- factor(covid19$country)

covid19$gender <- factor(covid19$gender)

covid19$death <- factor(covid19$death, label = c('no','yes'))

covid19$fever <- factor(covid19$fever, label = c('no','yes'))

covid19$gastric <- factor(covid19$gastric, label = c('no','yes'))


head(covid19)

str(covid19)

summary(covid19)

#removing missing values by mean and Not Disclosed
mean_age <- mean(covid19$age, na.rm=T)

covid19$age[which(is.na(covid19$age))] <- mean_age

covid19$gender1 <- as.character(covid19$gender)
covid19$gender1[which(is.na(covid19$gender1))] <- 'Not Disclosed'
covid19$gender <- as.factor(covid19$gender1)
covid19 <- select(covid19, -gender1)

table(is.na(covid19))

tail(covid19)

which(is.na(covid19$reporting.date))
covid19[262,]
covid19 <- covid19[-262,]

covid19 <- select(covid19, -symptom)

#rounding the age value
covid19$age <- round(covid19$age, 0)


#Plotting graph for death based on gender
ggplot(covid19, aes(death, fill = gender))+
  geom_bar(position='fill', show.legend=T)+
  theme_grey()+
  scale_fill_manual(values = c('red','blue','green'))

#plotting the boxplot graph based on age and death 
ggplot(covid19, aes(death, age, fill = death))+
  geom_boxplot()+
  theme_grey()+
  scale_fill_manual(values = c('red','blue', 'green'))


#plotting graph based on death and fever symptom
ggplot(covid19, aes(death, fill = fever))+
  geom_bar(position='fill', show.legend=T)+
  theme_grey()+
  scale_fill_manual(values = c('red','blue'))


#age density plot, freq=false to convert into relative percentage
hist(covid19$age, freq=F, ann=F)
title(main='Density plot of Age', xlab='Age', ylab='Density')
lines(density(covid19$age), lwd=5, col='red')

#feature selection to increase accuracy
library(Boruta)

set.seed(1234)

boruta <- Boruta(death~., data=covid19, doTrace=2, maxRuns=200)

plot(boruta, las=2)

str(covid19)

library(caret)

#cross validation for various models
control <- trainControl(method='cv', number=10)
metric <- "Accuracy"

#splitting dataset into training and test data
library(caTools)
set.seed(1234)

split <- sample.split(covid19$death, SplitRatio = 0.75)

train_split <- subset(covid19, split==T)

test_split <- subset(covid19, split==F)

#various models
#Linear Discriminant Analysis
set.seed(1234)

fit.lda <- train(death~gender+age, data=covid19, method='lda', metric=metric, trcontrol=control)

#Support Vector Machine
set.seed(1234)

fit.svm <- train(death~gender+age, data=covid19, method='svmRadial', metric=metric, trcontrol=control)

#random forest
set.seed(1234)

fit.rf <- train(death~gender+age, data=covid19, method='rf', metric=metric, trcontrol=control)

#listing all the results to check accuracy
results <- resamples(list(lda=fit.lda, svm=fit.svm, rf=fit.rf))

summary(results)

dotplot(results)

#random forest model having highest accuracy 
print(fit.rf)

predictions <- predict(fit.rf, test_split[3:5])

#Confusion Matrix
confusionMatrix(predictions, test_split$death)

