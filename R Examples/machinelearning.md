---
title: "Predicting Activities Using Machine Learning"
author: "kat"
date: "December 10, 2017"
output: 
  html_document:
    keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)
```

### Executive Summary

#### The purpose of this project is to take personal activity data collected from devices such as Jawbone Up and Fitbit to determine which activity a person was performing at a given time. The data comes from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. See appendix for information about the data sets used.

#### My general plan of attack was to determine which variables needed to be removed either because of too many NA values or because they were not necessary for the analysis. Then I would use k-fold cross validation with 5 folds to compress the data. From there, I would try various models presented in class individually, then combine if necessary to determine the best model. 

#### In summary, I found the Random Forest method in the caret package with a k-fold cross validation of 5-folds yielded a model with 99% accuracy that was able to predict the prediction quiz answers with 100% accuracy.

```{r, echo=FALSE}

#clear workspace, read in data
 rm(list = ls())

ALLdata <- read.csv("./Coursera Final Project/pml-training.csv")
testingFinal <- read.csv("./Coursera Final Project/pml-testing.csv")


```

### Exploratory Analysis & Data Cleaning

#### The classe variable is the value to be predicted and has factor levels, A to E. I removed the first 7 columns because they did not seem relevant for the prediction of activity (see code).

#### I removed all columns that had more that 50% NA values as well as measurements that were summary statistics of the raw data (see code).

```{r}
# remove columns x, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window
ALLdata <- ALLdata[,8:160]

# remove columns with NA values > 50%
ALLdata <- ALLdata[, -which(colMeans(is.na(ALLdata)) > 0.50)]

ALLdata2 <- ALLdata

# leave out summary statistic variables in model
summaries <- c("kurtosis_", "skewness_", "max_", "min_", "amplitude_", "var_", "avg_", "stddev_")

ALLdata2 <- ALLdata2[, -grep(paste(summaries, collapse="|"), colnames(ALLdata2))]
```


### Splitting Data Into Testing & Training Sets for Cross Validation
#### I used the given "training" set to create a 75/25 split into a new training and testing set. I then used this new training set to train my model and tested on the testing set. I only used the given "testing" set to predict once I reached 99% accuracy on the 25% testing set.
```{r}
library(caret)
library(kernlab)
inTrain <- createDataPartition(y=ALLdata2$classe, p=0.75, list=FALSE)

training <- ALLdata2[inTrain,]
testing <- ALLdata2[-inTrain,]
```

### Model Attempt #1 - Method = "rpart" - Recursive Partioning And Regression Trees from caret package
#### This model yieled 49% accuracy which was not good enough. ***Code not run to save time.
```{r, eval=FALSE}
set.seed(25)
modFit <- train(classe ~., method="rpart", data=training)
print(modFit$finalModel)
pred1 <- predict(modFit, newdata=testing)
confusionMatrix(testing$classe,pred1 )
```

### Model Attempt #2 - Method="gbm" - Gradient Boosting Algorithm from caret package with a k-fold cross validation.
#### This model yieled 96% accuracy which was better, but still not good enough. *** Code not run to save time.
```{r, eval=FALSE}
set.seed(25)
library(gbm)

modFit_ctrl <- trainControl(method="cv", number=5)
modFit1 <- train(classe ~., method="gbm", 
                 data=training, 
                 verbose=FALSE, 
                 #preProcess=c("center, "scale"),
                 trControl= modFit_ctrl)
print(modFit1$finalModel)
pred2 <- predict(modFit1, newdata=testing)
confusionMatrix(testing$classe,pred2 )
```

### Model Attempt #3 - Method="rf" - Random Forest from caret package with k-fold cross validation.
#### This model took ~ 13 min to run but yielded 99% accuracy. My out of sample error rate is low since I was able to predict with 99% accuracy on the test data and with 100% accuracy on the prediction quiz of 20 data observations.
```{r}
set.seed(25)
library(randomForest)
modFit_ctrl3 <- trainControl(method="cv", number=5)
modFit3 <- train(classe ~., method="rf", 
                 data=training, 
                 verbose=FALSE, 
                 #preProcess=c("center, "scale"),
                 trControl= modFit_ctrl3)
#print(modFit3$finalModel)
pred3 <- predict(modFit3, newdata=testing)
confusionMatrix(testing$classe,pred3 )

```

###  Predicting 20 sets of observations with Model #3 (Random Forest)
```{r, eval=FALSE}

testingFinal <- read.csv("./Coursera Final Project/pml-testing.csv")

pred5 <- predict(modFit3, newdata=testingFinal)
#pred5   #not run b/c not necessary for write up

```
### Appendix

#### The data from this project comes from 
http://groupware.les.inf.puc-rio.br/har .

#### The training set can be found here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

#### The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
