# "Titanic - Machine Learning from Disaster" - Survival Prediction using R (RStudio)

# Part 3 - Decision Tree Model


# Set working directory
setwd("C:/Users/hanna/OneDrive/Dokumenty/Projects/Kaggle/titanic")

# Import library
library(dplyr)
library(rpart)
library(rpart.plot)

# Import dataset
merged <- read.csv("merged_fam.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, fill = TRUE)

head(merged)
summary(merged)

# Change data types of Survived, Pclass, Sex, Embarked to factor as in Part 1
merged$Survived <- as.factor(merged$Survived)
merged$Pclass <- as.factor(merged$Pclass)
merged$Sex <- as.factor(merged$Sex)
merged$Embarked <- as.factor(merged$Embarked)
merged$Fam <- as.factor(merged$Fam)

# Check if there are no NAs except for Survived variable
sum(is.na(merged[,3:13]))


# Split the 'merged' data.frame to training set and test set
train <- merged %>% filter(IsTrain == TRUE)
test <- merged %>% filter(IsTrain == FALSE)


# Create Decision Tree Model with most variables
DTree <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Fam, 
               data=train, 
               maxdepth=10)
summary(DTree)

rpart.plot(DTree)


# Decision Tree Model with Fam variable instead of SibSp+Parch and without Fare
DTree <- rpart(Survived ~ Pclass+Sex+Age+Fam+Embarked, data=train, 
              maxdepth=6)
summary(DTree)

rpart.plot(DTree)


# Prediction for test data.frame
pred <- predict(DTree, newdata = test, type = "class")

summary(pred)


# Create data.frame and csv for Kaggle submition (PassengerId+Survived)
submitDT <- data.frame(PassengerId = test$PassengerId, 
                       Survived = pred)

head(submitDT)
tail(submitDT)

write.csv(submitDT, file = "PredSubmit.csv", row.names = FALSE)

# Accuracy of this prediction is 0.77511
