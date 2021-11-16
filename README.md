# Titanic-ML-Competition
Titanic Competition is one the most popular data science challenge on Kaggle platform:

https://www.kaggle.com/c/titanic
The goal of the challenge is simple: predict which people survived the Titanic disaster.

* Of course, the Titanic dataset is an open dataset which you can reach on many repositories and websites. However, I used dataset from Kaggle. Also, Kaggle easily measure the accuracy of your predictions. 

In this competition, you obtain two similar datasets: `train.csv` and `test.csv`.

1. `train.csv` contains the details of a subset of the 891 passengers on board and importantly, will reveal whether they survived or not.
2. `test.csv` dataset contains similar data for another 418 passengers but does not have survival information - it’s your job to predict these outcomes.

The training set is used to build a machine learning model, while test dataset is used to see how high is the accuracy of created model.
Kaggle also includes `gender_submission.csv`, a set of predictions that assume all and only female passengers survive, as an example of what a submission file should look like.


Variables in dataset with definition:
* PassengerId - unique ID
* Survived - 
* Pclass 
* Name
* Sex
* Age
* SibSp
* Parch
* Ticket      
* Fare
* Cabin
* Embarked   

## R (RStudio)

R version 4.1.1 (2021-08-10)

My work is divided into 3 parts:
* Part 1 - Data Cleaning
* Part 2 - EDA
* Part 3 - The Random Forest Model
