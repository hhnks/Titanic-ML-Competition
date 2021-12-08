# "Titanic - Machine Learning from Disaster" - Survival Prediction using R (RStudio)

# Part 1 - Data Cleaning



# Set working directory
setwd("C:/Users/hanna/OneDrive/Dokumenty/Projects/Kaggle/titanic")

# Import libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Import datasets
train <- read.csv("train.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, na.strings="")
test <- read.csv("test.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, na.strings="")


# Getting familiar with training dataset
head(train)
str(train)
summary(train)
# Note that the Survived, Pclass, and Sex columns contain numerical data types, but could be changed to factor for better summary
# For consistent data cleaning and modifying, I merge train and test data.frames 

# Add Survived column to test data.frame, so both frames have the same columns
test$Survived <- NA
# Additionally, I create new column which later help me seperate train and test data.frames
train$IsTrain <- TRUE
test$IsTrain <- FALSE

merged <- rbind(train, test)

# A glance at the merged data.frame
summary(merged)
# Data types changing for Survived, Pclass and Sex columns (numerical -> factor)
merged$Survived <- as.factor(merged$Survived)
merged$Pclass <- as.factor(merged$Pclass)
merged$Sex <- as.factor(merged$Sex)

summary(merged)
# Most of the passengers did not survive (~60%) - excluding NAs from test data.frame
# More passengers travelled 1st class than 2nd class, but 3rd class was most common (~55%)
# Almost 2/3 of passengers were male
# The youngest passenger was 0.17 years old, which is 2 months (if it's not a mistake, e.g. outlier), the oldest passenger was 80 y.o., but median (28 y.o.) and Q3 (39 y.o.) tell that passengers were rather in young age; In next steps, I'll chceck the Age distribution
# Median of SibSp variable shows that at least 50% of passengers travelled with no Siblings/Spouses aboard; 
# 3Q in Parch variable shows that at least 3/4 of passengers travelled with no children/parents
# Min in Fare column is 0, it's possible for Titanic's captain and service (I'll try to verify it), at least 3/4 of passengers paid fare <= 31, the most expensive fare was 512

# ***for future -> in previous steps, str() function showed that Name columns contains titles like 'Mr.', 'Mrs.', so it's possible to find relatives


# Check unique values

Unique_check <- function(x){
  # Check how many unique values has dataframe in each column
  
  Unique_val_df <- matrix(ncol=ncol(x), nrow=1)
  
  # Check over columns
  for(i in 1:ncol(x)) {
    Unique_val_df[ , i] <- length(unique((x[ , i])))
  }
  
  Unique_val_df <- data.frame(Unique_val_df)
  colnames(Unique_val_df) <- colnames(x)
  
  Unique_val_df
}

Unique_check(merged)

# There are few categorical data, e.g. for variables such as Pclass, SibSp, Parch, Embarked -> So it's good to change Embarked for factor data type as well
# Interestingly, there is a relatively large variation in prices for a ticket (Fare), types of tickets (Ticket) and Cabin

merged$Embarked <- as.factor(merged$Embarked)


# Check NAs in data.frame

NA_check <- function(x){
  # Check if dataframe has NAs in each column
  
  NA_df <- matrix(ncol=ncol(x), nrow=1)
  
  # Check over columns
  for(i in 1:ncol(x)) {
    NA_df[ , i] <- sum(is.na((x[ , i])))
  }
  
  NA_df <- data.frame(NA_df)
  colnames(NA_df) <- colnames(x)
  
  NA_df
}

NA_check(merged)
# NAs appear in 5 Columns: Survived (obviously), Age, Fare, Cabin, Embarked



# Dealing with missing values in Age variable
# Visualize Age distribution using histogram, boxplot and density plot
Age_hist <- ggplot(merged, aes(Age))+
  geom_histogram(binwidth = 2, colour="red", fill="pink", na.rm = TRUE)+
  xlab("age")

Age_box <- ggplot(merged, aes(y=Age))+
  geom_boxplot(colour="red", fill="pink", na.rm =TRUE)+
  scale_x_discrete()+
  coord_flip()

Age_dist <- ggplot(merged, aes(x=Age))+
  geom_density(colour="red", fill="pink", na.rm = TRUE)+
  xlab("age")

Age_distribution <- grid.arrange(arrangeGrob(Age_hist, Age_box, Age_dist, nrow = 2), top="Age distribution of Titanic passengers")
ggsave("plots/Age_distribution.pdf", plot=Age_distribution)
Age_distribution

# The distribution is slightly right skewed, that is why I replace NAs by median, not mean

median_age <- median(merged$Age, na.rm = TRUE)
merged$Age <- ifelse(is.na(merged$Age), median_age, merged$Age)

# The distribution is deformed because of number of NA but I'll leave it like that for now
Age_hist <- ggplot(merged, aes(Age))+
  geom_histogram(binwidth = 2, colour="red", fill="pink", na.rm = TRUE)+
  xlab("age")

Age_box <- ggplot(merged, aes(y=Age))+
  geom_boxplot(colour="red", fill="pink", na.rm =TRUE)+
  scale_x_discrete()+
  coord_flip()

Age_dist <- ggplot(merged, aes(x=Age))+
  geom_density(colour="red", fill="pink", na.rm = TRUE)+
  xlab("age")

Age_distribution2 <- grid.arrange(arrangeGrob(Age_hist, Age_box, Age_dist, nrow = 2), top="Age distribution of Titanic passengers")
ggsave("plots/Age_distribution2.pdf", plot=Age_distribution2)
Age_distribution2

# Dealing with missing values in Fare variable
summary(merged$Fare)
# NAs replacing by median
median_fare <- median(merged$Fare, na.rm = TRUE)
merged$Fare <- ifelse(is.na(merged$Fare), median_fare, merged$Fare)



# Dealing with missing values in Cabin variable
unique((merged$Cabin))
# NAs replacing by empty value ""
merged$Cabin <- ifelse(is.na(merged$Cabin), "", merged$Cabin)



# Dealing with missing values in Embarked variable
summary(merged$Embarked)
# Firstly, temporary changing data type to character
merged$Embarked = as.character(merged$Embarked)
# NAs replacing by the most frequently appearing value - "S"
merged$Embarked <- ifelse(is.na(merged$Embarked), "S", merged$Embarked)
# Changing data type to factor and check if NAs are replaced
merged$Embarked <- as.factor(merged$Embarked)
summary(merged$Embarked)


# Save marged data.frame in working directory
write.csv(merged, "merged.csv", row.names = FALSE, na="NA")

# All missing values are substituted. In part 2 I'm going to do some EDA to take a closer look at the variables and their correlation with the target variable - Survived

