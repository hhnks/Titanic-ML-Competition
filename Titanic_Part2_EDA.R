# "Titanic - Machine Learning from Disaster" - Survival Prediction using R (RStudio)

# Part 2 - EDA (Exploratory data analysis)



setwd("C:/Users/hanna/OneDrive/Dokumenty/Projects/Kaggle/titanic")

# Import library
library(ggplot2)
library(gridExtra)

# Import dataset
merged <- read.csv("merged.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, fill = TRUE)

head(merged)
tail(merged)
summary(merged)

# Change data types of Survived, Pclass, Sex, Embarked to factor as in Part 1
merged$Survived <- as.factor(merged$Survived)
merged$Pclass <- as.factor(merged$Pclass)
merged$Sex <- as.factor(merged$Sex)
merged$Embarked <- as.factor(merged$Embarked)

# Check if there are no NAs except for Survived variable
sum(is.na(merged[,3:13]))

# It's finally time for some EDA;
# The goal is to find patterns and groups of people that were more likely to survive Titanic Disaster
# This is why I'm using also normalized y axis for easier comparision of proportions


# Survival rate by Class
pclass_bar_norm <- ggplot(merged[1:891,], aes(x=Pclass, fill=Survived))+
  geom_bar(position = 'fill')

pclass_bar <- ggplot(merged[1:891,], aes(x=Pclass, fill=Survived))+
  geom_bar( position = 'dodge')

grid.arrange(pclass_bar_norm, pclass_bar, nrow=1)
ggsave("plots/pclass_barplot.pdf", plot = last_plot())
# Over 60% of passengers from 1st class survived, while 75% people from 3rd class died


# Survival rate by sex
sex_bar_norm <- ggplot(merged[1:891,], aes(x=Sex, fill=Survived))+
  geom_bar( position = 'fill')

sex_bar <- ggplot(merged[1:891,], aes(x=Sex, fill=Survived))+
  geom_bar( position = 'dodge')

grid.arrange(sex_bar_norm, sex_bar, nrow=1)
ggsave("plots/sex_barplot.pdf", plot = last_plot())
# Almost 75% of female passengers survived, while only about 18% of male passengers survived. 
# Note that only ~ 1/3 of all passengers were female


# Survival rate by number of Siblings/Spouses aboard
sibsp_bar_norm <- ggplot(merged[1:891,], aes(x=SibSp, fill=Survived))+
  geom_bar( position = 'fill')

sibsp_bar <- ggplot(merged[1:891,], aes(x=SibSp, fill=Survived))+
  geom_bar( position = 'dodge')

grid.arrange(sibsp_bar_norm, sibsp_bar, nrow=1)
ggsave("plots/sibsp_barplot.pdf", plot = last_plot())
# Passengers with 1 Sibling/Spouse were slightly more likely to survive. 
# For passengers with >=4 Siblings/Spouses, chances for surviving were very low (though such passengers were few)


#Survival rate by number of Parents/Children aboard
parch_bar_norm <- ggplot(merged[1:891,], aes(x=Parch, fill=Survived))+
  geom_bar( position = 'fill')

parch_bar <- ggplot(merged[1:891,], aes(x=Parch, fill=Survived))+
  geom_bar( position = 'dodge')

grid.arrange(parch_bar_norm, parch_bar, nrow=1)
ggsave("plots/parch_barplot.pdf", plot = last_plot())
# Passengers who travelled with 1-3 Parents/Children were more likely to survive.


# Survival rate by Embarked
emb_bar_norm <- ggplot(merged[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar( position = 'fill')

emb_bar <- ggplot(merged[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar(position = 'dodge')

grid.arrange(emb_bar_norm, emb_bar, nrow=1)
ggsave("plots/emb_barplot.pdf", plot = last_plot())
# For some reason, more than 50% of passengers that embarked from Cherbourg survived.


# Normalized histogram of discretized variable - passangers Age
age_hist_norm <- ggplot(merged[1:891,], aes(x=Age, fill=Survived))+
  geom_histogram( binwidth = 5, position = 'fill', colour='black')

age_hist <- ggplot(merged[1:891,], aes(x=Age, fill=Survived))+
  geom_histogram( binwidth = 5, colour='black')

grid.arrange(age_hist_norm, age_hist, nrow=1)

sum((merged$Age > 60) & (!is.na(merged$Survived)))
ggsave("plots/age_hist.pdf", plot=last_plot())
# Most children under 10 y.o. survived. 
# There might be lower survival rate for > 60 y.o. passengers but this group was too small for such statement (22 records in training dataset)


# Normalized histogram of discretized variable - Fare
fare_hist_norm <- ggplot(merged[1:891,], aes(x=Fare, fill=Survived))+
  geom_histogram( binwidth = 20, position = 'fill', colour='black')

fare_hist <- ggplot(merged[1:891,], aes(x=Fare, fill=Survived))+
  geom_histogram( binwidth = 20, colour='black')

grid.arrange(fare_hist_norm, fare_hist, nrow=2)
ggsave("plots/fare_hist.pdf", plot=last_plot())
# Higher survvial rate for fare > 50

# One variable plots showed some groups of people that were more likely to survive
# The next step is to study multidimensional relationships between variables


# Seems like the highest survival rates were for female, 1st class passengers and children <10yo
# Lowest survival rates were for male, 3rd class passengers, embarked from Southampton

# Also, survival rate for passengers with 0 Siblings/Spouses or 0 Parents/Children aboard was below 0.4 for each
# Check if passengers with no relatives aboard had lower survival rate than passengers that travelled with family

ggplot(merged[1:891,])+
  geom_point(aes(x=SibSp, y=Parch, color=Survived), position='jitter')
ggsave("plots/parch_sibsp_jitter.pdf", plot=last_plot())
# 3 groups can be distinguished from the plot above: single travellers (small survival ratio), small families (high survival ratio) and large families (small survival ratio)
# Creating new column with passengers grouped into 3 groups of family size + visualization


merged$Fam[merged$SibSp == 0 & 
             merged$Parch == 0 ] <- 'single_traveller'
merged$Fam[(merged$SibSp != 0 | merged$Parch !=0) & 
             (merged$SibSp <=3 & merged$Parch <=3)] <- 'small_family'
merged$Fam[merged$SibSp > 3 | 
             merged$Parch >3 ] <- 'large_family'


fam_bar_norm <- ggplot(merged[1:891,], aes(x=Fam, fill=Survived))+
  geom_bar( position = 'fill')

fam_bar <- ggplot(merged[1:891,], aes(x=Fam, fill=Survived))+
  geom_bar()

grid.arrange(fam_bar_norm, fam_bar, nrow=1)
ggsave("plots/fam_bar.pdf", plot=last_plot())
# We can clearly see that ~90% of passengers with large family aboard and ~70% of single travellers didn't survive
# Add Sex variable to fam_bar

ggplot(merged[1:891,], aes(x=Sex, fill=Survived))+
  geom_bar(position = 'fill')+
  facet_wrap( ~Fam)
ggsave("plots/fam_sex_hist.pdf", plot=last_plot())
# By adding another variable, we can see that real survival rates for family members is strongly related with the sex of passenger

# Add another variable - Age, by using geom_point()
ggplot(merged[1:891,])+ 
  geom_point(aes(x=Fam, y=Age, color=Survived), position = 'jitter')+
  facet_wrap( ~Sex, nrow = 1)
ggsave("plots/fam_sex_age_point.pdf", plot=last_plot())

# Replace Age with another continuous variable - Fare
ggplot(merged[1:891,])+ 
  geom_point(aes(x=Fam, y=Fare, color=Survived), position = 'jitter')+
  facet_wrap( ~Sex, nrow = 1)
ggsave("plots/fam_sex_fare_point.pdf", plot=last_plot())

# Combine Age and Fare
ggplot(merged[1:891,])+
  geom_point(aes(x=Age, y=Fare, color=Survived))+
  facet_grid( Sex~Fam)
ggsave("plots/fam_sex_age_fare_point.pdf", plot=last_plot())
# The facet with continuous variables on axes, in this case, does not bring new insights
# I'll stay with 1 continuous variable - age of passengers

ggplot(merged[1:891,])+
  geom_point(aes(x=Pclass, y=Age, color=Survived))+
  facet_grid( Sex~Fam)
ggsave("plots/fam_sex_age_pclass_point.pdf", plot=last_plot())
# Based on chart above, For male passengers we can define a group with high survival rate - men <20y.o. with small family aboard and from 1st or 2nd class
# For female, we can also define such group - women of any age, with none or small family aboard and from 1st or 2nd class


ggplot(merged[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar(position = 'fill')+
  facet_grid( Sex~Pclass)
ggsave("plots/emb_sex_pclass_hist.pdf", plot=last_plot())
# By last facet, we can add that if a passanger was a female from 1st or 2nd class that embarked from Cherbourg or Queenstown, 
# the chances of survival were close to 100%.



# Save marged data.frame in working directory
write.csv(merged, "merged_fam.csv", row.names = FALSE, na="NA")


# In part 3 I'm going to create decision tree and predict which passengers from 'test' data.frame survived