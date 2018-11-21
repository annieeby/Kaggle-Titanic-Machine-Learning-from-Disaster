# Titanic Tutorial
# Follow tutorial: https://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/
# Download data: https://www.kaggle.com/c/titanic/data 
  
# Set working directory and import datafiles (see Import Dataset in Environment pane)

setwd('~/Documents/Programming Projects/Data Sets/titanic_all/')
train = read.csv("train.csv")
test = read.csv("test.csv")

#take a quick look at the structure of the dataframe
str(train)

# view survived column
train$Survived

# make a table of survived (1) and not-survived (0)
table(train$Survived)

# send the output of the last function into another, prop.table(), to see the proportion of survived to not-survived
# Observe: 38% of passengers survived the disaster in the training set. 
prop.table(table(train$Survived))

# Create an “everyone dies” prediction for the test set dataframe. 
# Since there was no “Survived” column in the dataframe, create one and repeat the “0” prediction 418 times, the number of rows we have.
# To do this we’ll create a new variable "Survived.
# We'll need to use a new command, rep that simply repeats something by the number of times we tell it to:
test$Survived <- rep(0, 418)

# Submit a csv file with the PassengerId as well as our Survived predictions to Kaggle:
# Extract those two columns from the test dataframe, store them in a new container, and then send it to an output file:

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

# The data.frame command has created a new dataframe with the headings consistent with those from the test set, go ahead and take a look by previewing it. 
head(submit)

# The write.csv command has sent that dataframe out to a CSV file, and importantly excluded the row numbers that would cause Kaggle to reject our submission.
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# Kaggle score: 0.62679

## Sex

# The file should have written to your working directory, so first make sure you can see it in your working directory and then head back to Kaggle and click “Make a submission”.
# Notice that we have 62% of our predictions correct. This is pretty close to the amount we should have expected from the original prop.table that we ran.

# The disaster was famous for saving “women and children first”, so let’s take a look at the Sex and Age variables to see if any patterns are evident. 
summary(train$Sex)

# Use table() to make a two-way comparison on the number of males and females that survived
table(train$Sex, train$Survived)

# Get proportions
prop.table(table(train$Sex, train$Survived))

# Get row-wise proportions. (Use 1 for row, 2 for column proportions):
prop.table(table(train$Sex, train$Survived),1)

# Observe: a majority of females aboard survived, and a very low percentage of males did.

# Change prediction to all males perish, all females live.
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
# Here we have begun with adding the “everyone dies” prediction column as before, except that we’ll ditch the rep command and just assign the zero to the whole column, it has the same effect. We then altered that same column with 1’s for the subset of passengers where the variable “Sex” is equal to “female”.

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

# The data.frame command has created a new dataframe with the headings consistent with those from the test set, go ahead and take a look by previewing it. 
head(submit)

# The write.csv command has sent that dataframe out to a CSV file, and importantly excluded the row numbers that would cause Kaggle to reject our submission.
write.csv(submit, file = "allmalesperish.csv", row.names = FALSE)

# Kaggle score = 0.76555

## Age

# Observe the age variable 
summary(train$Age)

# Dealing with NA's: For now we could assume that the 177 missing values are the average age of the rest of the passengers, ie. late twenties.

summary(train$Age)
train$Age[is.na(train$Age)] <- 29.70

# Create a new binary variable, “Child"
# Begin with the assumption that all are adults. 
train$Child <- 0

# Overwrite the value for passengers below the age of 18.

train$Child[train$Age < 18] <- 1

# As with our prediction column, we have now created a new column in the training set dataframe indicating whether the passenger was a child or not. 

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

# The aggregate command takes a formula with the target variable on the left
# hand side of the tilde symbol and the variables to subset over on the right.
# We then tell it which dataframe to look at with the data argument, and finally
# what function to apply to these subsets. The command above subsets the whole
# dataframe over the different possible combinations of the age and gender
# variables and applies the sum function to the Survived vector for each of
# these subsets. As our target variable is coded as a 1 for survived, and 0 for
# not, the result of summing is the number of survivors. But we don’t know the
# total number of people in each subset; let’s find out:

aggregate(Survived ~ Child + Sex, data=train, FUN=length)


aggregate(Survived ~ Child + Sex, data=train, FUN= function(x) {sum(x)/length(x)})

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
head(submit)
write.csv(submit, file = "3classwomenperish.csv", row.names = FALSE)

# Kaggle score = 0.77990

## Decision Trees
# https://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/
  

# rpart stands for “Recursive Partitioning and Regression Trees” and uses the CART decision tree algorithm. 
library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
#plot
plot(fit)
text(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
head(submit)
write.csv(submit, file = "mysampledtree.csv", row.names = FALSE)

# Kaggle score = 0.77033 (not an improvement)

#load more packages

# install.packages("RGtk2")
# install.packages('rattle')
# install.packages("rattle", dependencies=TRUE)
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
# library(rattle)
# library(RGtk2)
library(rpart.plot)
library(RColorBrewer)


# fancyRpartPlot(fit)
library(rpart.plot)
rpart.plot(fit)
# QUESTION: THIS IS A WORKAROUND BECAUSE COULD NOT INSTALL RATTLE. https://stackoverflow.com/questions/36042749/error-installing-rattle-package-in-mac 

# Override the defaults to make the minimum-to-split-buckets 2 and unleash the cp parameter, the metric that stops splits that aren’t deemed important enough, by reducing it to zero.
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
rpart.plot(fit)

# Trim the plot manually with fancyRpartPlot. 
# An interactive version of the decision tree will appear in the plot tab where you simply click on the nodes that you want to kill. Once you’re satisfied with the tree, hit “quit” and it will be stored to the new.fit object. 

# new.fit <- prp(fit,snip=TRUE)$obj
# rpart.plot(new.fit)
# Question: can't click to kill branches without fancyRpartPlot(fit)


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=4, cp=0.2))
rpart.plot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
head(submit)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Kaggle score = 0.76555 (not an improvement)


## Feature Engineering

## Passenger Titles
# Let’s start with the name field. Take a glance at the first passenger’s name.
train$Name[1]

# Extract passengers' titles to make new variables.
# Perform the same actions on both the training and testing set with row bind.

# To rbind dataframes, they have to have the same columns as each other. 
# Since we lack some columns in our test set, create variables full of missing values (NAs)

test$Survived <- NA
test$Child <- NA
test$Fare2 <- NA

# Make a new dataframe called “combi” with all the same rows as the original two datasets, stacked in the order in which we specified: train first, and test second.
combi <- rbind(train, test)

# Observe that name is classified as a Factor
str(combi)

# Cast this column into a text string
combi$Name <- as.character(combi$Name)

# Use strsplit on the comma right after the person’s last name, and the period after their title. 
# Test on name 1
strsplit(combi$Name[1], split='[,.]')

# Go a level deeper into the indexing mess and extract the title. It’s the second item in this nested list, so let’s dig in to index number 2 of a new container [[1]]:
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Use an apply function to apply this transformation to every row of the combined train/test dataframe

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

# Use sub to strip extra spaces: substitute the first occurrence of a space with nothing 
combi$Title <- sub(" ", "", combi$Title)

# Observe 18 distinct titles
table(combi$Title)

# Combine similar titles
combi$Title[combi$Title %in% c('Mlle', 'Mme')] <- 'Mlle'
combi$Title[combi$Title %in% c('Captain', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Change the variable type back to a factor 
combi$Title <- factor(combi$Title)

## Family Size

# Combine the two variables SibSb and Parch into a new one, FamilySize:
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Extract the passengers’ last names.
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# Create a new Family ID variable.
# Remember: string operations need strings (not factors) to operate
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

# Call any family size of two or less a “small” family
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

# Observe that there are many families size two and smaller here
table(combi$FamilyID)

# Store the table above to a dataframe
famIDs <- data.frame(table(combi$FamilyID))
head(famIDs)

# Subset this dataframe to show only small FamilyID groups
famIDs <- famIDs[famIDs$Freq <= 2,]

# Overwrite any family IDs in our dataset for groups that were not correctly identified
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

# Convert to factor
combi$FamilyID <- factor(combi$FamilyID)

# Split the test and training sets back into their original states
train <- combi[1:891,]
test <- combi[892:1309,]

# Make a prediction
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")
rpart.plot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
head(submit)
write.csv(submit, file = "myengineereddtree.csv", row.names = FALSE)

# Kaggle score = 0.79425 

## Random Forests

# Goal: Use a decision tree to fill in missing age values (rather than mean)

summary(combi$Age)

# Question: The total actual NA values should be 263 but I already replaced my training age values with the mean and saved the altered train file. What now?
# Workaround: Change mean ages back to NA
combi$Age[combi$Age == 29.70] <- NA

# Got the NA's back
summary(combi$Age)

# Grow a tree on the subset of the data with the age values available, and then replace those that are missing
# We now want to use the method="anova" version of our decision tree, as we are not trying to predict a category any more, but a continuous variable. 
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Observe no more NA's
summary(combi$Age)

# Look for other variables missing values. 
summary(combi)

# Question: Why does Child have a bunch of NA's now?

# The variable Embarked has a blank for two passengers.
summary(combi$Embarked)

# Which passengers?
which(combi$Embarked == '')

# Replace blank fields with "S"
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# The variable Fare has one NA.
summary(combi$Fare)

# Which passenger?
which(is.na(combi$Fare))

# Replace with the median fare
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Our dataframe is now cleared of NAs.
# Question: It's not.

# Random Forests in R can only digest factors with up to 32 levels. 
# Observe FamilyID variable had almost double that. 
str(combi$FamilyID)

# Increase cut-off to be a “Small” family from 2 to 3 people

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

# Observe now only 22 levels
str(combi$FamilyID2)

# Split the test and train sets 
train <- combi[1:891,]
test <- combi[892:1309,]

# Install and load the package randomForest
# install.packages('randomForest')
library(randomForest)

# Random forest: Set random seed so results are reproducible (otherwise different each time you run). The number inside isn't important.

set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

#Plot
varImpPlot(fit)

#Predict
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Kaggle score = 0.77033
# Note: Must have missed something... tutorial scored 0.77512

# install.packages('party')
library(party)
install.packages('sandwich')


set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
      Embarked + Title + FamilySize + FamilyID,
      data = train, 
      controls=cforest_unbiased(ntree=2000, mtry=3))

# Predict
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "cforest.csv", row.names = FALSE)

# Kaggle score = 0.81339