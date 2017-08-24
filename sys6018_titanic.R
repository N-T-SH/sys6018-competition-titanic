# Nitesh Prakash, np2hf
# SYS 6018
install.packages("missForest")


library(readr)  
library(dplyr)  
library(missForest)

train = read.csv("train.csv")
# Selecting columns to keep
keeps <- c("PassengerId","Survived", "Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")
train.req <- train[keeps]
train.req$Survived <- factor(train.req$Survived)
train.req$Pclass <- factor(train.req$Pclass)
train.req$SibSp <- factor(train.req$SibSp)
train.req$Parch <- factor(train.req$Parch)
train.req$Embarked <- factor(train.req$Embarked)

#using missForest to impute missing values
train.imp <- missForest(train.req)

#check imputed values
train.imp$ximp

#check imputation error
train.imp$OOBerror
# Using only terms in model that are significant
train.lg <- glm(Survived~Pclass+Sex+Age+SibSp, data=train.imp$ximp, family = "binomial")
summary(train.lg)
mse1 <- mean(train.lg$residuals^2)
mse1

# Checking validation set
test = read.csv("test.csv")
# Selecting columns to keep
keeps <- c("PassengerId","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")
test.req <- test[keeps]
test.req$Pclass <- factor(test.req$Pclass)
test.req$SibSp <- factor(test.req$SibSp)
test.req$Parch <- factor(test.req$Parch)
test.req$Embarked <- factor(test.req$Embarked)

#using missForest to impute missing values
test.imp <- missForest(test.req)

# Calculating prediction values
test.imp$ximp$SurvivedProb<-predict(train.lg,newdata=test.imp$ximp, type="response")
test.imp$ximp$Survived[test.imp$ximp$SurvivedProb>0.5] <- 1 # p>0.5 -> 1
test.imp$ximp$Survived[test.imp$ximp$SurvivedProb<=0.5] <- 0

# Writing to file
write.csv(test.imp$ximp[,c("PassengerId","Survived")], file = "np2hf_submissions.csv", row.names=FALSE)
