# Nitesh Prakash, np2hf
# SYS 6018

library(readr)  
library(dplyr)  

train = read_csv("train.csv")

plot(train)  # Matrix plot in this case

train.lm <- lm(Survived ~ Pclass, data=train)
summary(train.lm)
abline(train.lm)
mse1 <- mean(train.lm$residuals^2)
mse1
# Generating prediction values for new data
predicttest = read_csv("test.csv")
mypredstest <- predict(train.lm, newdata = data.frame(predicttest)) 

# Generating plot for model
plot(train,pch=20,cex=.2)
lines(predicttest$Survived,mypredstrain,col='red', type='b',cex=.1)
# Writing to file
write.table(mypredstrain, file = "np2hf-hw3-p1-mypredictions.csv", row.names=F, col.names=c("wt"), sep=",")
