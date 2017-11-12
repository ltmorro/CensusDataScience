rm(list = ls())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Author: Luke Morrow
#Date: November 11, 2017
#Purpose: EDA for Adult Census Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(gridExtra)
library(boot)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Obtaining the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
currentDir <- getwd()
dataDir <- 'data'

if (!file.exists(dataDir)){
  dir.create(file.path(currentDir, dataDir))
} else {
  print ("Directory already exists")
}

trainFile <- file.path(currentDir, dataDir, "train.csv")
trainURL <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'
download.file(trainURL, trainFile)

testFile <- file.path(currentDir, dataDir, "test.csv")
testURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
download.file(testURL, testFile)

train <- read.csv(trainFile, header=FALSE)
names(train) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country","income")
summary(train)

test <- read.csv(testFile, header=FALSE, skip=1) #remove the line with "|1x3 Cross validator"
names(test) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country","income")
summary(test)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Exploratory data analysis for continuous variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Relative Frequency histograms for the 6 variables
agePlot <- ggplot(train, aes(x=age)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth=3, color="darkblue", fill="lightblue") + ylab("percentage")
wgtPlot <- ggplot(train, aes(x=fnlwgt)) + geom_histogram(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage")
educationNumPlot <- ggplot(train, aes(x=education.num)) + geom_histogram(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage")
capGainPlot <- ggplot(train, aes(x=capital.gain)) + geom_histogram(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage")
capLossPlot <- ggplot(train, aes(x=capital.loss)) + geom_histogram(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage")
hoursPlot <- ggplot(train, aes(x=hours.per.week)) + geom_histogram(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage")

grid.arrange(agePlot, wgtPlot, educationNumPlot, capGainPlot, capLossPlot, hoursPlot, ncol=2)

#Box plots of >50k <50k for continuous variables
ageIncomePlot <- ggplot(train, aes(x=income, y=age)) + geom_boxplot(color="darkblue", fill="lightblue")
wgtIncomePlot <- ggplot(train, aes(x=income, y=fnlwgt)) + geom_boxplot(color="darkblue", fill="lightblue")
educationNumIncomePlot <- ggplot(train, aes(x=income, y=education.num)) + geom_boxplot(color="darkblue", fill="lightblue")
hoursIncomePlot <- ggplot(train, aes(x=income, y=hours.per.week)) + geom_boxplot(color="darkblue", fill="lightblue")

grid.arrange(ageIncomePlot, wgtIncomePlot, educationNumIncomePlot, hoursIncomePlot, ncol=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Exploratory data analysis for discrete variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Relative Frequency bar graphs for the 8 variables
workPlot <- ggplot(train, aes(x=workclass)) + geom_bar(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage") + coord_flip()
educationPlot <- ggplot(train, aes(x=education)) + geom_bar(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage") + coord_flip()
maritalPlot <- ggplot(train, aes(x=marital.status)) + geom_bar(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage") + coord_flip()
occupationPlot <- ggplot(train, aes(x=occupation)) + geom_bar(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage") + coord_flip()
relationshipPlot <- ggplot(train, aes(x=relationship)) + geom_bar(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage") + coord_flip()
racePlot <- ggplot(train, aes(x=race)) + geom_bar(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage") + coord_flip()
sexPlot <- ggplot(train, aes(x=sex)) + geom_bar(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage") + coord_flip()
nativePlot <- ggplot(train, aes(x=native.country)) + geom_bar(aes(y=..count../sum(..count..)), color="darkblue", fill="lightblue") + ylab("percentage") + coord_flip()

grid.arrange(workPlot, educationPlot, maritalPlot, occupationPlot, relationshipPlot, racePlot, sexPlot, nativePlot, ncol=2)

#Stacked bar graph plots of >50k <50k for discrete variables
workIncomePlot <- ggplot(train, aes(x=workclass, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
maritalIncomePlot <- ggplot(train, aes(x=marital.status, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
occupationIncomePlot<- ggplot(train, aes(x=occupation, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
relationshipIncomePlot<- ggplot(train, aes(x=relationship, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
raceIncomePlot <- ggplot(train, aes(x=race, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
sexIncomePlot <- ggplot(train, aes(x=sex, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()

grid.arrange(workIncomePlot, maritalIncomePlot, occupationIncomePlot, relationshipIncomePlot, raceIncomePlot, sexIncomePlot, ncol=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Model Selection and Training
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m <- glm(income ~ age + education.num + hours.per.week + workclass + marital.status + occupation + relationship + race + sex, family=binomial("logit"), data=train)
sum <- summary(m)$coefficients
sort <- order(sum[,4])
sum <- sum[sort,]
sum

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Model Validation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cross validation
pred.probs <- predict(m, test, type="response")
pred.income <- rep(" <=50K.", length(test$income))
pred.income[pred.probs >= .5] <- " >50K."

error.rate <- mean(pred.income != test$income)
error.rate

#Confusion Matrix
confusion.matrix <- table(test$income, pred.income)
print(addmargins(confusion.matrix))
accuracy <- sum(diag(confusion.matrix)) / length(test$income)
accuracy

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Improving the Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m <- glm(income ~ poly(age, 3) + education.num + hours.per.week + workclass + marital.status + occupation + relationship + race + sex, family=binomial("logit"), data=train)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Model Validation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cross validation
pred.probs <- predict(m, test, type="response")
pred.income <- rep(" <=50K.", length(test$income))
pred.income[pred.probs >= .5] <- " >50K."

error.rate <- mean(pred.income != test$income)
error.rate

#Confusion Matrix
confusion.matrix <- table(test$income, pred.income)
print(addmargins(confusion.matrix))
accuracy <- sum(diag(confusion.matrix)) / length(test$income)
accuracy

#Leave one out cross validation
total <- rbind(train, test)

mycost <- function(r, pi = 0) {
  mean(abs(r-pi) > 0.5)
}

loocv.error <- cv.glm(total, m, K=nrow(total))
loocv.error$delta
