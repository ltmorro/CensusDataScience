rm(list = ls())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Author: Luke Morrow
#Date: November 11, 2017
#Purpose: Individual Data Science Project for Adult Census Data.
#The goal is to predict if an individual makes >50k or <50k. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(gridExtra)
library(boot)
library(glmnet)
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

ggsave("age.png", plot=agePlot, width=12, height=6, dpi=320, units = "in")
ggsave("fnlwgt.png", plot=wgtPlot, width=12, height=6, dpi=320, units = "in")
ggsave("education_num.png", plot=educationNumPlot, width=12, height=6, dpi=320, units = "in")
ggsave("cap_gain.png", plot=capGainPlot, width=12, height=6, dpi=320, units = "in")
ggsave("cap_loss.png", plot=capLossPlot, width=12, height=6, dpi=320, units = "in")
ggsave("hours.png", plot=hoursPlot, width=12, height=6, dpi=320, units = "in")


#Box plots of >50k <50k for continuous variables
ageIncomePlot <- ggplot(train, aes(x=income, y=age)) + geom_boxplot(color="darkblue", fill="lightblue")
wgtIncomePlot <- ggplot(train, aes(x=income, y=fnlwgt)) + geom_boxplot(color="darkblue", fill="lightblue")
educationNumIncomePlot <- ggplot(train, aes(x=income, y=education.num)) + geom_boxplot(color="darkblue", fill="lightblue")
hoursIncomePlot <- ggplot(train, aes(x=income, y=hours.per.week)) + geom_boxplot(color="darkblue", fill="lightblue")

ggsave("age_rel.png", plot=ageIncomePlot, width=12, height=6, dpi=320, units = "in")
ggsave("fnlwgt_rel.png", plot=wgtIncomePlot, width=12, height=6, dpi=320, units = "in")
ggsave("education_num_rel.png", plot=educationNumIncomePlot, width=12, height=6, dpi=320, units = "in")
ggsave("hours_rel.png", plot=hoursIncomePlot, width=12, height=6, dpi=320, units = "in")

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

ggsave("work.png", plot=workPlot, width=12, height=6, dpi=320, units = "in")
ggsave("education.png", plot=educationPlot, width=12, height=6, dpi=320, units = "in")
ggsave("marital.png", plot=maritalPlot, width=12, height=6, dpi=320, units = "in")
ggsave("occupation.png", plot=occupationPlot, width=12, height=6, dpi=320, units = "in")
ggsave("relationship.png", plot=relationshipPlot, width=12, height=6, dpi=320, units = "in")
ggsave("race.png", plot=racePlot, width=12, height=6, dpi=320, units = "in")
ggsave("sex.png", plot=sexPlot, width=12, height=6, dpi=320, units = "in")
ggsave("native.png", plot=nativePlot, width=12, height=6, dpi=320, units = "in")

grid.arrange(workPlot, educationPlot, maritalPlot, occupationPlot, relationshipPlot, racePlot, sexPlot, nativePlot, ncol=2)

#Stacked bar graph plots of >50k <50k for discrete variables
workIncomePlot <- ggplot(train, aes(x=workclass, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
maritalIncomePlot <- ggplot(train, aes(x=marital.status, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
occupationIncomePlot<- ggplot(train, aes(x=occupation, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
relationshipIncomePlot<- ggplot(train, aes(x=relationship, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
raceIncomePlot <- ggplot(train, aes(x=race, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()
sexIncomePlot <- ggplot(train, aes(x=sex, y=..count../sum(..count..), fill=workclass))+ geom_bar(aes(fill=income)) + scale_fill_manual(values=c("darkblue", "lightblue")) + ylab("percentage") + coord_flip()

ggsave("work_rel.png", plot=workIncomePlot, width=12, height=6, dpi=320, units = "in")
ggsave("marital_rel.png", plot=maritalIncomePlot, width=12, height=6, dpi=320, units = "in")
ggsave("occupation_rel.png", plot=occupationIncomePlot, width=12, height=6, dpi=320, units = "in")
ggsave("relationship_rel.png", plot=relationshipIncomePlot, width=12, height=6, dpi=320, units = "in")
ggsave("race_rel.png", plot=raceIncomePlot, width=12, height=6, dpi=320, units = "in")
ggsave("sex_rel.png", plot=sexIncomePlot, width=12, height=6, dpi=320, units = "in")

grid.arrange(workIncomePlot, maritalIncomePlot, occupationIncomePlot, relationshipIncomePlot, raceIncomePlot, sexIncomePlot, ncol=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Model Selection and Training
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m <- glm(income ~ age + education.num + hours.per.week + workclass + marital.status + occupation + relationship + race + sex, family=binomial("logit"), data=train)
sum <- summary(m)$coefficients
sort <- order(sum[,4])
sum <- data.frame(sum[sort,])
print(sum)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Improving the Model using LASSO
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert the data to a matrix to perform Lasso shrinkage technique
x <- model.matrix(income~., train) 
y <- ifelse(train$income==" <=50K",0,1)

cv.out <- cv.glmnet(x,y,alpha=1,family="binomial", type.measure = "mse")

plot(cv.out)

lambda1se <- cv.out$lambda.1se

plot(lambda1se)

coefTemp <- (coef(cv.out,s=lambda1se))
oddsTemp <- exp(coef(cv.out,s=lambda1se))[which(exp(coef(cv.out,s=lambda1se)) != 1)]

df <- data.frame(Feature = coefTemp@Dimnames[[1]][coefTemp@i + 1], Coefficients = coefTemp@x, OddsRatio= oddsTemp)

oddsDF <- data.frame(as.matrix(exp(coef(cv.out,s=lambda1se))))
mask <- rownames(oddsDF)[-(rownames(coefDF) == '(Intercept)')]
df$OddsRatios <- oddsDF[mask, ]
print(df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Model Validation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cross validation
x_test<- model.matrix(income~.,test)
#since the test data set does not have a native country Holand-Netherlands, we must add that column to the matrix so dimensions match
holand <- rep(0, 16281)
x_test <- cbind(x_test[,1:74], holand, x_test[,75:100])

lasso.probs <- predict(cv.out, newx=x_test,s=lambda1se, type="response")
lasso.income <- rep(" <=50K.", length(test$income))
lasso.income[lasso.probs >= .5] <- " >50K."

error.rate <- mean(lasso.income != test$income)
print(error.rate)

#Confusion Matrix
confusion.matrix <- table(test$income, lasso.income)
print(addmargins(confusion.matrix))
accuracy <- sum(diag(confusion.matrix)) / length(test$income)
print(accuracy)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Random Forest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(randomForest)
m <- randomForest(income ~ age + education.num + hours.per.week + workclass + marital.status + occupation + relationship + race + sex, data=train)

rf.pred <- predict(m, test, type="response")

#Confusion Matrix
confusion.matrix <- table(test$income, rf.pred)
print(addmargins(confusion.matrix))
accuracy <- sum(diag(confusion.matrix)) / length(test$income)
print(accuracy)
