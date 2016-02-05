
### initial caret tutorial
setwd("c:/dev/PML")
library(kernlab)
library(caret)
library(ggplot2)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing2 <- spam[-inTrain,]

set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")

modelFit$finalModel

predictions <- predict(modelFit, newdata=testing)
confusionMatrix(predictions, testing$type)


### 
args(train.default)

### basic plotting
library(ISLR)
library(Hmisc)
data(Wage)
summary(Wage)  # all men in middle atlantic region

inTrain <- createDataPartition(y=Wage$wage, p=0.75, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age", "education", "jobclass")], y=training$wage, plot="pairs")
qq <- qplot(age, wage, color=education, data=training)
qq + geom_smooth(method='lm', formula=y~x)

cutWage <- cut2(training$wage, g=3)
table(cutWage) # factors based on quantile groups

p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"))

t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1) # 1 = row, 2 = column
prop.table(t1,2)

## preprocessing (useful for model based)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve)
summary(training$capitalAve) 
sd(training$capitalAve) # skwewed and highly variable

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)
hist(trainCapAveS)

# when we apply the std to the test set, we have to use the training data

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

# you can also apply this the train function
modelFit <- train(type ~., data=training, preProcess=c("center", "scale"), method="glm")
modelFit

# covariate creation
inTrain <- createDataPartition(y=Wage$wage, p=0.75, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# turning factor variables into dummy variables, i.e.
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training)
dummies
head(predict(dummies, newdata=training))

# removing zero covariates
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv # i.e. sex is mostly males and region is only one region

# spline fitting

