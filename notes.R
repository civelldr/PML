
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
library(splines)
bsBasis <- bs(training$age, df=3) # 3rd degree polynomial
head(bsBasis)
# age, age^2, age^3

lm <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm, newdata=training), col="red", pch=19, cex=0.5)

predict(bsBasis, age=testing$age) # predict from the bsBasis from the training set

# proprocessing with PCA.  when you don't want to include all the variables
# if some of the variables are highly correlated

data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# calculate the correlation between all the covariates (leaving out one)
M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)
names(spam)[c(34,32)]
plot(spam[,34], spam[,32]) # right on the diagnoal

# one way to visualize the contribution of each variable is to rotate the plot

x <- 0.71*training$num415 + 0.71*training$num857
y <- 0.71*training$num415 - 0.71*training$num857
plot(x,y) # here you can the X is spread out, but the Y is mostly clustered near zero, 
# so adding the numbers gives you more information than subtracting the values
