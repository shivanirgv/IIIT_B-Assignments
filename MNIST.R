library(kernlab)
library(readr)
library(caret)
library(gridExtra)
library(caTools)

setwd("C:/Users/Shivani Raghuvanshi/Desktop/SVM Dataset")

mnist_train<- read.csv("mnist_train.csv", stringsAsFactors = F, header = F)
mnist_test<- read.csv("mnist_test.csv", stringsAsFactors = F, header = F)

View(mnist_train)
View(mnist_test)

##Data cleaning and Preparation

#Checking for any Duplicated rows

sum(duplicated(mnist_test)) 
sum(duplicated(mnist_train)) 
# No duplicate rows

#Checking for NAs
sum(is.na(mnist_test))
sum(is.na(mnist_train)) 
#No missing values in both the datasets

str(mnist_test)
str(mnist_train)
#All integer values
#Convert V1 into factor
mnist_train$V1 <- factor(mnist_train$V1)
mnist_test$V1 <- factor(mnist_test$V1)

#Understanding Dimensions

dim(mnist_train) 

#Splitting the data into train and test set
set.seed(1)
#As instructed, taking only 15% of data as sample
train.indices = sample(1:nrow(mnist_train), 0.15*nrow(mnist_train))
train <- mnist_train[train.indices, ]
test = mnist_train[-train.indices, ]
test <- mnist_test

## Model Building & Evaluation 

#Constructing Model

#Using Linear Kernel

Model_linear <- ksvm(V1 ~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#Confusion matrix - Linear Kernel
confusionMatrix(Eval_linear, test$V1)

#Accuracy : 0.9166

## Hyperparameter tuning and Cross Validation 

#using cross validation and number of folds as 5
trainControl <- trainControl(method="cv", number=5)

#Evaluation metric is Accuracy.

metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(C= c(0.01, 0.1 ,1 ,10 ,100))
fit.linear<- train(V1~., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

#Results of cross validation
print(fit.linear) 
plot(fit.linear)

Cv_linear <- predict(fit.linear, newdata = test)
confusionMatrix(Cv_linear, test$V1)

# Accuracy - 0.9166
# Specificity > 98%
# Sensitivity > 85%

##Radial Kernel

#Radial kernel using default parameters

model_rbf <- ksvm(V1 ~ ., data = train, scaled = FALSE, kernel = "rbfdot")

eval_rbf <- predict(model_rbf, newdata = test, type = "response")
confusionMatrix(eval_rbf, test$V1) 

# Accuracy : 0.9515
# Specificity > 99%
# Sensitivity > 93%

## Redial kernel with higher sigma

model1_rbf <- ksvm(V1 ~ ., data = train, scaled = FALSE, kernel = "rbfdot",
                   C = 1, kpar = list(sigma = 1))

eval1_rbf <- predict(model1_rbf, newdata = test)
confusionMatrix(eval2_rbf, test$V1) 

#Accuracy dropped down to 0.1117 
#Therefore, it can be concluded that sigma = 1 leads to too much non-linearity and the model is overfitting

##Cross validation

#Making grid of "sigma" and C values
grid_radial<- expand.grid(.sigma = c(0.001, 0.01, 0.1, 1, 5), .C= seq(1, 5, by=1)) 

#  Performing 2-fold cross validation
fit.svm_radial <- train(V1~ ., data = train, metric = metric, method = "svmRadial",tuneGrid = grid_radial,
                 trControl = trainControl)

# Printing cross validation result
print(fit.svm_radial) 
plot(fit.svm_radial)

#Checking overfitting 
#Validating the model results on test data
eval.cv_radial <- predict(fit.svm_radial, newdata = test)
confusionMatrix(eval.cv_radial, test$V1)

# Accuracy is best when C = 3 and sigma = 0.01
# Accuracy - 95%
# Sensitivity > 91%
# Specificity > 98%


##Polynomial Kernel

#Polynomial kernel with degree 2, default scale and offset
model_polynomial<- ksvm(V1 ~ ., data = train, kernel = "polydot", scale = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 1))

eval_polynomial <- predict(model_polynomial, newdata = test)
confusionMatrix(eval_polynomial, test$polynomial)

# Accuracy - 0.9424
# Sensitivity > 92% 
# Specificities > 99%

##Polynomial kernel with varied scale
model1_polynomial <- ksvm(V1 ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = -2, offset = 1))
eval1_polynomial <- predict(model1_polynomial, newdata = test)
confusionMatrix(eval1_polynomial, test$V1)
#Accuracy reduced slightly

##Polynomial kernel with varied offset
model2_polynomial <- ksvm(V1 ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 10))

eval2_polynomial <- predict(model2_polynomial, newdata = test)
confusionMatrix(eval2_polynomial, test$V1)

# Same results

#Using C = 3
model3_polynomial <- ksvm(v1 ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 3, 
                    kpar = list(degree = 2, scale = 1, offset = 1))

eval3_polynomial<- predict(model3_polynomial, newdata = test)
confusionMatrix(eval3_polynomial , test$V1)

# Same results

# Making grid of C values, degree and scale.

grid_polynomial<- expand.grid(.C= c(0.01, 0.1, 1, 10), .degree = c(1, 2, 3, 4, 5), 
                        .scale = c(-100, -10, -1, 1, 10, 100))

fit_polynomial <- train(V1 ~ ., data = train, metric = metric, method = "svmPoly",tuneGrid = grid_polynomial,
                  trControl = trainControl)

#Printing cross validation result
print(fit_polynomial) 
#Plotting model results
plot(fit_polynomial)

#Validating the model results on test data
eval.cv_polynomial <- predict(fit_polynomial, newdata = test)
confusionMatrix(eval.cv_polynomial, test$V1)

#Best model is when C = 0.01, degree = 2, scale = 1
# Accuracy - 94.24%
# Sensitivity > 91% 
# Specificity > 98%


## Implementing optmised polynomial model 
model4_polynomial <- ksvm(V1 ~ ., data = train, kernel = "polydot", scale = FALSE, C = 0.01, 
                    kpar = list(degree = 2, scale = 1, offset = 0.5))

eval4_polynomial<- predict(model4_polynomial, newdata = test)
confusionMatrix(eval4_polynomial, test$V1)

# Accuracy - 0.94


##CONCLUSION : rbf model gives the best accuracy and is hence best suited