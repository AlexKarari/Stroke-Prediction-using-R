# STROKE DATASET PREDICTION USING R
install.packages("DataExplorer")
install.packages('caTools')
install.packages('tidyverse')
install.packages("caret")
install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")

library(DataExplorer)
library(caTools)
library(tidyverse)
library(caret)
library(ggplot2)
library(plyr)
library(dplyr)

## Reading the data
trainDS = read.csv('train_2v.csv')

## Viewing the dataset
View(trainDS)

## Dimensions of the dataset
dim(trainDS)

## Features of the dataset
names(trainDS)

## Structure of the datasets
str(trainDS)

## Summary of the dataset
summary(trainDS)

# EDA
## bmi
ggplot(trainDS, aes(as.factor(stroke), bmi))+ geom_boxplot(col = "blue")+
  labs(title = "BMI by Stroke",
       x = "stroke") + 
  theme(plot.title = element_text(hjust = .5))

## Age
ggplot(trainDS, aes(as.factor(stroke), age, fill = gender))+ 
  geom_boxplot()+
  labs(
    title = "Age against Stroke",
    x = "stroke") + 
  theme(plot.title = element_text(hjust = .5))

## hypertension
ggplot(trainDS, aes(age))+ 
  geom_density(col = "black")+
  geom_histogram(aes(y=..density..), alpha = .3, fill = "blue")+
  facet_grid(as.factor(hypertension)~.)

## Heart Disease
ggplot(trainDS, aes(as.factor(heart_disease), age, fill = as.factor(heart_disease)))+
  geom_boxplot()+
  labs(
    title = "Age against Heart Disease",
    x = "heart_disease"
  )+
  scale_fill_discrete("heart_disease")+
  theme(plot.title = element_text(hjust = .5))

# DATA CLEANING
## gender variable that has "other" as a type of gender. We filter this out.
trainDS <- filter(trainDS, gender!="Other")

## Drop ID variable. It is irrelevant in model building
trainDS$id <- NULL

# Encoding categorical data to factors
## 1) ever_married
trainDS$ever_married = factor(trainDS$ever_married,
                              levels = c('No','Yes'),
                              labels = c(0,1))

## 2) work_type
trainDS$work_type = factor(trainDS$work_type,
                           levels = c('children','Govt_job','Never_worked',
                                      'Private','Self-employed'),
                           labels = c(0,1,2,3,4))

## 3) Residence_type
trainDS$Residence_type = factor(trainDS$Residence_type,
                                levels = c('Rural','Urban'),
                                labels = c(0,1))

## 4) gender
trainDS$gender = factor(trainDS$gender,
                        levels = c('Male','Female'),
                        labels = c(0,1))

## Features of trainDS after encoding
summary(trainDS)
str(trainDS)

## Check for missing data in the trainDS  
plot_missing(trainDS)
sum (is.na(trainDS))
colSums(sapply(trainDS,is.na))

## smoking_status variable has missing values that cannot be recognized by R.
## solution is to attach "NA"
table(trainDS$smoking_status)
trainDS$smoking_status[trainDS$smoking_status==""]<-NA

## Re-check missing variable. smoking_status now has missing values 
plot_missing(trainDS)
sum (is.na(trainDS))
colSums(sapply(trainDS,is.na))

## Solution 1 - drop smoking_status variable. It goes beyond the threshold or replacing missing data 
## Drop smoking_status variable with 30.63%. Required threshold is less than or eqaul to 5%
droppedDS <- trainDS[,-10]
temp_data <- droppedDS
View(droppedDS)
View(temp_data)

## Imputing BMI missing values
droppedDS$bmi = ifelse(is.na(droppedDS$bmi),
                       ave(droppedDS$bmi, FUN = function(x) mean(x, na.rm = TRUE)),
                       droppedDS$bmi)

## Check for missing data once more in trainOption_One
plot_missing(droppedDS)
sum (is.na(droppedDS))
colSums(sapply(droppedDS,is.na))
## smoking_status variable dropped. No missing data left.

## Features of droppedDS
summary(droppedDS)
str(droppedDS)

## Solution 2 - Replace missing data with "undisclosed"
## Imputing "undisclosed" to missing data
replacedDS <- trainDS[,10]
replacedDS <- as.factor(ifelse(is.na(replacedDS), "undisclosed", paste(replacedDS)))
temp_data$smoking_status <- replacedDS
replacedDS <- temp_data
View(replacedDS)

## Now encode smoking_status categorical variable to factors
replacedDS$smoking_status = factor(replacedDS$smoking_status,
                                   levels = c('formerly smoked','never smoked',
                                              'smokes','undisclosed'),
                                   labels = c(0,1,2,3))

## Viewing the replacedDS after encoding
View(replacedDS)

## Imputing BMI missing values
replacedDS$bmi = ifelse(is.na(replacedDS$bmi),
                        ave(replacedDS$bmi, FUN = function(x) mean(x, na.rm = TRUE)),
                        replacedDS$bmi)


## Check for missing data once more in replacedDS 
plot_missing(replacedDS)
sum (is.na(replacedDS))
colSums(sapply(replacedDS,is.na))
## smoking_status missing variables replaced with "undisclosed." No Missing data left.

#reorder column index
replacedDS <- replacedDS[c(1,2,3,4,5,6,7,8,9,11,10)]

## Features of replacedDS
summary(replacedDS)
str(replacedDS)

## Datasets to now work with.
## dropped DS has smoking_status variable dropped due to its high percentage in missing values
## replacedDS has missing values in smoking_status variable replaced with "undisclosed"
View(droppedDS)
View(replacedDS)

## All missing data at this point has been imputed

## Transforming the variables into factors or numeric data types for all above datasets
droppedDS <- droppedDS %>%
  mutate(hypertension = as.factor(hypertension))%>%
  mutate(heart_disease = as.factor(heart_disease))%>%
  mutate(stroke = as.factor(stroke)) 

str(droppedDS)
str(replacedDS)

# Checking for class balance using ROSE package for both train datasets

## install.packages("ROSE")
library(ROSE)
## check table to view no.of 0s & 1s in the stroke variable 
table(droppedDS$stroke)
table(replacedDS$stroke)
## check class distribution
prop.table(table(droppedDS$stroke))
prop.table(table(replacedDS$stroke))
## Barplot for class distribution
barplot(prop.table(table(droppedDS$stroke)),
        col = rainbow(2),
        ylim = c(0,1),
        main = "Class Distribution for droppedDS"
)

barplot(prop.table(table(replacedDS$stroke)),
        col = rainbow(2),
        ylim = c(0,1),
        main = "Class Distribution for replacedDS"
)

## Undersampling and Oversampling using ROSE for droppedDS
balancedDroppedDS <- ovun.sample(stroke ~., data = droppedDS, method = "both", 
                                 p = 0.5, N=43400, seed = 1)$data
table(balancedDroppedDS$stroke)

barplot(prop.table(table(balancedDroppedDS$stroke)),
        col = rainbow(2),
        ylim = c(0,1),
        main = "Class Distribution for balancedDroppedDS"
)

## Undersampling and Oversampling using ROSE for replacedDS
balancedReplacedDS <- ovun.sample(stroke ~., data = replacedDS, method = "both", 
                                  p = 0.5, N=43400, seed = 1)$data
table(balancedReplacedDS$stroke)

barplot(prop.table(table(balancedReplacedDS$stroke)),
        col = rainbow(2),
        ylim = c(0,1),
        main = "Class Distribution for balancedReplacedDS"
)

### Both oversampling & undersampling are done.
### The minority class is oversampled with replacement and majority class
### is undersampled without replacement. 

# Splitting the balancedDroppedDS into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(balancedDroppedDS$stroke, SplitRatio = 0.8)
droppedTrainingSet = subset(balancedDroppedDS, split == TRUE)
droppedTestSet = subset(balancedDroppedDS, split == FALSE)

# Splitting the balancedReplacedDS into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(balancedReplacedDS$stroke, SplitRatio = 0.8)
replacedTrainingSet = subset(balancedReplacedDS, split == TRUE)
replacedTestSet = subset(balancedReplacedDS, split == FALSE)

## Dataset to now work with are:
droppedTrainingSet
droppedTestSet
replacedTrainingSet
replacedTestSet

# Feature Scaling
# First, we reorder column index and combine all numeric data types together
# droppedTrainingSet <- droppedTrainingSet[c(1,2,8,9,3,4,5,6,7,10)]
# droppedTrainingSet = scale(droppedTrainingSet[2:4])
# droppedTestSet = scale(droppedTestSet)
# replacedTrainingSet = scale(replacedTrainingSet)
# replacedTestSet = scale(replacedTestSet)

# MODELING - SECTION 1: balancedDroppedDS
## 1.DECISION TREE ALGORITHM FOR balancedDroppedDS
## install.packages("rpart")
## install.packages("rpart.plot")

## Models to be used
droppedTrainingSet
droppedTestSet

library(rpart)
library(rpart.plot)

## Generating trees with Gini Index
(tree <- rpart(stroke ~., data=droppedTrainingSet, method="class"))
prp(tree)
prp(tree, type = 3)
rpart.plot(tree, extra = 104, nn = TRUE)
rpart.rules(tree, cover=TRUE)
rpart.plot(tree, extra = 101, nn = TRUE)
plotcp(tree)

# Split with entropy information
entTree <- rpart(stroke ~ ., data=droppedTrainingSet, method="class", parms=list(split="information"))
prp(entTree)
prp (entTree, type = 3)
rpart.plot(entTree, extra = 104, nn = TRUE)
plotcp(entTree)

# Using Decision tree with parameter settings.Training data is used
tree_with_params = rpart(stroke ~ ., data=droppedTrainingSet, method="class", minsplit = 1, minbucket = 10, cp = -1)
prp (tree_with_params)
print(tree_with_params)
summary(tree_with_params)
plot(tree_with_params)
text(tree_with_params)
plotcp(tree_with_params)

## Predicting and evaluating the performance of the trained tree model 
Predict <- predict(tree_with_params, droppedTestSet)
Predict

## Redoing the validation data prediction and displaying the class result:
Predict <- predict(tree_with_params, droppedTestSet, type = "class")
Predict

## Result evaluation
## Creating the confusion matrix
(Confusion_matrix <- table(Predict, droppedTestSet$stroke))
print(Confusion_matrix)

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- Confusion_matrix[1,1])
(FN <- Confusion_matrix[1,2])
(FP <- Confusion_matrix[2,1])
(TP <- Confusion_matrix[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

# ROC curve
# install.packages("ROCR")
library(ROCR)

## Predicting the probability values to draw the ROC (receiver operating characteristic) curve.
## Running predict again: Note: PredictROC is the same as Predict with "type = prob", instead of "type = class"
(PredictROC = predict(tree_with_params, droppedTestSet))

## Keeping only the 2nd column containing probability where stroke = 1:
PredictROC2 = PredictROC[,2]

## The prediction is rerun using the probability of stroke = 1:
(pred = prediction(PredictROC2, droppedTestSet$stroke))

(perf = performance(pred, "tpr", "fpr"))

# Plotting the ROC curve:
plot(perf, colorize = T)

# Alternative version of the ROC curve with relabeled X and Y axes and (1-Specificity) cut-off markers by increments of 0.3 from range 0 to 1 to indicate the
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "(1 - Specificity)",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

## Calculating the Area Under Curve (AUC):
auc = as.numeric(performance(pred, "auc")@y.values)
(auc = round(auc, 3))


## 2.RANDOM FOREST ALGORITHM FOR balancedDroppedDS
# install.packages("corrplot")
# install.packages("reshape2")
# install.packages("ggthemes")
# install.packages("randomForest")
library(ggplot2)
library(corrplot)
library(reshape2)
library(ggthemes)
library(dplyr)
library(randomForest)

## Datasets to be used
droppedTrainingSet
droppedTestSet

## Transforming the variables into factors or numeric data types for all above datasets
droppedTrainingSet <- droppedTrainingSet %>%
  mutate(hypertension = as.factor(hypertension))%>%
  mutate(heart_disease = as.factor(heart_disease))%>%
  mutate(stroke = as.factor(stroke)) 

droppedTestSet <- droppedTestSet %>%
  mutate(hypertension = as.factor(hypertension))%>%
  mutate(heart_disease = as.factor(heart_disease))%>%
  mutate(stroke = as.factor(stroke)) 

## Fitting a random forest model
rfModel <- randomForest(stroke~.,data = droppedTrainingSet, importance = TRUE)
print(rfModel)
attributes(rfModel)

## Visualizing the plot
## Option 1
varImpPlot(rfModel) 

## option 2
importance = importance(rfModel)
varImportance = data.frame(Variables = row.names(importance), Importance =round(importance[,"MeanDecreaseAccuracy"],2)) 
rankImportance=varImportance%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance))+ 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

## Testing the model with our test set
(rfPredict1 <- predict(rfModel, droppedTrainingSet))
(cm <- table(rfPredict1, droppedTrainingSet$stroke))

(rfPredict2 <- predict(rfModel, droppedTestSet))
(cm <- table(rfPredict2, droppedTestSet$stroke))

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- cm[1,1])
(FN <- cm[1,2])
(FP <- cm[2,1])
(TP <- cm[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

plot(rfModel)

## Tuning the RF model
# Fine tuning parameters of Random Forest model
tunedModel <- randomForest(stroke ~ ., data = droppedTrainingSet, ntree = 500, mtry = 6, importance = TRUE)
tunedModel

# Predicting on train set
predTrain <- predict(tunedModel, droppedTrainingSet, type = "class")
# Checking classification accuracy
table(predTrain, droppedTrainingSet$stroke)
(cm <- table(predTrain, droppedTrainingSet$stroke))

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- cm[1,1])
(FN <- cm[1,2])
(FP <- cm[2,1])
(TP <- cm[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

# Predicting on Test/validation set
predValid <- predict(tunedModel, droppedTestSet, type = "class")
# Checking classification accuracy
mean(predValid == droppedTestSet$stroke)                    
table(predValid,droppedTestSet$stroke)
(cm <- table(predValid,droppedTestSet$stroke))

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- cm[1,1])
(FN <- cm[1,2])
(FP <- cm[2,1])
(TP <- cm[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

# To check important variables for droppedTrainingSet
importance(tunedModel) 

### Plotting option 1
varImpPlot(tunedModel)

### Ploting option 2
## option 2
importance = importance(tunedModel)
varImportance = data.frame(Variables = row.names(importance), 
                           Importance =round(importance[,"MeanDecreaseGini"],2)) # MeanDecreaseAccuracy can be used to substitute this
rankImportance=varImportance%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance))+ 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()


print(tunedModel)

# Recall that values on the diagonal correspond to true positives and true 
# negatives (correct predictions) whereas the others correspond to false positives 
# and false negatives.

## 3.LINEAR REGRESSION ALGORITHM FOR balancedDroppedDS
## Models to be used
droppedTrainingSet
droppedTestSet

## Model Fitting
model <- glm(stroke~.,family = binomial, data = droppedTrainingSet)
summary(model)

# Predicting the Test set results for droppedTestSet
(predict <- predict(model, type = 'response', droppedTestSet[ ,-10] ))

# decision boundary is set to 0.5,
# The predicted outcome is a 1 if the predicted probability is >0.5 and a 0 if otherwise
(ypred = ifelse(predict > 0.5, 1, 0))
(cm = table(droppedTestSet$stroke, ypred))

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- cm[1,1])
(FN <- cm[1,2])
(FP <- cm[2,1])
(TP <- cm[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

# Calculating Area Under Curve (AUC)
pred = prediction(ypred, droppedTestSet$stroke)
auc = as.numeric(performance(pred, "auc")@y.values)
(auc = round(auc, 3))

# ROC Curve
rocCurve <- performance(pred, "tpr", "fpr")
plot(rocCurve, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "(1 - Specificity)",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

############################ END OF SECTION ONE ###################################

# MODELING - SECTION 2: replacedTrainingSet
## 1.DECISION TREE ALGORITHM FOR replacedTrainingSet
## install.packages("rpart")
## install.packages("rpart.plot")

# Models to work with
replacedTrainingSet
replacedTestSet

library(rpart)
library(rpart.plot)

## Generating trees with Gini Index
(tree1 <- rpart(stroke ~., data=replacedTrainingSet, method="class"))
prp(tree1)
prp(tree1, type = 3)
rpart.plot(tree1, extra = 104, nn = TRUE)
rpart.rules(tree1, cover=TRUE)
rpart.plot(tree1, extra = 101, nn = TRUE)
plotcp(tree1)

# Split with entropy information
entTree1 <- rpart(stroke ~ ., data=replacedTrainingSet, method="class", parms=list(split="information"))
prp(entTree1)
prp (entTree1, type = 3)
rpart.plot(entTree1, extra = 104, nn = TRUE)
plotcp(entTree1)

# Using DT with parameter settings
treeParams = rpart(stroke ~ ., data=replacedTrainingSet, method="class", minsplit = 1, minbucket = 10, cp = -1)
prp (treeParams)
print(treeParams)
summary(treeParams)
plot(treeParams)
text(treeParams)
plotcp(treeParams)

## Predicting and evaluating the performance of the trained tree model 
Predict <- predict(treeParams, replacedTestSet)
Predict

## Redoing the validation data prediction and displaying the class result:
Predict <- predict(treeParams, replacedTestSet, type = "class")
Predict

## Result evaluation
## Creating the confusion matrix
Confusion_matrix <- table(Predict, replacedTestSet$stroke)
print(Confusion_matrix)

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- Confusion_matrix[1,1])
(FN <- Confusion_matrix[1,2])
(FP <- Confusion_matrix[2,1])
(TP <- Confusion_matrix[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

# ROC curve
# install.packages("ROCR")
library(ROCR)

## Predicting the probability values to draw the ROC (receiver operating characteristic) curve.
## Running predict again: Note: PredictROC is the same as Predict with "type = prob", instead of "type = class"
(PredictROC = predict(treeParams, replacedTestSet))

## Keeping only the 2nd column containing probability where stroke = 1:
Predict_ROC2 = PredictROC[,2]

## The prediction is rerun using the probability of stroke = 1:
(pred = prediction(Predict_ROC2, replacedTestSet$stroke))

(perf = performance(pred, "tpr", "fpr"))

# Plotting the ROC curve:
plot(perf, colorize = T)

# Alternative version of the ROC curve with relabeled X and Y axes and (1-Specificity) cut-off markers by increments of 0.3 from range 0 to 1 to indicate the
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "(1 - Specificity)",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

## Calculating the Area Under Curve (AUC):
auc = as.numeric(performance(pred, "auc")@y.values)
(auc = round(auc, 3))


## 2.RANDOM FOREST ALGORITHM FOR replacedTrainingSet
# install.packages("corrplot")
# install.packages("reshape2")
# install.packages("ggthemes")
# install.packages("randomForest")
library(ggplot2)
library(corrplot)
library(reshape2)
library(ggthemes)
library(dplyr)
library(randomForest)

## Models to be used
replacedTrainingSet
replacedTestSet

## Transforming the variables into factors or numeric data types for all above datasets
replacedTrainingSet <- replacedTrainingSet %>%
  mutate(hypertension = as.factor(hypertension))%>%
  mutate(heart_disease = as.factor(heart_disease))%>%
  mutate(stroke = as.factor(stroke)) 

replacedTestSet <- replacedTestSet %>%
  mutate(hypertension = as.factor(hypertension))%>%
  mutate(heart_disease = as.factor(heart_disease))%>%
  mutate(stroke = as.factor(stroke)) 

## Fitting a random forest model
rf_Model <- randomForest(stroke~.,data = replacedTrainingSet, importance = TRUE)
print(rf_Model)
attributes(rf_Model)

## Visualizing the plot
## Option 1
varImpPlot(rf_Model) 

## option 2
importance = importance(rf_Model)
varImportance = data.frame(Variables = row.names(importance), Importance =round(importance[,"MeanDecreaseGini"],2)) 
rankImportance=varImportance%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance))+ 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

## Testing the model with the test set
(rf_Predict1 <- predict(rf_Model, replacedTrainingSet))
(cm <- table(rfPredict1, replacedTrainingSet$stroke))

(rf_Predict2 <- predict(rf_Model, replacedTestSet))
(cm <- table(rfPredict2, replacedTestSet$stroke))

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- cm[1,1])
(FN <- cm[1,2])
(FP <- cm[2,1])
(TP <- cm[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

plot(rf_Model)

## Tuning the RF model for replacedTrainingSet
# Fine tuning parameters of Random Forest model
tuned_Model <- randomForest(stroke ~ ., data = replacedTrainingSet, ntree = 500, mtry = 6, importance = TRUE)
tuned_Model

# Predicting on train set
pred_Train <- predict(tuned_Model, replacedTrainingSet, type = "class")
# Checking classification accuracy
table(pred_Train, replacedTrainingSet$stroke)
(cm <- table(pred_Train, replacedTrainingSet$stroke))

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- cm[1,1])
(FN <- cm[1,2])
(FP <- cm[2,1])
(TP <- cm[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

# Predicting on Test/validation set
pred_Valid <- predict(tuned_Model, replacedTestSet, type = "class")
# Checking classification accuracy
mean(pred_Valid == replacedTestSet$stroke)                    
table(pred_Valid,replacedTestSet$stroke)
(cm <- table(pred_Valid, replacedTestSet$stroke))

## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- cm[1,1])
(FN <- cm[1,2])
(FP <- cm[2,1])
(TP <- cm[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))


# To check important variables for replacedTrainingSet
importance(tuned_Model) 

### Plotting option 1
varImpPlot(tuned_Model)

### Ploting option 2
## option 2
importance = importance(tuned_Model)
varImportance = data.frame(Variables = row.names(importance), 
                           Importance =round(importance[,"MeanDecreaseGini"],2)) # MeanDecreaseAccuracy can be used to substitute this
rankImportance=varImportance%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance))+ 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()


print(tuned_Model)

# Recall that values on the diagonal correspond to true positives and true 
# negatives (correct predictions) whereas the others correspond to false positives 
# and false negatives.

## 3.LINEAR REGRESSION ALGORITHM FOR replacedTrainingSet

## Models to be used
replacedTrainingSet
View(replacedTestSet)


## Model Fitting
lrModel <- glm(stroke~.,family = binomial, data = replacedTrainingSet)
summary(lrModel)

# Predicting the Test set results
(predict <- predict(lrModel, type = 'response', replacedTestSet[ ,-11] ))

# decision boundary is set to 0.5,
# The predicted outcome is a 1 if the predicted probability is >0.5 and a 0 if otherwise
(y_pred = ifelse(predict > 0.5, 1, 0))
(c_m = table(replacedTestSet$stroke, y_pred))


## Computing the True Positives (TP), False Positives (FP), True Negatives (TN) and False Negatives (FN) from the confusion matrix
(TN <- c_m[1,1])
(FN <- c_m[1,2])
(FP <- c_m[2,1])
(TP <- c_m[2,2])

## Calculating the Accuracy, Sensitivity/True Positive Rate (TPR), Specificity and False Positive Rate (FPR):
(Accuracy <- (TP + TN)/(TP + TN + FP + FN))
(Sensitivity <- TP/(TP+FN))
(Specificity <- TN/(TN + FP))
(True_Positive_Rate <- Sensitivity)
(False_Positive_Rate <- 1 - TN/(TN + FP))

# Calculating Area Under Curve (AUC) for droppedTestSet
pred = prediction(ypred, droppedTestSet$stroke)
auc = as.numeric(performance(pred, "auc")@y.values)
(auc = round(auc, 3))

# Calculating Area Under Curve (AUC) for replacedTestSet
pred = prediction(y_pred, replacedTestSet$stroke)
auc = as.numeric(performance(pred, "auc")@y.values)
(auc = round(auc, 3))

# ROC Curve
rocCurve <- performance(pred, "tpr", "fpr")
plot(rocCurve, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "(1 - Specificity)",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

