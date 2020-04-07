data<-read.csv('D:/Ivy/Files/projects/CCFraud/creditcard.csv')
dim(data)
str(data)
prop.table(table(data$Class))
sum(is.na(data))
#new<-scale(data[,1:28],center = TRUE, scale = TRUE)
data<-data[,-c(1)]
data$Amount<-scale(data$Amount,center = TRUE, scale = TRUE)
library(dplyr)
library(caret)
index<-createDataPartition(data$Class,p=0.75,list=FALSE)
train<-data[,index]
test<-data[,-index]
#Fitting a logistic model using caret or glm()
#Predict using test data
library(pROC)
lr.predict <- predict(Logistic_Model,test, probability = TRUE)
auc.gbm = roc(test_data$Class, lr.predict, plot = TRUE, col = "blue")
#implement with Decision tree, Random Forest, Naive Bayes, GBM
library(gbm, quietly=TRUE)
# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train_data, test_data)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
)
)
# Determine best iteration based on test data
gbm.iter = gbm.perf(model_gbm, method = "test")
model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE)
#Plot the gbm model
plot(model_gbm)
# Plot and calculate AUC on test data
gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)
gbm_auc = roc(test_data$Class, gbm_test, plot = TRUE, col = "red")
print(gbm_auc)
#Random Forest using randomForest() and apply vif()
#Ensemble technique
#Spot-Check and select the best model<Jason Brownlee>