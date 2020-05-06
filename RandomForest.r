#Applying random forests model

#Training The Model
set.seed(123)
library(randomForest)
pima$Diabetes <- as.factor(pima$Diabetes)

rf_pima <- randomForest(Diabetes ~., data = pima_training, mtry = 8, ntree=50, importance = TRUE)

# Testing the Model
rf_probs <- predict(rf_pima, newdata = pima_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)
rf_pred
pima_testing$Diabetes

a<-head(rf_pred,200)
b<-head(pima_testing$Diabetes,200)

#ConfusionMatrix

rf_probs <- predict(rf_pima, newdata = pima_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)
confusionMatrix(rf_pred, pima_testing$Diabetes )
table(rf_pred, pima_testing$Diabetes)

131+36+26+38
#Accuracy
acc_rf=(131+38)/231
acc_rf

#PLOT
importance(rf_pima)
par(mfrow = c(1, 2))
varImpPlot(rf_pima, type = 2, main = "Variable Importance",col = 'black')
plot(rf_pima, main = "Error vs No. of trees grown")

acc_rf_pima <- confusionMatrix(a, b)$overall['Accuracy']
