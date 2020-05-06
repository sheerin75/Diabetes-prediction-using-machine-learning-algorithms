#Applying Support Vector Machine
pima <- read.csv(file.path("E:", "Diabetes.csv"), col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
pima$Diabetes <- as.factor(pima$Diabetes)
library(e1071)

#Preparing the DataSet:
set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

tuned <- tune.svm(Diabetes ~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned) # to show the results

#Training The Model: In order to build a svm model to predict "Diabetes" using Cost=10 and gamma=0.01, which were the best values according the tune() function performed before.
svm_model  <- svm(Diabetes ~., data = train, kernel = "radial", gamma = 0.01, cost = 10) 
summary(svm_model)

#Testing the Model:
svm_pred <- predict(svm_model, newdata = test)
confusionMatrix(svm_pred, test$Diabetes)

acc_svm_model <- confusionMatrix(svm_pred, test$Diabetes)$overall['Accuracy']
acc_svm_model
