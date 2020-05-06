#APPLYING LOGISTIC REGRESSION MODEL
#we use a training data set containing a random sample of 70% of the observation to perform a Logistic Regression with "Diabetes" as the response and the remains variables as predictors.

# Preparing the DataSet
set.seed(123)
n <- nrow(pima)
train <- sample(n, trunc(0.70*n))
pima_training <- pima[train, ]
pima_testing <- pima[-train, ]

# Training The Model
glm_fm1 <- glm(Diabetes ~., data = pima_training, family = binomial)
summary(glm_fm1)

#The result shows that the variables Triceps_Skin, Serum_Insulin and Age are not statiscally significance. In other words, the p_values is greather than 0.01. Therefore they will be removed.
#Update to use only the significant variables
glm_fm2 <- update(glm_fm1, ~. - Triceps_Skin - Serum_Insulin - Age )
summary(glm_fm2)

# Testing the Model
glm_probs <- predict(glm_fm2, newdata = pima_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
#print("Confusion Matrix for logistic regression"); 
table(Predicted = glm_pred, Actual = pima_testing$Diabetes)

install.packages("caTools")
library(caTools)
set.seed(88)
split = sample.split(pima_testing$Diabetes, SplitRatio = 0.75)
split

# Create training and testing sets
qualityTrain = subset(pima_testing, split == TRUE)
qualityTest = subset(pima_testing, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)
table(pima_testing$Diabetes, glm_probs > 0.5)
#sensitivity
37/74
#specificity
137/157
table(pima_testing$Diabetes, glm_probs > 0.7)
table(pima_testing$Diabetes, glm_probs > 0.2)

#We see that by increasing the threshold value, the model's 
#sensitivity decreases and specificity increases while the reverse happens 
#if the threshold value is decreased. So how to choose the optimum threshold value. 
#Picking a good threshold value is often challenging. 
#A Receiver Operator Characteristic curve, or ROC curve, 
#can help us decide which value of the threshold is best.

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

ROCRpred = prediction(glm_pred, pima_testing$Diabetes)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
#ConfusionMatrix
table(pima_testing$Diabetes, glm_probs >= 0.3)
#Accuracy
acc_lrm=(116+52)/231
acc_lrm
