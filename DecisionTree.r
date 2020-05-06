#Applying Decision Tree
pima <- read.csv(file.path("E:", "Diabetes.csv"), col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
pima$Diabetes <- as.factor(pima$Diabetes)
library(caret)
library(tree)
library(e1071)

set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

# Training The Model
treemod <- tree(Diabetes ~ ., data = train)

summary(treemod)
treemod # get a detailed text output.
#The results display the split criterion (e.g. Plasma_Glucose < 123.5), 
#the number of observations in that branch, the deviance, the overall prediction for 
#the branch (Yes or No), and the fraction of observations in that branch that take on 
#values of Yes and No. Branches that lead to terminal nodes are indicated using 
#asterisks.
#Now we plot of the tree, and interpret the results.
plot(treemod)
text(treemod, pretty = 0)
# Testing the Model
tree_pred <- predict(treemod, newdata = test, type = "class" )
confusionMatrix(tree_pred, test$Diabetes)
acc_treemod <- confusionMatrix(tree_pred, test$Diabetes)$overall['Accuracy']
acc_treemod
