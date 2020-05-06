accuracy <- data.frame(Model=c("Logistic Regression Model","Random Forest","Decision Tree","Support Vector Machine (SVM)"), Accuracy=c(acc_lrm,acc_rf,
acc_treemod,acc_svm_model ))
ggplot(accuracy,aes(x=Model,y=Accuracy)) + geom_bar(stat='identity') + theme_bw() + ggtitle('Comparison of Model Accuracy')
