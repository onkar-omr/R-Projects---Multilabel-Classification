library("e1071")
library(ROCR)
library(pROC)
T_data<- training_data
C_data<-testing_data
#T_data<-training_MLR_general
#C_data<-testing_MLR_general
#T_data<-training_data_num
#C_data<-testing_data_num

#dim(T_data)
#str(T_data)

summary(T_data)
summary(C_data)

fitdefaultradial <- svm(class~.,data = T_data,kernel= "linear",type = "C-classification")
#fitdefaultradial2 <- svm(class ~ branch_instructions	+ cache_references + branch_misses	+ node_stores,data = T_data,type = "C-classification")
#fitdefaultpoly <- svm(class~.,data = T_data,kernel="linear")
summary(fitdefaultradial)
#plot(fitdefaultradial,T_data)
library(caret)
predSVMLinear<-predict(fitdefaultradial,C_data,type="class")
predSVMLinear2<-predict(fitdefaultradial,C_data,type="prob")
#cor(C_data$class,predSVMLinear)^2

#plot(predSVMLinear,data=C_data)
tab <- table(predSVMLinear,C_data$class)
tab
sum(diag(tab))/sum(tab)
#pred <- prediction(predSVMLinear2,C_data$class)
#roc <- performance(pred,"tpr","fpr")
predictions <- as.numeric(predSVMLinear)
roc.multi <- multiclass.roc(C_data$class,predictions)
auc(roc.multi)
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))



