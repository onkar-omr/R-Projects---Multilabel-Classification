library("e1071")
library(ROCR)
library(pROC)
library(nnet)
T_data<- training_data
C_data<- testing_data
#fitdefaultradial <- svm(class~.,data = T_data,kernel= "linear",type = "C-classification")
fitdefaultradial <- svm(class~branch_instructions	+ cache_references + branch_misses	+ node_stores,data = T_data,kernel= "linear",type = "C-classification")
#library(caret)
predSVMLinear2<-predict(fitdefaultradial,C_data,type="class")
#pred <- prediction(predSVMLinear2,C_data$class)
predictions <- as.numeric(predSVMLinear2)
roc.multi <- multiclass.roc(C_data$class,predictions)
rs <- roc.multi[['rocs']]
#AUC : SVM
auc(roc.multi)
sapply(1:length(rs),function(i) lines.roc(rs[[i]],col=i))
plot.roc(rs[[1]],legacy.axes = "TRUE")
tab <- table(predSVMLinear2,C_data$class)
tab
#Misclassification Error : SVM
sum(diag(tab))/sum(tab)

#mymodel = multinom(class ~ bus_cycles + branch_instructions	+ cache_references	+ node_loads	+ branch_misses	+ node_stores	+ cache_misses	+ instructions	+ L1_icache_load_misses	+ branch_loads	+ LLC_load_misses	+ L1_dcache_loads	+ LLC_loads	+ L1_dcache_stores	+ L1_dcache_load_misses	+ iTLB_load_misses, data = T_data)
mymodel = multinom(class ~ branch_instructions	+ cache_references + branch_misses	+ node_stores, data = T_data)
pred2 <- predict(mymodel,C_data)
predictions2 <- as.numeric(pred2)
roc.multi2 <- multiclass.roc(C_data$class,predictions2)
rs2 <- roc.multi2[['rocs']]
plot.roc(rs2[[1]],legacy.axes = "TRUE")
#sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
auc(roc.multi2)
sapply(1:length(rs2),function(i) lines.roc(rs2[[i]],col=i))
#Misclassification error
cm <- table(pred2,C_data$class)
print(cm)
sum(diag(cm))/sum(cm)



#PLOT
#plot.roc(rs2[[5]],legacy.axes = "TRUE",col = "purple" , lty = 3,print.auc = TRUE,grid = 0.2,grid.lty = 3, grid.lwd = 2, identity = FALSE, xlab = "False positive rate", ylab = "True positive rate",print.auc.x =0.2, print.auc.y = 0.4, legend = TRUE,print.auc.cex = 1.2)
plot.roc(rs2[[5]],legacy.axes = "TRUE",col = "purple" , lty = 3,grid = 0.2,grid.lty = 3, grid.lwd = 2, identity = FALSE, xlab = "False positive rate", ylab = "True positive rate",legend = TRUE)
plot.roc(rs[[5]],legacy.axes = "TRUE",col = "green" , lty = 2,identity = FALSE,add=TRUE)
legend("bottomright",c("MLR (AUC = 0.690)","SVM (AUC = 0.7125)"),lty =2,col = c("purple","green"),cex = 1.4, lwd = 3:2, title = "ML Algorithm")
