#mydata<-testing_MLR
T_data<-training_data
C_data<-testing_data
#mydata2<- training_MLR
#mydata$NSPF <- as.factor(mydata$class)
#mydata$out <- relevel(mydata$NSPF,ref = "1")
library(nnet)
library(pROC)


### All 16 HPc's
mymodel = multinom(class ~ bus_cycles + branch_instructions	+ cache_references	+ node_loads	+ branch_misses	+ node_stores	+ cache_misses	+ instructions	+ L1_icache_load_misses	+ branch_loads	+ LLC_load_misses	+ L1_dcache_loads	+ LLC_loads	+ L1_dcache_stores	+ L1_dcache_load_misses	+ iTLB_load_misses, data = T_data)
###Four Common HPC's
#mymodel = multinom(class ~ branch_instructions	+ cache_references + branch_misses	+ node_stores, data = mydata)
####Backdoor
#mymodel = multinom(class ~ branch_instructions	+ cache_references + branch_misses	+ node_stores	+ L1_icache_load_misses	+ branch_loads	+ LLC_load_misses	+ iTLB_load_misses, data = mydata)
###Trojan
#mymodel = multinom(class ~ branch_instructions	+ cache_references + branch_misses	+ node_stores	+ cache_misses + L1_icache_load_misses	+ LLC_load_misses	+ iTLB_load_misses, data = mydata)
###Virus
#mymodel = multinom(class ~ branch_instructions	+ cache_references + branch_misses	+ node_stores	+ LLC_load_misses	+ L1_dcache_loads	+ L1_dcache_stores	+ iTLB_load_misses, data = mydata)
###Rootkit
#mymodel = multinom(class ~ branch_instructions	+ cache_references + branch_misses	+ node_stores	+ cache_misses	+ branch_loads	+ LLC_load_misses	+ L1_dcache_stores, data = mydata)
###Worm
#mymodel = multinom(class ~ branch_instructions	+ cache_references + branch_misses	+ node_stores	+ cache_misses	+ L1_dcache_loads	+ L1_dcache_load_misses	+ iTLB_load_misses, data = mydata)

summary(mymodel)
#head(pp<-fitted(mymodel))
#library(dplyr)
#summarise(pp,Average = mean(backdoor,na.rm = T))
#z<-with(pp,mean(worm))
#duration = pp$backdoor
#mean(duration)
#write.csv(pp,"write_4common_Test_Full.csv")


#predict(mymodel,mydata2)
#library(xlsx)
#write.xlsx(pp, "/home/research/Documents/mydata.xlsx")
#z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
#p <- (1 - pnorm(abs(z), 0, 1)) * 2
#p
pred <- predict(mymodel,C_data)
predictions2 <- as.numeric(pred)
roc.multi2 <- multiclass.roc(C_data$class,predictions2)
rs2 <- roc.multi2[['rocs']]
plot.roc(rs2[[1]])
#sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
auc(roc.multi2)
sapply(2:length(rs2),function(i) lines.roc(rs2[[i]],col=i))
#Misclassification error
cm <- table(predict(mymodel,mydata),mydata$class)
print(cm)
sum(diag(cm))/sum(cm)
