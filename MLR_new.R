library(nnet)
library(ROCR)
T_data<-training_MLR_general
C_data<-testing_MLR_general

mymodel = multinom(class ~ bus_cycles + branch_instructions	+ cache_references	+ node_loads	+ branch_misses	+ node_stores	+ cache_misses	+ instructions	+ L1_icache_load_misses	+ branch_loads	+ LLC_load_misses	+ L1_dcache_loads	+ LLC_loads	+ L1_dcache_stores	+ L1_dcache_load_misses	+ iTLB_load_misses, data = mydata)

cm <- table(predict(mymodel,mydata),mydata$class)