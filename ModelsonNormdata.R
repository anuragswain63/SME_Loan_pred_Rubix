library(caTools)
library(dplyr)

Final_dataset_Norm_5to95$Default.Flag<-as.factor(Final_dataset_Norm_5to95$Default.Flag)

MissingAge_df<-Final_dataset_Norm_5to95[!is.na(Final_dataset_Norm_5to95$AgeofCompany),]%>%group_by(Industry.group)%>%summarise(missage=sum(AgeofCompany,na.rm = TRUE)/n())
MissingAge_df<-as.data.frame(MissingAge_df)

Final_dataset_Norm_5to95<-merge(Final_dataset_Norm_5to95,MissingAge_df,by.x = c("Industry.group"))%>%arrange(Company.Name)

Final_dataset_Norm_5to95[is.na(Final_dataset_Norm_5to95$AgeofCompany),c("AgeofCompany")]<-Final_dataset_Norm_5to95[is.na(Final_dataset_Norm_5to95$AgeofCompany),c("missage")]

MissingAge_dfbyentity<-Final_dataset_Norm_5to95[!is.na(Final_dataset_Norm_5to95$AgeofCompany),]%>%group_by(Entity.type)%>%summarise(missage_entity=sum(AgeofCompany,na.rm = TRUE)/n())
MissingAge_dfbyentity<-as.data.frame(MissingAge_dfbyentity)

Final_dataset_Norm_5to95<-merge(Final_dataset_Norm_5to95,MissingAge_dfbyentity,by.x = c("Entity.type"))%>%arrange(Company.Name)
Final_dataset_Norm_5to95[is.na(Final_dataset_Norm_5to95$AgeofCompany),c("AgeofCompany")]<-Final_dataset_Norm_5to95[is.na(Final_dataset_Norm_5to95$AgeofCompany),c("missage_entity")]

Final_dataset_Norm_5to95$AgeofCompany<-(Final_dataset_Norm_5to95$AgeofCompany - min(Final_dataset_Norm_5to95$AgeofCompany))/(max(Final_dataset_Norm_5to95$AgeofCompany) - min(Final_dataset_Norm_5to95$AgeofCompany))

Final_dataset_Norm_5to95$NSE_Flag<-as.factor(Final_dataset_Norm_5to95$NSE_Flag)
Final_dataset_Norm_5to95$BSE_Flag<-as.factor(Final_dataset_Norm_5to95$BSE_Flag)
Final_dataset_Norm_5to95$EntityTypePublicFlag<-as.factor(Final_dataset_Norm_5to95$EntityTypePublicFlag)
Final_dataset_Norm_5to95$EntityTypePrivateFlag<-as.factor(Final_dataset_Norm_5to95$EntityTypePrivateFlag)
Final_dataset_Norm_5to95$Ownershipgrp_PrivateForeign<-as.factor(Final_dataset_Norm_5to95$Ownershipgrp_PrivateForeign)
Final_dataset_Norm_5to95$Ownershipgrp_PrivateIndian<-as.factor(Final_dataset_Norm_5to95$Ownershipgrp_PrivateIndian)

Final_Ultra_bkup<-Final_dataset_Norm_5to95

splitprop<-sample.split(Final_dataset_Norm_5to95,SplitRatio = 0.7)
Final_datasetTrain_Norm_5to95<-subset(Final_dataset_Norm_5to95,splitprop==TRUE)
Final_datasetTest_Norm_5to95<-subset(Final_dataset_Norm_5to95,splitprop==FALSE)

############## logistic model ################

Model_logistic<-glm(Default.Flag~CurrentRatio+QuickRatio+InterestRatioCoverage+DebtEquityRatio+DSCR+EBITA+PBT_percentage+InventoryDays+ReceivableDays+PayableDays+ROCE+RONW+FixedAssetsTurnover+AgeofCompany+BSE_Flag+EntityTypePublicFlag+EntityTypePrivateFlag+Ownershipgrp_PrivateForeign+Ownershipgrp_PrivateIndian,family=binomial(link='logit'),data = Final_datasetTrain_Norm_5to95)
summary(Model_logistic)

install.packages("ROCR")
library(ROCR)

PredictedTrain_logistic<-predict(Model_logistic,Final_datasetTrain_Norm_5to95,type = "response")
table(actualvalue=Final_datasetTrain_Norm_5to95$Default.Flag,predictedvalue=PredictedTrain_logistic>0.5)

ROCRpred<-prediction(PredictedTrain_logistic,Final_datasetTrain_Norm_5to95$Default.Flag)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0.5,by=0.1))

PredictedTest_logistic<-predict(Model_logistic,Final_datasetTest_Norm_5to95,type = "response")
table(actualvalue=Final_datasetTest_Norm_5to95$Default.Flag,predictedvalue=PredictedTest_logistic>0.5)



############### decision tree ##########

library(rpart)
library(rpart.plot)

Model_tree<-rpart(Default.Flag~CurrentRatio+QuickRatio+InterestRatioCoverage+DebtEquityRatio+DSCR+EBITA+PBT_percentage+InventoryDays+ReceivableDays+PayableDays+ROCE+RONW+FixedAssetsTurnover+AgeofCompany+BSE_Flag+EntityTypePublicFlag+EntityTypePrivateFlag+Ownershipgrp_PrivateForeign+Ownershipgrp_PrivateIndian,data=Final_datasetTrain_Norm_5to95,method="class",control = rpart.control(minsplit = 2,cp = 0.001))
summary(Model_tree)
plot(Model_tree)
text(Model_tree,cex=0.5)
printcp(Model_tree)
plotcp(Model_tree)

rpart.plot(Model_tree)



################ random forest ###########

library(randomForest)


RF_df_tunerf<-Final_datasetTrain_Norm_5to95[,colnames(Final_datasetTrain_Norm_5to95)%in%c("Default.Flag","CurrentRatio","QuickRatio","InterestRatioCoverage","DebtEquityRatio","DSCR","EBITA","PBT_percentage","InventoryDays","ReceivableDays","PayableDays","ROCE","RONW","FixedAssetsTurnover","AgeofCompany","BSE_Flag","EntityTypePublicFlag","EntityTypePrivateFlag","Ownershipgrp_PrivateForeign","Ownershipgrp_PrivateIndian")]

library(caret)
x<-RF_df_tunerf[,2:20]
y<-RF_df_tunerf[,1] 

############# i wrote 
bestmtry<-tuneRF(RF_df_tunerf[,2:20],RF_df_tunerf[,1],stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

Model_randomforest<-randomForest(Default.Flag~CurrentRatio+QuickRatio+InterestRatioCoverage+DebtEquityRatio+DSCR+EBITA+PBT_percentage+InventoryDays+ReceivableDays+PayableDays+ROCE+RONW+FixedAssetsTurnover+AgeofCompany+BSE_Flag+EntityTypePublicFlag+EntityTypePrivateFlag+Ownershipgrp_PrivateForeign+Ownershipgrp_PrivateIndian,data=Final_datasetTrain_Norm_5to95,mtry=2,ntree=500)

PredictedTrain_randomforest<-predict(Model_randomforest,Final_datasetTrain_Norm_5to95,type="class")

table(actualvalue=Final_datasetTrain_Norm_5to95$Default.Flag,predictedvalue=PredictedTrain_randomforest)


PredictedTest_randomforest<-predict(Model_randomforest,Final_datasetTest_Norm_5to95,type="class")

table(actualvalue=Final_datasetTest_Norm_5to95$Default.Flag,predictedvalue=PredictedTest_randomforest)


############### this got stuck

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Default.Flag~CurrentRatio+QuickRatio+InterestRatioCoverage+DebtEquityRatio+DSCR+EBITA+PBT_percentage+InventoryDays+ReceivableDays+PayableDays+ROCE+RONW+FixedAssetsTurnover+AgeofCompany+BSE_Flag+EntityTypePublicFlag+EntityTypePrivateFlag+Ownershipgrp_PrivateForeign+Ownershipgrp_PrivateIndian, data=RF_df_tunerf, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Default.Flag~CurrentRatio+QuickRatio+InterestRatioCoverage+DebtEquityRatio+DSCR+EBITA+PBT_percentage+InventoryDays+ReceivableDays+PayableDays+ROCE+RONW+FixedAssetsTurnover+AgeofCompany+BSE_Flag+EntityTypePublicFlag+EntityTypePrivateFlag+Ownershipgrp_PrivateForeign+Ownershipgrp_PrivateIndian, data=RF_df_tunerf, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


##   rmarkdown::render('SME Loan Documentation.Rmd')   to get as html document
################### svm ###############


