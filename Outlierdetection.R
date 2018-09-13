library(ggplot2)
library(scales)

# Outlier_currentratio<-boxplot.stats(Final_dataset_meanimpute$CurrentRatio)$out
# ggplot(Final_dataset_meanimpute[!Final_dataset_meanimpute$CurrentRatio%in%Outlier_currentratio,],aes(x="",y=CurrentRatio))+geom_boxplot()
# 
# 
# ggplot(Final_dataset_meanimpute,aes(CurrentRatio))+geom_histogram(bins=30)
# 
# ggplot(Final_dataset_meanimpute[!Final_dataset_meanimpute$CurrentRatio%in%Outlier_currentratio,],aes(CurrentRatio))+geom_histogram(bins = 30)
# 
# ggplot(Final_dataset_meanimpute,aes(QuickRatio))+geom_histogram(bins = 30)

###function for capping outliers##### final_dataset_meanimpute has data before capping
#Final_dataset_meanimpute

## putting in final dataset mean impute
cappingfunc<-function(y){

  for (i in 57:69)
  {
    quantiles <- quantile( y[,i], c(.05, .95 ), na.rm =TRUE)
    y[,i] = ifelse(y[,i] < quantiles[1] , quantiles[1], y[,i])
    y[,i] = ifelse(y[,i] > quantiles[2] , quantiles[2], y[,i])
  }
  y
}


##backup for the data capped but stiil having issues with payable and inventory

##important bkup for Final_dataset_capped_5to95perc_bkup   this has data with inventory daya and payable days as infinity which was in Final_dataset_capped_5to95perc

## putting in finaldataset_capped
# cappingfunc_2<-function(x){
#   
#   for (i in c(64,66))
#   {
#     quantiles <- quantile( x[,i], c(.25, .75 ), na.rm =TRUE)
#     x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
#     x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])
#   }
#   x
# }




Final_dataset_capped_5to95perc<-cappingfunc(Final_dataset_meanimpute)

#Final_dataset_capped_5to95perc<-cappingfunc_2(Final_dataset_capped_5to95perc)



###### after capping 
ggplot(Final_dataset_capped_5to95perc,aes(CurrentRatio))+geom_histogram(bins=30)
ggplot(Final_dataset_capped_5to95perc,aes(InventoryDays))+geom_histogram(bins=30)
ggplot(Final_dataset_capped_5to95perc,aes(ReceivableDays))+geom_histogram(bins=30)
ggplot(Final_dataset_capped_5to95perc,aes(PayableDays))+geom_histogram(bins = 30)
ggplot(Final_dataset_capped_5to95perc,aes(QuickRatio))+geom_histogram(bins = 30)
ggplot(Final_dataset_capped_5to95perc,aes(DSCR))+geom_histogram(bins=30)
ggplot(Final_dataset_capped_5to95perc,aes(DebtEquityRatio))+geom_histogram(bins = 30)
ggplot(Final_dataset_capped_5to95perc,aes(EBITA))+geom_histogram(bins = 30)
ggplot(Final_dataset_capped_5to95perc,aes(PBT_percentage))+geom_histogram(bins = 30)
ggplot(Final_dataset_capped_5to95perc,aes(ROCE))+geom_histogram(bins = 30)
ggplot(Final_dataset_capped_5to95perc,aes(RONW))+geom_histogram(bins = 30)


  
###### before capping plots#####



ggplot(Final_dataset_bkup,aes(ROCE))+geom_histogram(bins = 30)+scale_x_continuous(limits = c(-0.5,1))
ggplot(Final_dataset_bkup,aes(CurrentRatio))+geom_histogram(bins = 30)+scale_x_continuous(limits = c(-5,50))
ggplot(Final_dataset_bkup,aes(InventoryDays))+geom_histogram(bins = 30)+scale_x_continuous(limits = c(-10,3000))
ggplot(Final_dataset_bkup,aes(ReceivableDays))+geom_histogram(bins = 30)+scale_x_continuous(limits = c(-10,3000))
ggplot(Final_dataset_bkup,aes(PayableDays))+geom_histogram(bins = 30)+scale_x_continuous(limits = c(-10,3000))
ggplot(Final_dataset_bkup,aes(QuickRatio))+geom_histogram(bins = 30)+scale_x_continuous(limits = c(0,25))


#nrow(Final_dataset_capped_5to95perc[is.finite(Final_dataset_capped_5to95perc$InventoryDays)&is.finite(Final_dataset_capped_5to95perc$PayableDays),])
#nrow(Final_dataset_capped_5to95perc[is.infinite(Final_dataset_capped_5to95perc$InventoryDays)|is.infinite(Final_dataset_capped_5to95perc$PayableDays),])

#Final_dataset_capped_5to95perc$ReceivableDays_Normminmax<-(Final_dataset_capped_5to95perc$ReceivableDays - min(Final_dataset_capped_5to95perc$ReceivableDays))/(max(Final_dataset_capped_5to95perc$ReceivableDays) - min(Final_dataset_capped_5to95perc$ReceivableDays))
#Final_dataset_capped_5to95perc$FixedAssetsTurnover_Normminmax<-(Final_dataset_capped_5to95perc$FixedAssetsTurnover - min(Final_dataset_capped_5to95perc$FixedAssetsTurnover))/(max(Final_dataset_capped_5to95perc$ReceivableDays) - min(Final_dataset_capped_5to95perc$ReceivableDays))

##goin to use this Final_dataset_Norm_5to95 from now onwards
Final_dataset_Norm_5to95<-Final_dataset_capped_5to95perc


# normalizefunc <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }

#Final_dataset_Norm_5to95<-as.data.frame(lapply(Final_dataset_Norm_5to95[60:62],normalizefunc))

#### ratios are normalised here


Final_dataset_Norm_5to95<-Final_dataset_Norm_5to95[!is.na(Final_dataset_Norm_5to95$InterestRatioCoverage),]

# summary(Final_dataset_Norm_5to95[is.finite(Final_dataset_Norm_5to95$PayableDays),c("PayableDays")])
# summary(Final_dataset_Norm_5to95[is.finite(Final_dataset_Norm_5to95$InventoryDays),c("InventoryDays")])

#sd(Final_dataset_Norm_5to95[is.finite(Final_dataset_Norm_5to95$PayableDays),c("PayableDays")])






Final_dataset_Norm_5to95$CurrentRatio<-(Final_dataset_Norm_5to95$CurrentRatio - min(Final_dataset_Norm_5to95$CurrentRatio))/(max(Final_dataset_Norm_5to95$CurrentRatio) - min(Final_dataset_Norm_5to95$CurrentRatio))
Final_dataset_Norm_5to95$QuickRatio<-(Final_dataset_Norm_5to95$QuickRatio - min(Final_dataset_Norm_5to95$QuickRatio))/(max(Final_dataset_Norm_5to95$QuickRatio) - min(Final_dataset_Norm_5to95$QuickRatio))
Final_dataset_Norm_5to95$DebtEquityRatio<-(Final_dataset_Norm_5to95$DebtEquityRatio - min(Final_dataset_Norm_5to95$DebtEquityRatio))/(max(Final_dataset_Norm_5to95$DebtEquityRatio) - min(Final_dataset_Norm_5to95$DebtEquityRatio))
Final_dataset_Norm_5to95$DSCR<-(Final_dataset_Norm_5to95$DSCR - min(Final_dataset_Norm_5to95$DSCR))/(max(Final_dataset_Norm_5to95$DSCR) - min(Final_dataset_Norm_5to95$DSCR))
Final_dataset_Norm_5to95$EBITA<-(Final_dataset_Norm_5to95$EBITA - min(Final_dataset_Norm_5to95$EBITA))/(max(Final_dataset_Norm_5to95$EBITA) - min(Final_dataset_Norm_5to95$EBITA))
Final_dataset_Norm_5to95$PBT_percentage<-(Final_dataset_Norm_5to95$PBT_percentage - min(Final_dataset_Norm_5to95$PBT_percentage))/(max(Final_dataset_Norm_5to95$PBT_percentage) - min(Final_dataset_Norm_5to95$PBT_percentage))
Final_dataset_Norm_5to95$ReceivableDays<-(Final_dataset_Norm_5to95$ReceivableDays - min(Final_dataset_Norm_5to95$ReceivableDays))/(max(Final_dataset_Norm_5to95$ReceivableDays) - min(Final_dataset_Norm_5to95$ReceivableDays))
Final_dataset_Norm_5to95$ROCE<-(Final_dataset_Norm_5to95$ROCE - min(Final_dataset_Norm_5to95$ROCE))/(max(Final_dataset_Norm_5to95$ROCE) - min(Final_dataset_Norm_5to95$ROCE))
Final_dataset_Norm_5to95$RONW<-(Final_dataset_Norm_5to95$RONW - min(Final_dataset_Norm_5to95$RONW))/(max(Final_dataset_Norm_5to95$RONW) - min(Final_dataset_Norm_5to95$RONW))
Final_dataset_Norm_5to95$InterestRatioCoverage<-(Final_dataset_Norm_5to95$InterestRatioCoverage - min(Final_dataset_Norm_5to95$InterestRatioCoverage))/(max(Final_dataset_Norm_5to95$InterestRatioCoverage) - min(Final_dataset_Norm_5to95$InterestRatioCoverage))
Final_dataset_Norm_5to95$PayableDays<-(Final_dataset_Norm_5to95$PayableDays - min(Final_dataset_Norm_5to95$PayableDays))/(max(Final_dataset_Norm_5to95$PayableDays) - min(Final_dataset_Norm_5to95$PayableDays))
Final_dataset_Norm_5to95$InventoryDays<-(Final_dataset_Norm_5to95$InventoryDays - min(Final_dataset_Norm_5to95$InventoryDays))/(max(Final_dataset_Norm_5to95$InventoryDays) - min(Final_dataset_Norm_5to95$InventoryDays))
Final_dataset_Norm_5to95$FixedAssetsTurnover<-(Final_dataset_Norm_5to95$FixedAssetsTurnover - min(Final_dataset_Norm_5to95$FixedAssetsTurnover))/(max(Final_dataset_Norm_5to95$FixedAssetsTurnover) - min(Final_dataset_Norm_5to95$FixedAssetsTurnover))



ggplot(Final_dataset_Norm_5to95,aes(CurrentRatio))+geom_histogram(bins=30)
ggplot(Final_dataset_Norm_5to95,aes(QuickRatio))+geom_histogram(bins=30)
ggplot(Final_dataset_Norm_5to95,aes(ReceivableDays))+geom_histogram(bins=30)
ggplot(Final_dataset_Norm_5to95,aes(DSCR))+geom_histogram(bins = 30)
ggplot(Final_dataset_Norm_5to95,aes(DebtEquityRatio))+geom_histogram(bins = 30)
ggplot(Final_dataset_Norm_5to95,aes(EBITA))+geom_histogram(bins = 30)
ggplot(Final_dataset_Norm_5to95,aes(PBT_percentage))+geom_histogram(bins = 30)
ggplot(Final_dataset_Norm_5to95,aes(ROCE))+geom_histogram(bins = 30)
ggplot(Final_dataset_Norm_5to95,aes(RONW))+geom_histogram(bins = 30)
ggplot(Final_dataset_Norm_5to95,aes(InventoryDays))+geom_histogram(bins=30)
ggplot(Final_dataset_Norm_5to95,aes(PayableDays))+geom_histogram(bins=30)
ggplot(Final_dataset_Norm_5to95,aes(AgeofCompany))+geom_histogram(bins=30)


## this has the normalised values but issue with age of comapny
Final_Ultra_bkup<-Final_dataset_Norm_5to95