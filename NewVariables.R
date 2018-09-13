Final_dataset_meanimpute$PublicFlag
Final_dataset_meanimpute$PrivateFlag

Final_dataset_meanimpute[Final_dataset_meanimpute$Entity.type=="Public Ltd.",c("PublicFlag")]<-1
Final_dataset_meanimpute[Final_dataset_meanimpute$Entity.type!="Public Ltd.",c("PublicFlag")]<-0

Final_dataset_meanimpute[Final_dataset_meanimpute$Entity.type=="Private Ltd.",c("PrivateFlag")]<-1
Final_dataset_meanimpute[Final_dataset_meanimpute$Entity.type!="Private Ltd.",c("PrivateFlag")]<-0

colnames(Final_dataset_meanimpute)[colnames(Final_dataset_meanimpute)=="PublicFlag"] <- "EntityTypePublicFlag"
colnames(Final_dataset_meanimpute)[colnames(Final_dataset_meanimpute)=="PrivateFlag"] <- "EntityTypePrivateFlag"


##age of company
Final_dataset_meanimpute$AgeofCompany<-2018-Final_dataset_meanimpute$Incorporation.year


Final_dataset_meanimpute$Ownershipgrp_PrivateIndian
Final_dataset_meanimpute$Ownershipgrp_PrivateForeign

Final_dataset_meanimpute[Final_dataset_meanimpute$Ownership.group=="Private (Indian)",c("Ownershipgrp_PrivateIndian")]<-1
Final_dataset_meanimpute[Final_dataset_meanimpute$Ownership.group!="Private (Indian)",c("Ownershipgrp_PrivateIndian")]<-0


Final_dataset_meanimpute[Final_dataset_meanimpute$Ownership.group=="Private (Foreign)",c("Ownershipgrp_PrivateForeign")]<-1
Final_dataset_meanimpute[Final_dataset_meanimpute$Ownership.group!="Private (Foreign)",c("Ownershipgrp_PrivateForeign")]<-0

##where to put central govt and state govt in??
## tata grp and larsen turbo are put under private indian
##classifying others is still remaining

Final_dataset_meanimpute[Final_dataset_meanimpute$Ownership.group=="Tata Group",c("Ownershipgrp_PrivateIndian")]<-1
Final_dataset_meanimpute[Final_dataset_meanimpute$Ownership.group=="Larsen & Toubro Group",c("Ownershipgrp_PrivateIndian")]<-1


Final_dataset_meanimpute$NSE_Flag
Final_dataset_meanimpute$BSE_Flag

Final_dataset_meanimpute[Final_dataset_meanimpute$NSE.symbol=="",c("NSE_Flag")]<-0
Final_dataset_meanimpute[Final_dataset_meanimpute$NSE.symbol!="",c("NSE_Flag")]<-1

## bseflag is 1 if bse script is not NA 
Final_dataset_meanimpute[!is.na(Final_dataset_meanimpute$BSE.scrip.code),c("BSE_Flag")]<-1
Final_dataset_meanimpute[is.na(Final_dataset_meanimpute$BSE.scrip.code),c("BSE_Flag")]<-0


######### removing the one remaing na in interest expense

Final_dataset_meanimpute<-Final_dataset_meanimpute[!is.na(Final_dataset_meanimpute$Interest.expense),]


###ratios###

Final_dataset_meanimpute$CurrentRatio<-Final_dataset_meanimpute$Current.assets..incl..short.term.investments..loans...advances./Final_dataset_meanimpute$Current.liabilities...provisions

Final_dataset_meanimpute$QuickRatio<-(Final_dataset_meanimpute$Current.assets..incl..short.term.investments..loans...advances. - Final_dataset_meanimpute$Short.term.inventories)/Final_dataset_meanimpute$Current.liabilities...provisions

Final_dataset_meanimpute$InterestRatioCoverage<-Final_dataset_meanimpute$PBDITA/Final_dataset_meanimpute$Interest.expense

Final_dataset_meanimpute$DebtEquityRatio<-(Final_dataset_meanimpute$Short.term.borrowings+Final_dataset_meanimpute$Non.current.liabilities)/Final_dataset_meanimpute$Net.worth

Final_dataset_meanimpute$DSCR<-Final_dataset_meanimpute$PBDITA/(Final_dataset_meanimpute$Interest.expense+Final_dataset_meanimpute$Short.term.borrowings)

Final_dataset_meanimpute$EBITA<-Final_dataset_meanimpute$PBDITA/Final_dataset_meanimpute$Sales

Final_dataset_meanimpute$PBT_percentage<-Final_dataset_meanimpute$PBT/Final_dataset_meanimpute$Sales

Final_dataset_meanimpute$InventoryDays<-(Final_dataset_meanimpute$Short.term.inventories*365)/Final_dataset_meanimpute$Sales

Final_dataset_meanimpute$ReceivableDays<-(Final_dataset_meanimpute$Short.term.trade.receivables...bills.receivable*365)/Final_dataset_meanimpute$Sales

Final_dataset_meanimpute$PayableDays<-(Final_dataset_meanimpute$Short.term.trade.payables.and.acceptances*365)/Final_dataset_meanimpute$Sales

Final_dataset_meanimpute$ROCE<-Final_dataset_meanimpute$PBDITA/Final_dataset_meanimpute$Capital.employed

Final_dataset_meanimpute$RONW<-Final_dataset_meanimpute$Profit.after.tax.reported.by.company/Final_dataset_meanimpute$Net.worth

Final_dataset_meanimpute$FixedAssetsTurnover<-Final_dataset_meanimpute$Sales/Final_dataset_meanimpute$Net.fixed.assets

#############################################



Final_dataset_meanimpute$Industry.type<-as.factor(Final_dataset_meanimpute$Industry.type)
Final_dataset_meanimpute$BSE_Flag<-as.factor(Final_dataset_meanimpute$BSE_Flag)
Final_dataset_meanimpute$NSE_Flag<-as.factor(Final_dataset_meanimpute$NSE_Flag)
Final_dataset_meanimpute$Ownershipgrp_PrivateIndian<-as.factor(Final_dataset_meanimpute$Ownershipgrp_PrivateIndian)
Final_dataset_meanimpute$Ownershipgrp_PrivateForeign<-as.factor(Final_dataset_meanimpute$Ownershipgrp_PrivateForeign)
Final_dataset_meanimpute$EntityTypePrivateFlag<-as.factor(Final_dataset_meanimpute$EntityTypePrivateFlag)
Final_dataset_meanimpute$EntityTypePublicFlag<-as.factor(Final_dataset_meanimpute$EntityTypePublicFlag)



library(caret)
NumericCol<-Final_dataset_meanimpute[sapply(Final_dataset_meanimpute,is.numeric)]
#columnsCor<-cor(NumericCol)  too many variables henmce plot now showing
#HighCor<-findCorrelation(columnsCor,cutoff =0.7)


##getting NAs in Interest.expense,Premium.discount.on.issue.of.debt.instruments,Other.fund.based.financial.services.expenses,Treasury.operations.expenses,Fund.based.financial.services.expenses,Fee.based.financial.services.expenses,Ratings,AgeofCompany,InterestRatioCoverage,DSCR
columnsCor<-cor(NumericCol[,colnames(NumericCol)%in%c("Total.liabilities","Total.capital","Non.current.liabilities","Current.liabilities...provisions","Short.term.borrowings","Short.term.trade.payables.and.acceptances","Net.worth","Capital.employed","Net.fixed.assets","Current.assets..incl..short.term.investments..loans...advances.","Short.term.inventories","PBDITA","Interest.expense","Sales","PBT","Short.term.trade.receivables...bills.receivable")])
columnsCor_new<-cor(NumericCol[,colnames(NumericCol)%in%c("Total.liabilities","Total.capital","Non.current.liabilities","Current.liabilities...provisions","Short.term.borrowings","Short.term.trade.payables.and.acceptances","Net.worth","Capital.employed","Net.fixed.assets","Current.assets..incl..short.term.investments..loans...advances.","Short.term.inventories","PBDITA","Interest.expense","Sales","PBT","Short.term.trade.receivables...bills.receivable","CurrentRatio","QuickRatio","InterestRatioCoverage","DebtEquityRatio","DSCR","EBITA","PBT_percentage","InventoryDays","ReceivableDays","PayableDays","ROCE","RONW","FixedAssetsTurnover")])

install.packages("corrplot")
library(corrplot)
corrplot(columnsCor_new,method = "color",tl.cex = 0.5)
corrplot(columnsCor,method = "color",tl.cex = 0.5)