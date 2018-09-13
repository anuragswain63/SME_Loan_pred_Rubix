## start from totalliabilities upto profit tax reported by company

## columns for imputation is from 16:46    ##31 columns total

## for imputing first  i will consider  Main.product.service.group , NIC.name ,Industry.group,Ownership.group,Entity.type
## i guess first we should go with the more specific groups

## BUT RIGHT NOW IAM DOING IT WITH Industry.group and Entity.type

Final_dataset_meanimpute<-Final_dataset
Final_dataset_bkup<-Final_dataset

Columns_forImpute<-colnames(Final_dataset_meanimpute[,c(16:46)])



library(zoo)

# for(j in 1:31)
# {
#   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Columns_forImpute[j]=na.aggregate(Columns_forImpute[j],by =Main.product.service.group))
#   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Columns_forImpute[j]=na.aggregate(Columns_forImpute[j],by =NIC.name))
#   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Columns_forImpute[j]=na.aggregate(Columns_forImpute[j],by =Industry.group))
#   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Columns_forImpute[j]=na.aggregate(Columns_forImpute[j],by =Ownership.group))
#   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Columns_forImpute[j]=na.aggregate(Columns_forImpute[j],by =Entity.type))
#   
# }


   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Total.liabilities =na.aggregate(Total.liabilities,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Total.capital =na.aggregate(Total.capital,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Reserves.and.funds =na.aggregate(Reserves.and.funds,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Non.current.liabilities =na.aggregate(Non.current.liabilities,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Current.liabilities...provisions =na.aggregate(Current.liabilities...provisions,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Short.term.borrowings =na.aggregate(Short.term.borrowings,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Short.term.trade.payables.and.acceptances =na.aggregate(Short.term.trade.payables.and.acceptances,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Net.worth =na.aggregate(Net.worth,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Capital.employed =na.aggregate(Capital.employed,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Contingent.liabilities =na.aggregate(Contingent.liabilities,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Non.current.assets =na.aggregate(Non.current.assets,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Gross.fixed.assets =na.aggregate(Gross.fixed.assets,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Net.fixed.assets =na.aggregate(Net.fixed.assets,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Current.assets..incl..short.term.investments..loans...advances. =na.aggregate(Current.assets..incl..short.term.investments..loans...advances. ,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Cash.balance =na.aggregate(Cash.balance,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Short.term.trade.receivables...bills.receivable =na.aggregate(Short.term.trade.receivables...bills.receivable,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Short.term.inventories =na.aggregate(Short.term.inventories,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,CostofGoodsSold =na.aggregate(CostofGoodsSold,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Interest.expense =na.aggregate(Interest.expense,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Premium.discount.on.issue.of.debt.instruments =na.aggregate(Premium.discount.on.issue.of.debt.instruments,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Other.fund.based.financial.services.expenses=na.aggregate(Other.fund.based.financial.services.expenses,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Treasury.operations.expenses=na.aggregate(Treasury.operations.expenses,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Fund.based.financial.services.expenses=na.aggregate(Fund.based.financial.services.expenses,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Fee.based.financial.services.expenses=na.aggregate(Fee.based.financial.services.expenses,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Sales=na.aggregate(Sales,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Other.income=na.aggregate(Other.income,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Total.expenses=na.aggregate(Total.expenses,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,PBDITA=na.aggregate(PBDITA,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,PBT=na.aggregate(PBT,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Cash.profit=na.aggregate(Cash.profit,by =Industry.group))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Profit.after.tax.reported.by.company=na.aggregate(Profit.after.tax.reported.by.company,by =Industry.group))
   
   
   #### imputing with entity type
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Total.liabilities =na.aggregate(Total.liabilities,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Total.capital =na.aggregate(Total.capital,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Reserves.and.funds =na.aggregate(Reserves.and.funds,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Non.current.liabilities =na.aggregate(Non.current.liabilities,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Current.liabilities...provisions =na.aggregate(Current.liabilities...provisions,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Short.term.borrowings =na.aggregate(Short.term.borrowings,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Short.term.trade.payables.and.acceptances =na.aggregate(Short.term.trade.payables.and.acceptances,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Net.worth =na.aggregate(Net.worth,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Capital.employed =na.aggregate(Capital.employed,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Contingent.liabilities =na.aggregate(Contingent.liabilities,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Non.current.assets =na.aggregate(Non.current.assets,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Gross.fixed.assets =na.aggregate(Gross.fixed.assets,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Net.fixed.assets =na.aggregate(Net.fixed.assets,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Current.assets..incl..short.term.investments..loans...advances. =na.aggregate(Current.assets..incl..short.term.investments..loans...advances. ,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Cash.balance =na.aggregate(Cash.balance,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Short.term.trade.receivables...bills.receivable =na.aggregate(Short.term.trade.receivables...bills.receivable,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Short.term.inventories =na.aggregate(Short.term.inventories,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,CostofGoodsSold =na.aggregate(CostofGoodsSold,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Interest.expense =na.aggregate(Interest.expense,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Premium.discount.on.issue.of.debt.instruments =na.aggregate(Premium.discount.on.issue.of.debt.instruments,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Other.fund.based.financial.services.expenses=na.aggregate(Other.fund.based.financial.services.expenses,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Treasury.operations.expenses=na.aggregate(Treasury.operations.expenses,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Fund.based.financial.services.expenses=na.aggregate(Fund.based.financial.services.expenses,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Fee.based.financial.services.expenses=na.aggregate(Fee.based.financial.services.expenses,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Sales=na.aggregate(Sales,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Other.income=na.aggregate(Other.income,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Total.expenses=na.aggregate(Total.expenses,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,PBDITA=na.aggregate(PBDITA,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,PBT=na.aggregate(PBT,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Cash.profit=na.aggregate(Cash.profit,by =Entity.type))
   Final_dataset_meanimpute<-transform(Final_dataset_meanimpute,Profit.after.tax.reported.by.company=na.aggregate(Profit.after.tax.reported.by.company,by =Entity.type))
   
   
   
   
   

