## unable to read xlsx file , hence using csv file


ScoringDataset<-read.csv("ScoringDataset.csv",header = TRUE)

ScoringDataset<-subset(ScoringDataset,select=-Industry.group.1)
Deletecols<-c("Industry.type.1","Main.product.service.group.1","Short.term.inventories.2009.1","Cost.of.goods.sold.2009.1","Short.term.inventories.2010.1","Cost.of.goods.sold.2010.1","Short.term.inventories.2011.1","Cost.of.goods.sold.2011.1","Short.term.inventories.2012.1","Cost.of.goods.sold.2012.1","Short.term.inventories.2013.1","Cost.of.goods.sold.2013.1","Short.term.inventories.2014.1","Cost.of.goods.sold.2014.1","Short.term.inventories.2015.1","Cost.of.goods.sold.2015.1","Short.term.inventories.2016.1","Cost.of.goods.sold.2016.1","Short.term.inventories.2017.1","Cost.of.goods.sold.2017.1","Short.term.inventories.2018.1","Cost.of.goods.sold.2018.1")
ScoringDataset<-ScoringDataset[,!colnames(ScoringDataset)%in%Deletecols]

Firmographics<-ScoringDataset[,c(1:14)]

Compnames<-ScoringDataset$Company.Name



##have run from here it runs fine
## 320 cols, we also need to create a 2009 ratings as it is not there
##total liabilities
MajorAttri<-list()
a=15 
b=31
##15-184
yr_MajorAttri=2009
yr_CostofGoodsSoldlist=2009
yr_interestexp_otherattri=2009
yr_salesnattri=2009
yr_rating=2010
for(i in 1:10){
  #MajorAttri[[i]]<-ScoringDataset[,c(a:b)]
   #a=a+17
   #b=b+17
  
  
  maj_attr<-ScoringDataset[,c(a:b)]
  names(maj_attr)<-substring(names(maj_attr),1,nchar(names(maj_attr))-5)
  maj_attr$Year=yr_MajorAttri
  maj_attr$CompanyNames<-Compnames
  MajorAttri[[i]]<-maj_attr
  a=a+17
  b=b+17
  yr_MajorAttri=yr_MajorAttri+1
  }


##185-194

CostofGoodsSoldlist<-list()
c=185
for (i in 1:10)
{
  
  CostofGoodsSold<-ScoringDataset[,c(c)] 
  CostofGoodsSold<-data.frame(CostofGoodsSold)
  CostofGoodsSold$Year=yr_CostofGoodsSoldlist
  CostofGoodsSold$CompanyNames<-Compnames
  CostofGoodsSoldlist[[i]]<-CostofGoodsSold
  c=c+1
  yr_CostofGoodsSoldlist=yr_CostofGoodsSoldlist+1
  
}


##195-254
interestexp_otherattri<-list()
d=195
e=200
for(i in 1:10){
  
  
  IE<-ScoringDataset[,c(d:e)]
  names(IE)<-substring(names(IE),1,nchar(names(IE))-5)
  IE$Year=yr_interestexp_otherattri
  IE$CompanyNames<-Compnames
  interestexp_otherattri[[i]]<-IE
  d=d+6
  e=e+6
  yr_interestexp_otherattri=yr_interestexp_otherattri+1
}


##255-324

salesnattri<-list()
f=255
g=261

for(i in 1:10){
  
  
  SLS<-ScoringDataset[,c(f:g)]
  names(SLS)<-substring(names(SLS),1,nchar(names(SLS))-5)
  SLS$Year=yr_salesnattri
  SLS$CompanyNames<-Compnames
  salesnattri[[i]]<-SLS
  f=f+7
  g=g+7
  yr_salesnattri=yr_salesnattri+1
}



## ratings 9years 325-333

Ratingsofcompany<-list()

h=325

for(i in 2:10){
  
  Ratings<-ScoringDataset[,c(h)]
  Ratings<-data.frame(Ratings)
  Ratings$Year=yr_rating
  Ratings$CompanyNames<-Compnames
  Ratingsofcompany[[i]]<-Ratings
  h=h+1
  yr_rating=yr_rating+1
}



Ratings<-rep(NA,10394)
Ratings<-data.frame(Ratings)
Rtngs_df<-Ratings
Rtngs_df$Year=2009
Rtngs_df$CompanyNames=Compnames
Ratingsofcompany[[1]]<-Rtngs_df     


##merge all data frames in a list
MajorAttri_listmerged<-bind_rows(MajorAttri)
CostofGoodsSoldlist_merged<-bind_rows(CostofGoodsSoldlist)
interestexp_otherattri_listmeged<-bind_rows(interestexp_otherattri)
salesnattri_listmerged<-bind_rows(salesnattri)
Ratingsofcompany_listmerged<-bind_rows(Ratingsofcompany)

Merged_Masterdata<-merge(MajorAttri_listmerged,CostofGoodsSoldlist_merged,by = c("CompanyNames","Year"))
Merged_Masterdata<-merge(Merged_Masterdata,interestexp_otherattri_listmeged,by = c("CompanyNames","Year"))
Merged_Masterdata<-merge(Merged_Masterdata,salesnattri_listmerged,by = c("CompanyNames","Year"))
Merged_Masterdata<-merge(Merged_Masterdata,Ratingsofcompany_listmerged,by = c("CompanyNames","Year"))
Merged_Masterdata<-merge(Firmographics,Merged_Masterdata,by.x ="Company.Name",by.y ="CompanyNames")

DefaultFlag_Yr_df<-ScoringDataset[,c("Company.Name","Default.Flag","Data.Set.Year")]
Merged_Masterdata<-merge(Merged_Masterdata,DefaultFlag_Yr_df,by ="Company.Name")


library(dplyr)


####default companies####   start running code from here
default_companies_merged_dataset<-Merged_Masterdata[Merged_Masterdata$Default.Flag==1,]
nondefault_companies_merged_dataset<-Merged_Masterdata[Merged_Masterdata$Default.Flag==0,]
nondefault_companies_scoringdataset<-ScoringDataset[ScoringDataset$Default.Flag==0,]   ##to retrieve names of comapnies in prop of default companies

def_comp_list<-list()
def_comp_filtered_list<-list()
yr_def_comp<-2009

no_of_def=0
namewise_distri_vector<-c()  #namewise ditibution according to the names randomly selected like c(0,564,872,1240,864,964,936,916,800,276)

non_def_comp_scoringdataset_nameslist<-list()
non_def_comp_mergeddataset_yrwisedislist<-list()   ## yearwise distribution according to line 172


for(x in 1:10)
{
  dcmd<-default_companies_merged_dataset[default_companies_merged_dataset$Data.Set.Year==yr_def_comp,]
  def_comp_list[[x]]<-dcmd
  def_comp_filtered_list[[x]]<-dcmd[dcmd$Year==yr_def_comp-1,]
  no_of_def=no_of_def+nrow(def_comp_filtered_list[[x]])
  namewise_distri_vector[x]<-nrow(def_comp_filtered_list[[x]])
  ##non default companies into a list acording to ratio mentioned in yrwisedis 172 ,
  ##where we first pickup companynames from scoringdataset and then pickup rows from merged_master  
  yr_def_comp=yr_def_comp+1
  
}


#non_def_yrwisedis<-c(0,564,872,1240,864,964,936,916,800,276)


### not used flawed logic##########
#for(y in 1:10)
#{
#  df_yrwisedis<-sample_n(nondefault_companies_scoringdataset,(namewise_distri_vector[y])*4)
#  non_def_comp_scoringdataset_nameslist[[y]]<-df_yrwisedis$Company.Name
  
#}
###################################

##here we find the next iteration of company names from the anti join of selected database and the whole scoring data set 
### new code to avoid repetition

unique_comp_list<-list()
unique_comp_list[[1]]<-nondefault_companies_scoringdataset
unique_names_list<-list()
for(v in 1:10)
{
  
  currentcomp_df<-sample_n(unique_comp_list[[v]],(namewise_distri_vector[v])*4)
  unique_names_list[[v]]<-currentcomp_df$Company.Name
  antijoined_df<-anti_join(unique_comp_list[[v]],currentcomp_df,by='Company.Name')
  unique_comp_list[[v+1]]<-antijoined_df
  
}

yr_non_def_comp<-2009

for(z in 1:10)
{
  #ndc_scnames<-as.character(non_def_comp_scoringdataset_nameslist[[z]])  ##previous flawed logic due to some names repeating
  ndc_scnames<-as.character(unique_names_list[[z]])  ## new hopefully correct logic
  ndc_mdyrdis<-nondefault_companies_merged_dataset[(as.character(nondefault_companies_merged_dataset$Company.Name)%in%ndc_scnames)&(nondefault_companies_merged_dataset$Year==yr_non_def_comp-1),] ##newly added
  non_def_comp_mergeddataset_yrwisedislist[[z]]<- ndc_mdyrdis 
  
  yr_non_def_comp=yr_non_def_comp+1
}

Final_list<-list()

for(w in 1:10)
{
  
  ##binding non default[w] with default [w]
  Final_list[[w]]<-rbind(def_comp_filtered_list[[w]],non_def_comp_mergeddataset_yrwisedislist[[w]])
  
}
  
  
Final_dataset<-bind_rows(Final_list)
Final_dataset<-arrange(Final_dataset,Year,Company.Name)


write.csv(Final_dataset,"Final_Dataset.csv",row.names = FALSE)