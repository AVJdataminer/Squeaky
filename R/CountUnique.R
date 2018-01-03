#script to remove 100% unique vars
#such as row counts from file writes
CountUnique<-function(df, present){
  #example input natable=CountUnique(test3)
  require(tidyverse)
  require(purrr)
  #map_df allows for variable wise operations
#df=m1_d
  out=df%>%
    map_df(function(x) nrow(df)/length(unique(x)))%>%
    gather(feature,num_nulls)%>%
    filter(num_nulls==1)
#create a list of those variables to delete
	varlist=out$feature
  	v2=paste(varlist,sep=",")
	write.csv(v2,paste(present,"/CountUnique_deleted.csv", sep=""))
 #remove these vars from df
	df1=df%>% 
	select(-one_of(v2))
return(df1)

}