#balance the levels of factors between two dfs
#this assumes both dfs have the same variables, 
#e.g. find_vars was already run on these data

BalanceLevels<-function(df1,df2,modelpath){
	require(tidyverse)
  	require(purrr)
  	require(dplyr)
	#split df1 into factor and non-factor
	#df1=train
	#df2=test
	#Name1='train'
	#Name2='test'
	out=df1%>%
    	map_df(function(x) is.factor(x))%>%
    	gather(feature,num_nulls)%>%
    	filter(num_nulls==TRUE)%>%
	print(n=50)
	list1=out$feature
#select these vars from df2 using dots
#store as new df, feed to same function rewritten to set levels for each variable
	out2=df2%>%
	select_(.dots=list1)
	out3=df1%>%
	select_(.dots=list1)
#merge the two dfs together then set levels to match merged set
	totdf=rbind(out2,out3)
	for(i in list1){
	levels(out2[[i]])<-levels(totdf[[i]])
	levels(out3[[i]])<-levels(totdf[[i]])}
#recreate non-factor data from search list
	nf=df1%>%
    	map_df(function(x) is.factor(x))%>%
    	gather(feature,num_nulls)%>%
    	filter(num_nulls==FALSE)
	list2=nf$feature
	nf2=df2%>%
	select_(.dots=list2)
	nf3=df1%>%
	select_(.dots=list2)
# put pieces back togther for output dfs
train=data.frame(nf3,out3)
test=data.frame(nf2,out2)
return(test)
return(train)
}
