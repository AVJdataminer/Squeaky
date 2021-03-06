Find_vars<-function(home,df1,df2){
  #find missing variables between two data frames and add them to each
#setwd(home)
  #already good:x1,x3,x4,x6,x8
  #df1=x0
  #df2=xt0
  
  tame=names(df1)%in% names(df2)
  tm=data.frame(names(df1),tame)
  #where tame =False use that variable name to make a variable in xt
  nan=dplyr::filter(tm,tame=='FALSE')
  nan
  mat=matrix(rep(0,(nrow(x0)*nrow(nan))),nrow=nrow(x0),ncol = nrow(nan))
  mad=data.frame(mat)
  names(mad)=as.character(nan[,1])
  df2=data.frame(df2,mad)
  write.csv(df2,paste("xt",substr(names(df2[1]),3,4),".csv",sep = ""),row.names=F)
  tame=names(df2)%in% names(df1)
  tm=data.frame(names(df2),tame)
  nan=dplyr::filter(tm,tame=='FALSE')
  mat=matrix(rep(0,(nrow(x0)*nrow(nan))),nrow=nrow(x0),ncol = nrow(nan))
  mad=data.frame(mat)
  names(mad)=as.character(nan[,1])
  df1=data.frame(df1,mad)
  write.csv(df1,paste("x",substr(names(df2[1]),3,4),".csv",sep = ""),row.names = F)
}
BalanceLevels<-function(df1,df2){
	require(tidyverse)
  	require(purrr)
  	require(dplyr)
	#split df1 into factor and non-factor
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
	map_df(function(x) levels(x)==levels(select(df1,

}

