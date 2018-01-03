#function to convert factor variables each level to a zero and ones variable
FindNulls<-function(df,modelpath)
#seperate by pcamix split
#find factors with blank data
#use model.matrix function to create new variables
#recombine with numeric data
#write out to model_ready data
df=indata
d=PCAmixdata::splitmix(df)
require(dplyr)
dl=d$X.quali%>%summarize_each(funs(list(.)))

dn=ds$X.quali%>%map_df(function(x) is.null(levels(x)))%>%
	gather(feature,num_nulls)%>%
	filter(num_nulls>0)

mm=model.matrix(~.,-1,data=d$X.quali)

