RemoveNA50<-function(df,natable){
#run CountNA function here to feed this function the natable df
  require(tidyverse)
  require(purrr)
  require(dplyr)
  nlen=nrow(df)*.5
  varlist=natable$feature[natable$num_nulls>nlen]
  v2=paste(varlist,sep=",")
  #remove these vars from df
	df1=df%>% 
	select(-one_of(v2))
  print(v2)
 return(df1)
}