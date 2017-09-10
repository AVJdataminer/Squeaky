#script to remove 100% unique vars
#such as row counts from file writes
CountUnique<-function(df){
  #example input natable=CountUnique(test3)
  require(tidyverse)
  require(purrr)
  #map_df allows for variable wise operations
#df=indata
  out=df%>%
    map_df(function(x) nrow(df)/length(unique(x)))%>%
    gather(feature,num_nulls)%>%
    filter(num_nulls==1)%>%
    print(n=50)
  return(out$feature)
}