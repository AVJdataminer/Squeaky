CountNA<-function(df, present){
  #example input natable=CountNA(df)
  require(tidyverse)
  require(purrr)
  require(dplyr)
  #map_df allows for variable wise operations
  out=df%>%
    map_df(function(x) sum(is.na(x)))%>%
    gather(feature,num_nulls)%>%
    filter(num_nulls>0)
#write out the names of deleted vars
write.csv(names(out),paste(present,"/Count50_deleted.csv",sep=""))
  return(out)
}

