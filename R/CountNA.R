CountNA<-function(df){
  #example input natable=CountNA(df)
  require(tidyverse)
  require(purrr)
  require(dplyr)
  #map_df allows for variable wise operations
  out=df%>%
    map_df(function(x) sum(is.na(x)))%>%
    gather(feature,num_nulls)%>%
    filter(num_nulls>0)%>%
    print(n=50)
  ifelse(out$num_nulls>0,return(out),return("No NA's found")
}

