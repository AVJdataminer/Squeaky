CountNA<-function(df){
  #example input natable=CountNA(df)
  require(tidyverse)
  require(purrr)
  require(dplyr)
  #map_df allows for variable wise operations
  out=df%>%
    map_df(function(x) sum(is.na(x)))%>%
    gather(feature,num_nulls)%>%
    num_nulls>0 >%
    print(n=50)
  return(out)
}

