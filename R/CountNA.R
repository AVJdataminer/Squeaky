CountNA<-function(df){
  #example input natable=CountNA(df)
  require(tidyverse)
  require(purrr)
  require(dplyr)
  #map_df allows for variable wise operations
  out=df%>%
    map_df(function(x) sum(is.na(x)))%>%
    gather(feature,num_nulls)%>%
    if(num_nulls>0){filter(num_nulls>0)%>%
    print(n=50)} else {print("No NA's Found")}
  return(out))
}

