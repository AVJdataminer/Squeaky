filtbyDate<-function(df,column,format,startdate,enddate){
  require(dplyr)
  #df=df
  #column='Contract.Eff.Date'
  #format="%Y-%m-%d"
  invar=as.Date(df[[column]],format=format)
  #sd='1/1/2008'
  #ed='1/1/2016'
  startdate=as.Date(startdate,"%m/%d/%Y")
  enddate=as.Date(enddate,"%m/%d/%Y")
  
  newdf=df%>%filter(invar >startdate & invar<enddate)
  return(newdf)
  write.csv(newdf, 'df_filterdbydate.csv',row.names = F)
}