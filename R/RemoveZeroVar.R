RemoveZeroVar<-function(df){
  nzv=caret::nearZeroVar(df)
  df=df[,-nzv]
  return(df)
}