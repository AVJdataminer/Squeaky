Fill_NA<-function(x,modelpath){
  setwd(modelpath)
  if(nrow(x)<1){x<-read.csv(list.files(pattern = "indata_file.csv"))}	
  #split into numeric and non-numeric
  ds=PCAmixdata::splitmix(x)
 
  impute.med <- function(x) {
    z <- median(x, na.rm = TRUE)
    x[is.na(x)] <- z
    return(x)
  }
  
  #treat numeric NA's
  dat2 <- data.frame(sapply(ds$X.quanti, function(x){
    if(is.numeric(x) & any(is.na(x))){
      impute.med(x)
    } else {
      x
    } 
  }
  ))
  #treat factor or character Na's
  df1=lapply(ds$X.quali,forcats::fct_explicit_na)
  #paste two df's back together
  out1=cbind(dat2,df1)

  write.csv(out1, "Na_filled.csv", row.names = F)
  return(out1)
setwd(codein)
}
#need to add in a function to deal with logical data types before fct_explicit