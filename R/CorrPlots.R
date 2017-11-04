#function to create correlation plots
#output to modelpath figures and reporting

CorrPlots<-function(df,response,modelpath){
require(ggplot2)
require(GGally)
require(corrplot)
require(dplyr)
require(caret)
  #remove response variable
  ds=select(df, -response)
  #remove factor variables
  rain=PCAmixdata::splitmix(ds)
  corm=cor(rain$X.quanti)
  
  #reorder variables
    reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
      dd <- as.dist((1-cormat)/2)
      hc <- hclust(dd)
      cormat <-cormat[hc$order, hc$order]
   }
  cormat <- reorder_cormat(corm)
  
  hc=findCorrelation(cormat, cutoff = 0.50)
  df2 <- data.frame(subset(df, select = c(hc)))
  vars=paste("-", names(df2), sep="")
  print(names(df2))
  df3=select_(df,.dots = vars)
  write.csv(df2,paste(modelpath, '/reporting/df_cor_vars_deleted.csv',sep=""), row.names = F)
  write.csv(names(df2),paste(modelpath, '/reporting/cor_vars_deleted.csv',sep=""),row.names = F)
  write.csv(data.frame(df3, df$response),paste(modelpath, '/data_cor_vars_kept.csv',sep=""),row.names = F)
  
  #save corr plot
  setwd(paste(modelpath, '/figures', sep=""))
  png("var_corr_all.png", width = 1400, height = 1100)
  ggh2=GGally::ggcorr(cormat, nbreaks=5)
  print(ggh2)
  device.off()
  
  #recalc cor
  corm2=cor(df2)
  cormat <- reorder_cormat(corm2)
  #deleted cor
  setwd(paste(modelpath, '/figures', sep=""))
  png("var_corr_deleted.png", width = 1400, height = 1100)
  ggh2=GGally::ggcorr(cormat, nbreaks=5)
  print(ggh2)
  device.off()
  
  #recalc cor
  ds=select(df3, -response)
  #remove factor variables
  wind=PCAmixdata::splitmix(ds)
  corm3=cor(wind$X.quanti)
  cormat <- reorder_cormat(corm3)
  #keptd cor
  setwd(paste(modelpath, '/figures', sep=""))
  png("var_corr_kept.png", width = 1400, height = 1100)
  ggh2=GGally::ggcorr(cormat, nbreaks=5)
  print(ggh2)
  device.off()

  return(df3)
  
}
