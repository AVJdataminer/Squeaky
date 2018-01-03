#function to create correlation plots
#output to modelpath figures and reporting

CorrPlots<-function(df,response,modelpath){
 # df=noNAnzv
require(ggplot2)
require(GGally)
require(corrplot)
require(dplyr)
  require(caret)
  #remove response variable
  #ds=select(df, -response)
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
  
  
  hc=findCorrelation(cormat, cutoff = 0.50,names=TRUE, exact = TRUE)
  vars=paste("-", hc, sep="")
  df2=select_(df,.dots = vars)
  
  # sim=PCAmixdata::splitmix(df2)
  # corm2=cor(sim$X.quanti)
  # cormat2 <- reorder_cormat(corm2)
  # ha=findCorrelation(cormat2, cutoff = 0.20,exact=T,names=T)
  # vars2=paste("-",ha,sep = "")
  # df2.3 <- select_(df2,.dots=vars2)
  
  
  #write.csv(df2.3,paste(modelpath, '/reporting/df_cor_vars_deleted.csv',sep=""), row.names = F)
  write.csv(c(hc),paste(modelpath, '/reporting/cor_vars_deleted.csv',sep=""),row.names = F)
  write.csv(data.frame(df2, df$response),paste(modelpath, '/data_cor_vars_kept.csv',sep=""),row.names = F)
  
  #save corr plot
  setwd(paste(modelpath, '/figures', sep=""))
  ggh2=GGally::ggcorr(cormat, nbreaks=5)
  png("var_corr_all.png", width = 1400, height = 1100)
    print(ggh2)
  dev.off()
  pdf("var_corr_all.pdf", width = 8, height = 8)
  print(ggh2)
  dev.off()
  
  #deleted cor
  setwd(paste(modelpath, '/figures', sep=""))
  png("var_corr_deleted.png", width = 1400, height = 1100)
  ggh2=GGally::ggcorr(cormat, nbreaks=5)
    print(ggh2)
  dev.off()
  
  pdf("var_corr_deleted.pdf", width = 8, height = 8)
  print(ggh2)
  dev.off()

  #keptd cor
  setwd(paste(modelpath, '/figures', sep=""))
  png("var_corr_kept.png", width = 1400, height = 1100)
  ggh2=GGally::ggcorr(cormat, nbreaks=5)
    print(ggh2)
  dev.off()
  pdf("var_corr_kept.pdf", width = 8, height = 8)
  print(ggh2)
  dev.off()
  return(df2)
  
}
rain=PCAmixdata::splitmix(df)
corm=cor(rain$X.quanti)
ggh2=GGally::ggcorr(corm, nbreaks=5)
ggh2
CorrPlots(df,out.n,paste(modelpath,"/figures", sep=""))