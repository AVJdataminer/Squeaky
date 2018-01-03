#create percent 
  
theme_new <- function(base_size = 12, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #line = element_line(colour="black"),
      #text = element_text(colour="black"),
      axis.title = element_text(size = 14),
      #axis.text = element_text(colour="black", size=8),
      strip.text = element_text(size=12,colour="darkred"),
      legend.key=element_rect(colour=NA, fill =NA),
      panel.grid = element_blank(),   
      panel.border = element_rect(fill = NA, colour = "darkred", size=1),
      panel.background = element_rect(fill = "white", colour = "white"), 
      strip.background = element_rect(fill = "gray", colour="white")
    )
}

df$outcome=ifelse(df$response=='Y' & df$predicted=='Y', "Accepted-as-Accepted",
                  ifelse(df$response=='N' & df$predicted=='Y',"Not Accepted-as-Accepted",
                         ifelse(df$response=='Y' & df$predicted=='N',"Accepted-as-Not Accepted",
                                ifelse(df$response=='N' & df$predicted=='N',"Not Accepted-as-Not Accepted"," "))))

PercentHists<-function(df, folder){
  require(ggplot2)
  ds=PCAmixdata::splitmix(df)
  dfn<- data.frame(ds$X.quanti)
  dff=data.frame(ds$X.quanti$response,ds$X.quali)
  names(dff)[1]<-"response" #as numeric
  plotFunc <- function(x, na.rm = TRUE){
    nm <- names(x)
    for (i in seq_along(nm)) {
      plots <-ggplot(x) + stat_count(mapping = aes(x=nm[i], y=..prop.., group=1))+facet_grid(~response)+
        geom_histogram(fill="darkred", color="lightgrey", position="dodge")+theme_new()+
        theme(legend.position = "none")+ ggtitle(paste(nm[i]))+
        theme(axis.text.x = element_text(angle = 0, hjust = 1))
      ggsave(filename=paste(folder,"/",nm[i],".png", sep=""),plot=plots)
    }}
  plotFunc(df)
}

