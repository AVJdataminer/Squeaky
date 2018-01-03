#function to create freq plots
FreqPlots<-function(df, folder){
require(ggplot2)
 plotFunc <- function(x, na.rm = TRUE){
  nm <- names(x)
  for (i in seq_along(nm)) {
    plots <-ggplot(x,aes_string(x = nm[i]), fill="lightblue") +
      geom_bar(stat="count",position = position_dodge())+theme_bw()+
      theme(legend.position = "none")+ ggtitle(paste(nm[i]))+ 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(filename=paste(folder,"/Freq_plot_",i,".png", sep=""),plot=plots)
  }}
 plotFunc(df)
}


#variable importance
td=read.csv(paste(modelpath,"/Feature_list.csv", sep=""))

dr_plot <- ggplot(td,
                  aes(reorder(Feature.Name, Relative.Importance),Relative.Importance)) +
  geom_col(aes(fill = '')) +
  scale_fill_manual(values = c("darkred"),guide=FALSE) +
  theme_minimal()+
  scale_y_continuous() +
  labs(list(y = "Relative Importance", x = "Variable"))+
  theme(panel.grid.minor = element_blank()) +theme_new+coord_flip()

ggsave(filename='variable_importance.png', plot=dr_plot)

ggh=GGally::ggcorr(cormat)
ggh

hc=findCorrelation(corm, cutoff = 0.50)
print(hc)
df2 <- data.frame(subset(train, select = c(hc)))
vars=paste("-", names(df2), sep="")
df3=select_(train,.dots = vars)
png("Var_corr_test3.png", width = 1400, height = 1100,)
ggh2=GGally::ggcorr(df3, nbreaks=5)
ggh2
dev.off()

pdf("Var_corr_test8.pdf", width = 8, height =8 )
ggh2=GGally::ggcorr(df3, nbreaks=5, hjust = 0.75, size = 3)
ggh2
dev.off()

##another type of correlation plots

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat) 
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
ggheatmap
geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
