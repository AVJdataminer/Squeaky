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

