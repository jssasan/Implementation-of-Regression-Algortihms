

data_path <-  "C:/Users/jasmeet/Desktop/SC/Solarcity/Analytics/Github/Projects/Kaggle/HouseSalesinKingCountyUSA/housesalesprediction"
input_fileName <- paste(data_path, "kc_house_data.csv",sep = "/")


data_ip <- read.csv(file = input_fileName, header = T, sep = ",")

head(data_ip)

colnames(data_ip)



### correlation plot



require(ggplot2)
require(reshape2)
require(dplyr)

## correlation plot

# or use one liner to change multiple columns at once
sapply(data_ip, class)

# df_6[3:22]  <- lapply(df_6[3:22], as.numeric)
# sapply(df_6, class)
# str(df_6)


correlation_matrix <- function(dataframe, cols_to_remove, output_filename){

  ## dataframe : this should be a dataframe in which there exists numerical columns
  ## cols_to_remove  : it should be a vector of columns for which we don't want correlations e.g. dates, ids etc
  ## output_filename : it should be name of the output pdf file name
  
  if(require(ggplot2)== FALSE) install.packages("ggplot2")
  if(require(reshape2)== FALSE) install.packages("reshape2")
  if(require(dplyr)== FALSE) install.packages("dplyr")
  
  g1<- ggplot(data = melt(cor(dataframe[,-cols_to_remove])), aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1,size = 12, hjust = 1))+
    coord_fixed()
  
  
  g2<- g1+ geom_text(aes(Var2, Var1, label = round(value,2)), color = "black", size = 3) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major =  element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(0,1),
      legend.position = c(1,1),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5)) +
    labs(title="Correlation Plot")
  
  g2
  
  
  ggsave(paste0(output_filename,"_corr.pdf"), plot = g2, height = 10, width = 15, units = "in")

}

correlation_matrix(dataframe = data_ip, cols_to_remove = c(1,2), output_filename = "trial_corr_matrix")


# outlier -- yes there are some of them
qplot(bedrooms,price, data=data_ip, fill=bedrooms, geom = c("boxplot", "jitter", "dotplot"))

qplot(bedrooms,price, data=data_ip, fill=bedrooms, geom = c("boxplot"))


cor_Search_SE<- cor.test(df_1$total_opps, df_1$leadagg_spend, method = "spearman")
cor_Search_SE















