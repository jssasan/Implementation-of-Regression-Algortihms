



## this function generates spearman correlation of continuous variables and gives a matrix plot

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
  
  
  ggsave(paste0(output_filename,"_corr.pdf"), plot = g2, height = 10, width = 15, units = "in")  ## generate the corr. matrix graph
  
  correlation_of_vars <- melt(cor(dataframe[,-cols_to_remove]))  ## generate the correlation dataset 
  
  write.csv(x = correlation_of_vars, file = paste0(output_filename, ".csv"), row.names = FALSE) ## output the correlation in csv
  
}



## Correlation of categorical variables











## Correlation of categorical vs continuous variables










## anova











## chi-square test 









## indepenednt t-test








## Mann-whitney test








## Paired T-test










## WILCOXON SIGNED RANK TEST






## ONE-WAY ANOVA







##  Kruskal - Wallis test






##  ONE-WAY ANOVA WITH REPEATED MEASURES (WITHIN SUBJECTS









## FRIEDMAN TEST ..





## TWO-WAY ANOVA







##  ODDS AND RELATIVE RISK .............................................................................................................................................................31
## Odds .................................................................................................................................................................................31
## Odds Ratio ........................................................................................................................................................................31
## Relative Risk (RR)



## Shapiro-Wilk 





## Kolmogorov-Smirnoff




## Homogeneity of variances: Levene’s test




## Sphericity: Mauchly’s test





##  Durbin Watson test for regression





###  Tukey and Sidak Test 




##  Scheffe’s Test


## CORRELATION ............................................................................................................................................................................32
## PEARSON’S CORRELATION COEFFICIENT ............................................................................................................................................32
## RANKED CORRELATION COEFFICIENTS ...............................................................................................................................................33
## Spearman’s Rank Correlation Coefficient ..........................................................................................................................33
## Kendall’s Tau Rank Correlation Coefficient ........................................................................................................................33
## PARTIAL CORRELATION .................................................................................................................................................................33






## REGRESSION ..............................................................................................................................................................................34
## LINEAR REGRESSION ....................................................................................................................................................................34
## LOGISTIC REGRESSION ..................................................................................................................................................................36


## PROPORTIONS TEST (Z-TEST)..........................................................................................................................................................38
## RELIABILITY ...............................................................................................................................................................................39
## Interrater reliability ..........................................................................................................................................................39
## Cohen’s Kappa ..................................................................................................................................................................39
## Intraclass Correlation Coefficient ......................................................................................................................................40
## Cronbach's alpha (reliability of scales) .............................................................................................................................41
## PRINCIPAL COMPONENT ANALYSIS (PCA) ........................................................................................................................................43
## CLUSTER ANALYSIS ......................................................................................................................................................................47



#3 HIERARCHICAL CLUSTERING ...........................................................................................................................................................47
## K-MEANS CLUSTERING..................................................................................................................................................................49













