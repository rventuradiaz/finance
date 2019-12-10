library(ggplot2)
library(scales)
source(file = 'indexes_lookup_table.R')
source(file = 'Strength_PriceIndex.R')
# strength_index <- strength_index[complete.cases(strength_index),]
plot <- ggplot(strength_index[strength_index$Quintile %in% c("Q1","Q2","Q3","Q4","Q5") & strength_index$StrengthIndexQuintile %in% c("Q1","Q2","Q3","Q4","Q5"),], 
       aes(x = value, y = index_change, label = paste(stock_ticker,
                                                      "(",
                                                      Quintile ,
                                                      ",",
                                                      StrengthIndexQuintile,
                                                      ")",
                                                      sep = "")
           )
       ) + geom_point()+geom_text(aes(label=paste(stock_ticker,
                                                  "(",
                                                  Quintile ,
                                                  ",",
                                                  StrengthIndexQuintile,
                                                  ")",
                                                  sep = "")),hjust=0, vjust=0) + scale_x_continuous(trans = log10_trans(), limits = c(0.5,3.0)) + scale_y_continuous(trans = log10_trans(), limits = c(-0.1,0.2))

plot + theme(
  plot.margin = margin(1,1,1,1, "cm")
)
