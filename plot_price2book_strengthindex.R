library(ggplot2)
library(scales)
source(file = 'indexes_lookup_table.R')
source(file = "r_script_add_wsj_url.R")
# source(file = "price_to_book.R")
# source(file = 'Strength_PriceIndex.R')
# strength_index <- strength_index[complete.cases(strength_index),]
untilQuintile <- 2
quintileSet <- c()
for (i in 1:untilQuintile){
  quintileSet <- append(quintileSet,paste("Q",as.character(i),sep = ""))
}
dataSet <- strength_index[strength_index$StrengthIndexQuintile %in% quintileSet &strength_index$PriceToBookQuintile %in% quintileSet
                          # strength_index$Quintile %in% c("Q1"
                          #                                                            ,"Q2"
                          #                                                            ,"Q3"
                          #                                                            ) 
                          #                             & strength_index$StrengthIndexQuintile %in% c("Q1"
                          #                                                                           ,"Q2"
                          #                                                                           ,"Q3")
                          ,]
View(dataSet)
if (nrow(dataSet)>1){
  limit_scale_x <- c(mean(dataSet$value)-3.16*sd(dataSet$value),mean(dataSet$value)+3.16*sd(dataSet$value))
  limit_scale_y <- c(mean(dataSet$stock_change)-3.16*sd(dataSet$stock_change),mean(dataSet$stock_change)+3.16*sd(dataSet$stock_change))
} else {
  limit_scale_x <- c(-0.5,3.0)
  limit_scale_y <- c(-0.05,0.2)
  
  }
plot <- ggplot(dataSet, 
       aes(x = value, y = stock_change, label = paste(stock_ticker,
                                                      "(",
                                                      PriceToBookQuintile ,
                                                      ",",
                                                      StrengthIndexQuintile,
                                                      ")",
                                                      sep = "")
           )
       ) + geom_point()+geom_text(aes(label=paste(stock_ticker,
                                                  "(",
                                                  PriceToBookQuintile ,
                                                  ",",
                                                  StrengthIndexQuintile,
                                                  ")",
                                                  sep = ""))
                                  ,hjust=0
                                  , vjust=0) + scale_x_continuous(trans = identity_trans()
                                                                  , limits = limit_scale_x) + scale_y_continuous(trans = identity_trans()
                                                                                                               , limits = limit_scale_y)

plot + theme(
  plot.margin = margin(1,1,1,1, "cm")
)

