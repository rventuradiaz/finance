library(ggplot2)

ggplot(strength_index[strength_index$Quintile %in% c("Q1") & strength_index$PriceIndexQuintile %in% c("Q1","Q2"),], aes(x = value, y = index_change, label = stock_ticker), )+ geom_point()+geom_text(aes(label=stock_ticker),hjust=0, vjust=0)