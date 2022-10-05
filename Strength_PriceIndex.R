# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}


strength_index <- data.frame(stock_ticker = character(0), stock_change = numeric(0), index_change = numeric(0), rel_strength = numeric(0))

end <- Sys.Date()
start <- end - 180

# "7012.T"


# stocks <- c("AVV.L")

stocksts <- list()
index_change <- data.frame(row_index = as.integer(0), change = as.numeric(0))
stock_change <- data.frame(row_index = as.integer(0), change = as.numeric(0))

i <- 1
for(stock in stocks){
  stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
  price <- na.omit(Cl(stocksts[[i]]))
  stock_change <- data.frame(row_index = nrow(price), change = as.numeric(price[nrow(price),c(1)])/as.numeric(price[1,c(1)])-1.0)
  i <- i+1
  index <- lookup_index [match(stock, lookup_index$stock_sticker, nomatch = 0),"stock_index"]
  stocksts[[i]] <- getSymbols(index, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
  price <- na.omit(Cl(stocksts[[i]]))
  index_change <- data.frame(row_index = nrow(price), change = as.numeric(price[nrow(price),c(1)])/as.numeric(price[1,c(1)])-1.0)
  strength_index <- rbind(strength_index, data.frame(stock_ticker = stock , stock_change = as.numeric(stock_change[1,2]), index_change = as.numeric(index_change[1,2]), rel_strength = as.numeric((1.0+stock_change[1,2])/(1.0+index_change[1,2])-1.0)))
  i <- i+1
}

# merge with price to book data frame

strength_index <- merge(strength_index, pricebook, by = "stock_ticker")

ApplyQuintilesPriceIndex <- function(x) {
  cut(x, breaks=c(quantile(strength_index$stock_change, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q5","Q4","Q3","Q2","Q1"), include.lowest=TRUE)
}

strength_index$PriceIndexQuintile <- sapply(strength_index$stock_change, ApplyQuintilesPriceIndex)

ApplyQuintilesStrengthIndex <- function(x) {
  cut(x, breaks=c(quantile(strength_index$rel_strength, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q5","Q4","Q3","Q2","Q1"), include.lowest=TRUE)
}

strength_index$StrengthIndexQuintile <- sapply(strength_index$stock_change, ApplyQuintilesStrengthIndex)

head(strength_index)
# order by change of stock
# View(strength_index[with(strength_index, order(PriceToBookQuintile, StrengthIndexQuintile)),])



# # 3402.T
# 
# stocks <- c("^N225")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# stocks <- c("3402.T")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# 
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength = as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# # SAL.MI
# 
# stocks <- c("SAL.MI")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# stocks <- c("SAL.MI")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# 
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# # 000157.KS
# 
# stocks <- c("000157.KS")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# stocks <- c("000157.KS")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# 
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# # HYFXF
# 
# stocks <- c("HYFXF")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# stocks <- c("HYFXF")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# 
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# # 6254.T
# 
# stocks <- c("6254.T")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# stocks <- c("6254.T")
# 
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# 
# stocksts <- list()
# 
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# 
# price <- na.omit(Cl(stocksts[[1]]))
# 
# 
# 
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# 
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# 
# 
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# #  "CWCO"
# stocks <- c("CWCO")
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
# }
# price <- na.omit(Cl(stocksts[[1]]))
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# stocks <- c("CWCO")
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# price <- na.omit(Cl(stocksts[[1]]))
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# 
# # "TTEK"
# 
# stocks <- c("TTEK")
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
# }
# price <- na.omit(Cl(stocksts[[1]]))
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# stocks <- c("TTEK")
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# price <- na.omit(Cl(stocksts[[1]]))
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# 
# # "AWK"
# 
# stocks <- c("AWK")
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
# }
# price <- na.omit(Cl(stocksts[[1]]))
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# stocks <- c("AWK")
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# price <- na.omit(Cl(stocksts[[1]]))
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# 
# # "ARTNA"
# 
# stocks <- c("ARTNA")
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
# }
# price <- na.omit(Cl(stocksts[[1]]))
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# stocks <- c("ARTNA")
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# price <- na.omit(Cl(stocksts[[1]]))
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))
# 
# 
# # "YORW"
# 
# stocks <- c("YORW")
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
# }
# price <- na.omit(Cl(stocksts[[1]]))
# index_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   index_change <- rbind(index_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# stocks <- c("YORW")
# par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
# stocksts <- list()
# i <- 1
# for(stock in stocks){
#   stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
#   i <- i+1
#   
# }
# price <- na.omit(Cl(stocksts[[1]]))
# stock_change <- data.frame(row_index = as.integer(1), change = as.numeric(NA))
# for (i in c(nrow(price))){
#   stock_change <- rbind(stock_change,  data.frame(row_index = i, change = as.numeric(price[i,c(1)])/as.numeric(price[1,c(1)])-1.0))
# }
# strength_index <- rbind(strength_index, data.frame(stock_ticker = stocks[1] , stock_change = as.numeric(stock_change[2,2]), index_change = as.numeric(index_change[2,2]), rel_strength= as.numeric((1.0+stock_change[2,2])/(1.0+index_change[2,2])-1.0)))

