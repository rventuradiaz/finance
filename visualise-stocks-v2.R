
# As per Curtis Miller's Blog [https://ntguardian.wordpress.com/2017/03/27/introduction-stock-market-data-r-1/]

testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}



end <- Sys.Date()
start <- end - 365


# stocks <- c("SGRE.MC", "ITRI", "VIE.PA","ANA.MC", "XYL","3402.T","FLC.AX","WTS", "ABG.MC","SZ1.F", "6366.T", "7012.T", "000157.KS", "GE","SAL.MI", "HYFXF","BFSA.DE","SCYR.MC", "ICL", "6254.T", "FCC.MC", "7011.T", "6370.T", "CWCO","TTEK","AWK","ARTNA","YORW")

subset <- match("AWK", stocks)
subset <- append(subset, match("YORW", stocks))
subset <- append(subset, match("CWCO", stocks))

stocksSubset <- stocks[subset]

par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))

stocksts <- list()

i <- 1
for(stock in stocksSubset){
  stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
  i <- i+1
  
}

# df  <- Cl(stocksts[[i]])
# p <- plot( df, main = names(df))

# par(mfcol=c(5,6), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
par(mfcol=c(3,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))

print(chart_theme)

for (i in 1:length(stocksts)){
  price <- na.omit(scale(Cl(stocksts[[i]])))
  #thanks to [https://stackoverflow.com/a/8816064]
  # To easily compare stocks, time series will be scaled, and compared along the same y range.
  
  print(chart_Series(na.omit(scale(stocksts[[i]])) #scaled
                                 ,"candlesticks"
                                 ,name = names(price)
                                 ,TA = 'add_TA(SMA(price, n = 20),on = 1, col = "green" ); add_TA(SMA(price, n = 132),on = 1, col = "red")'
                                 , layout = NULL
                                 , yrange = c(-3,3) #set the y range
  ))

}

