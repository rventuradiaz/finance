
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


# stocks <- c("SGRE.MC", "ITRI", "OHL.MC","VIE.PA","ANA.MC", "XYL","3402.T","FLC.AX","WTS", "ABG.MC","SZ1.F", "^IBEX","^FCHI","^N225")
stocks <- c("OHL.MC")

par(mfcol=c(1,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))

stocksts <- list()

i <- 1
for(stock in stocks){
  stocksts[[i]] <- getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign =  FALSE , return.class = "xts")  
  i <- i+1
  
}


# df  <- Cl(stocksts[[i]])
# p <- plot( df, main = names(df))

for (i in 1:length(stocksts)){
  df <- na.omit(Cl(stocksts[[i]]))
  sma <- SMA(df,20)
  #thanks to [https://stackoverflow.com/a/8816064]
  # To easily compare stocks, time series will be scaled, and compared along the same y range.
  chartSeries(na.omit(stocksts[[i]]) #scaled
              ,"candlesticks"
              ,name = names(df)
              ,TA = NULL #No volume plot
              , layout = NULL
              , yrange = c(-0,3) #set the y range
              )
  print(addSMA(n = 20, col = "green"))
  print(addSMA(n = 180, col = "red"))
  testit(5.0)

}

