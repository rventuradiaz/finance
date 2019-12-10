# install.packages("rvest")
library(rvest)
library(xml2)


url_address <- "https://www.reuters.com/finance/stocks/financial-highlights/"

# stocks <- c("SGRE.MC", "ITRI", "OHL.MC","VIE.PA","ANA.MC", "XYL","3402.T","FLC.AX","WTS", "ABG.MC","SZ1.F", "6366.T", "7012.T")
pricebook <- data.frame(stock_ticker =character(), value =numeric(), url = character())
i <- 1
for (stock in stocks) {
  
  
  url_mas_informacion <- paste('https://finance.yahoo.com/quote/',stock,'/key-statistics?p=',stock,sep = '') %>%
    read_html()
  path <-   xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(url_mas_informacion, 2), 1), 1), 1), 1), 1), 1), 2), 1), 1), 1), 4), 1), 1), 1), 2), 1), 2), 1), 1), 1), 7), 2)
  xml_text (path)
  print(stock)
  pricebook <- rbind(pricebook, data.frame(stock_ticker = stock, value = as.numeric(xml_text(path)), url = as.character(paste('https://finance.yahoo.com/quote/',stock,'/key-statistics?p=',stock,sep = '')) ))
  
}

View(pricebook)

for (stock in pricebook[is.na(pricebook$value),"stock_ticker"]) {
  
  
  url_mas_informacion <- paste(url_address,stock,sep = '') %>% 
    read_html() 
  path <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(url_mas_informacion, 2), 7), 3), 1), 2), 1), 4), 2), 1), 1), 9)
  pricebook[pricebook$stock_ticker == stock,"value"] <- as.numeric(unlist(strsplit(xml_text(path),"\n\t\t\t"))[2])
  pricebook <- rbind(pricebook, data.frame(stock_ticker = stock, value = as.numeric(unlist(strsplit(xml_text(path),"\n\t\t\t"))[2]), url = as.character(paste(url_address,stock,sep = ''))))
  # url_mas_informacion <- paste('https://finance.yahoo.com/quote/',stock,'/key-statistics?p=',stock,sep = '') %>% 
  #   read_html() 
  # path <-   xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(url_mas_informacion, 2), 1), 1), 1), 1), 1), 1), 2), 1), 1), 1), 4), 1), 1), 1), 2), 1), 2), 1), 1), 1), 7), 2)
  # xml_text (path)
  print(stock)
  # pricebook <- rbind(pricebook, data.frame(stock_ticker = stock, value = as.numeric(xml_text(path)), url = paste('https://finance.yahoo.com/quote/',stock,'/key-statistics?p=',stock,sep = '') ))
  
}

pricebook<- pricebook[!is.na(pricebook$value),]

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(pricebook$value, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3","Q4","Q5"), include.lowest=TRUE)
}

pricebook <- pricebook[-c(12,13),]

pricebook$Quintile <- sapply(pricebook$value, ApplyQuintiles)


table(df$Quintile)

View(pricebook[with(pricebook,order(-value)),])

# stock <- "OHL.MC"
# url_address <- "https://www.reuters.com/finance/stocks/financial-highlights/"
# 
# url_mas_informacion <- paste(url_address,stock,sep = '') %>% 
#   read_html() 
# 
# View(url_mas_informacion)
# 
# # 'https://finance.yahoo.com/quote/SGRE.MC/key-statistics?p=SGRE.MC'
# # url_initial <- "https://finance.yahoo.com/quote/SGRE.MC/key-statistics?p="
# # stock = "SGRE.MC"
# # 
# # url_mas_informacion <- paste(url_initial,stock, sep = "") %>% 
# #   read_html() 
# path <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(url_mas_informacion, 2), 7), 3), 1), 2), 1), 4), 2), 1), 1), 9)
# xml_text(path)
# unlist(strsplit(xml_text(path),"\n\t\t\t"))[2]
# # 
