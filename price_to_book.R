# install.packages("rvest")
library(rvest)
library(xml2)
library(magrittr)

# url_address <- "https://www.reuters.com/finance/stocks/financial-highlights/"

stocks <- c("ITRI", "VIE.PA", "XYL","3402.T","FLC.AX","WTS",  "6366.T", "7012.T", "GE", "HYFXF", "ICL", "6254.T",  "7011.T", "6370.T", "CWCO","TTEK","AWK","ARTNA","YORW", "PTC", "DSY.PA", "ANSS","AVV.L")
pricebook <- data.frame(stock_ticker =character(), value =numeric(), url = character())
i <- 1
for (stock in stocks) {
  print(stock)
  
  # url_mas_informacion <- paste('https://finance.yahoo.com/quote/',stock,'/key-statistics?p=',stock,sep = '') %>%
  #   read_html()
  url_mas_informacion <- lookup_index[match(stock,lookup_index$stock_sticker ),"WSJ_URL"] %>%
    read_html()
  write_xml(url_mas_informacion, file = "financials.xml")
  ptobook <- tryCatch({
    html_nodes(url_mas_informacion,"table") %>% 
      extract2(4) %>% 
      html_nodes("td")  %>% 
      extract2(4) %>% 
      html_nodes("span") %>% 
      extract2(3) %>% 
      html_text() %>% as.numeric()
  },error = function(err){
    print(paste("Error for stock",stock,": ", err))
    f <- NA
    return(f)
  }
  )
    
  # ptobook <- url_mas_informacion %>% 
  #   html_nodes(xpath = "/html/body/div[1]/section[2]/div[2]/div[1]/div[3]/div/div[2]/div[1]/div[1]/table/tbody/tr[4]/td/span[2]/span") %>%
  #   html_text() %>%
  #   as.numeric()
  
  pricebook <- rbind(pricebook, data.frame(stock_ticker = stock
                                           , value = ptobook
                                           , url = lookup_index[match(stock,lookup_index$stock_sticker ),"WSJ_URL"] ))
  
}

# View(pricebook)

# for (stock in pricebook[is.na(pricebook$value),"stock_ticker"]) {
#   
#   
#   url_mas_informacion <- paste(url_address,stock,sep = '') %>% 
#     read_html() 
#   path <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(url_mas_informacion, 2), 7), 3), 1), 2), 1), 4), 2), 1), 1), 9)
#   pricebook[pricebook$stock_ticker == stock,"value"] <- as.numeric(unlist(strsplit(xml_text(path),"\n\t\t\t"))[2])
#   pricebook <- rbind(pricebook, data.frame(stock_ticker = stock, value = as.numeric(unlist(strsplit(xml_text(path),"\n\t\t\t"))[2]), url = as.character(paste(url_address,stock,sep = ''))))
#   # url_mas_informacion <- paste('https://finance.yahoo.com/quote/',stock,'/key-statistics?p=',stock,sep = '') %>% 
#   #   read_html() 
#   # path <-   xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(url_mas_informacion, 2), 1), 1), 1), 1), 1), 1), 2), 1), 1), 1), 4), 1), 1), 1), 2), 1), 2), 1), 1), 1), 7), 2)
#   # xml_text (path)
#   print(stock)
#   # pricebook <- rbind(pricebook, data.frame(stock_ticker = stock, value = as.numeric(xml_text(path)), url = paste('https://finance.yahoo.com/quote/',stock,'/key-statistics?p=',stock,sep = '') ))
#   
# }

pricebook<- pricebook[!is.na(pricebook$value),]

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(pricebook$value, probs = seq(0, 1, by = 0.20))), 
      labels=c("Q1","Q2","Q3","Q4","Q5"), include.lowest=TRUE)
}

# pricebook <- pricebook[-c(12,13),]

pricebook$PriceToBookQuintile <- sapply(pricebook$value, ApplyQuintiles)


table(df$PriceToBookQuintile)

# View(pricebook[with(pricebook,order(-value)),])

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
