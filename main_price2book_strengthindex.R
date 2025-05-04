library(ggplot2)
library(scales)
library(httr)
library(keyring)
library(rvest)
library(openxlsx)
library(stringr)

#  Login in WSJ
kr_Username <- "Biblioteca@ie.edu"
kr_service <- "WSJ"
# keyring::key_set(service = kr_service, username = kr_Username)

# wsjUrl <- "https://accounts.wsj.com/login?target=https%3A%2F%2Fwww.wsj.com%2Fmarket-data%2Fquotes%2FPTC%2Ffinancials"
# wsjSession <- session(wsjUrl)
# wsjForm <- html_form(wsjSession)[[1]]
# fl_fm <- set_values(wsjForm,
#                     username = kr_Username,
#                     password = keyring::key_get(service = kr_service,username = kr_Username)
# )
# main_page <- session_submit(wsjSession,fl_fm)


source(file = 'reference/indexes_lookup_table.R')
source(file = "reference/r_script_add_wsj_url.R")
source(file = "price_to_book.R")
source(file = 'Strength_PriceIndex.R')

par(mfcol=c(2,1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))


# output file
date_string <- as.character(Sys.Date())

# strength_index <- strength_index[complete.cases(strength_index),]
untilQuintile <- 5
untilQuintileString <- as.character(untilQuintile)
out_filename <- str_glue("output/export_strenght_index_Q{untilQuintileString}_{date_string}.xlsx")

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
names(dataSet)<-c("stock_ticker"          
                  ,"stock_change"          
                  ,"index_change"          
                  ,"rel_strength"  
                  ,"price_to_book"                 
                  ,"url"                   
                  ,"PriceToBookQuintile"   
                  ,"PriceIndexQuintile"
                  ,"StrengthIndexQuintile")

View(dataSet[with(dataSet,order(price_to_book,-rel_strength)),])

openxlsx::write.xlsx(dataSet, file = out_filename)

if (nrow(dataSet)>1){
  limit_scale_x <- c(mean(dataSet$price_to_book)-3.16*sd(dataSet$price_to_book),mean(dataSet$price_to_book)+3.16*sd(dataSet$price_to_book))
  limit_scale_y <- c(mean(dataSet$rel_strength)-3.16*sd(dataSet$rel_strength),mean(dataSet$rel_strength)+3.16*sd(dataSet$rel_strength))
} else {
  limit_scale_x <- c(-0.5,3.0)
  limit_scale_y <- c(-0.05,0.2)
  
  }
plot <- ggplot(dataSet, 
       aes(x = price_to_book, y = rel_strength, label = paste(stock_ticker,
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

source("visualise-stocks-v2.R")
