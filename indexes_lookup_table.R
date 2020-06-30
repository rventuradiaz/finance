stocks <- c("SGRE.MC","ANA.MC","SCYR.MC","FCC.MC", "ABG.MC",
            "ITRI", "CWCO","TTEK","ARTNA","YORW", "ANSS","PTC",
            "VIE.PA","DSY.PA",
            "GE","XYL","WTS","ICL", "AWK",
            "3402.T","6366.T",  "7012.T", "6254.T",  "7011.T", "6370.T",
            "FLC.AX",
            "SZ1.F", "BFSA.DE",
            "000157.KS",
            # "SAL.MI",
            "HYFXF"  )
indexes <- c("^IBEX","^IBEX","^IBEX","^IBEX","^IBEX",
           "^IXIC","^IXIC","^IXIC","^IXIC","^IXIC","^IXIC","^IXIC",
           "^FCHI", "^FCHI",
           "^NYA","^NYA","^NYA","^NYA","^NYA",
           "^N225","^N225","^N225","^N225","^N225","^N225",
           "^AXAT",
           "^GDAXI","^GDAXI",
           "^KS11",
           # "FTSEMIB.MI",
           "^GSPC")
lookup_index <- data.frame(stock_sticker = as.character(stocks), stock_index = as.character(indexes), stringsAsFactors = FALSE)
# View(lookup_index)
