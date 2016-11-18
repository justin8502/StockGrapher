if(require(quantmod) && require(ggplot2) && require(reshape2) && require(TTR)){
  
  # TODO
  # Cleanup Variable Names
  # Add declaration for MACROS
  # Get ATR to work
  
  # CLEANUP
  graphics.off()
  rm(list = ls())
  
  # MACROS - Tickers - Vector of tags wanted
  #          Month - Desired month to start from
  #          Day - Desired day to start from
  tickers <- c("YHOO","AAPL","IWM","SMH","OIH","XLY")
  month <- 11
  day <- 1
  # Stores # of stocks we are looking at
  numstock <- length(tickers)
  # Date to record from
  datefrom <- paste("2016-", month, "-", "0", day, sep = '', collapse='')
  # Date to record to (default current date)
  dateto <- paste(Sys.Date(), sep = '', collapse='')
  # Retrieve data
  getSymbols(tickers, from=datefrom, to=dateto)
  # Condense ticker data into matrix
  LargeSet <- do.call(merge, lapply(tickers, function(x) get(x)))
  
  # Vectors to extract Open and ATR (we'll use them later)
  getsubopen <- c(1)
  getsubatr <- c(1)
  # Setting up vectors to extract correctly
  for(i in 0:(numstock-1)) {
    getsubopen <- c(getsubopen, 2+i*6)
    if(i == 0){
      getsubatr <- c(getsubatr, 3)
    } else {
      getsubatr <- c(getsubatr, 3+i*4)
    }
  }
  
  # Subset yet again into only Open columns (We don't want CLOSE, etc.)
  dfOpen <- data.frame(Date=index(LargeSet), LargeSet, row.names=NULL)
  dfOpen <- dfOpen[, getsubopen]
  # Condense for easy graphing
  dfOpenCondense = melt(dfOpen, id='Date')
  # Plot the Opening values
  plot1 <- ggplot(dfOpenCondense, aes(Date, value, color = variable)) + 
    geom_line(size=0.5)
  
  # TEMPORARY FIX - RELOAD DATA AGAIN
  # Date to record from
  datefrom <- paste("2016-", month-1, "-", "0", day, sep = '', collapse='')
  # Date to record to (default current date)
  dateto <- paste(Sys.Date(), sep = '', collapse='')
  # Retrieve data
  getSymbols(tickers, from=datefrom, to=dateto)
  
  # Create set to store results of ATR (Average True Range)
  ATRset <- do.call(merge, lapply(tickers, function(x) ATR(get(x), 20)))

  # Subset the data (cut based on time), then make it into a dataframe and
  # Subset yet again into only atr columns (We don't want tr, etc.)
  ATRset <- subset(ATRset, index(ATRset) >= 
                      paste("2016-", month, "-", "0", day, sep = '', collapse=''))
  dfATR <- data.frame(Date=index(ATRset), ATRset, row.names=NULL)
  dfATR <- dfATR[, getsubatr]
  # Make the graph pretty
  colnames(dfATR) <- c("Date", tickers)
  # Condense for easy graphing
  dfATRCondense = melt(dfATR, id='Date')
  # Make graph EVEN prettier
  colnames(dfATRCondense)[3] <- "Percent"
  # Plot the ATR of tickers
  plot2 <- ggplot(dfATRCondense, aes(Date, Percent, color = variable)) + 
    geom_line(size=0.5)
  
  print(YHOO[, 1])
  print(RSI(YHOO[, 1], n=14));
  
  # RSIset <- do.call(merge, lapply(tickers, function(x) RSI(get(x), n=14)))
  
  print(plot1)
  print(plot2)
  
  summary(plot1)
  summary(plot2)
  
  rm(list = ls())
} else {
  print("Load packages failed")
}
