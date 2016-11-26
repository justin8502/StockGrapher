if(require(quantmod) && require(ggplot2) && require(reshape2) && require(TTR)){
  
  # CLEANUP
  graphics.off()
  rm(list = ls())
  
  # MACROS - Tickers - Vector of tags wanted
  #          Month - Desired month to start from
  #          Day - Desired day to start from
  tickers <- c("MSFT","IWM","KO","AAPL","DD")
  month <- 10
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
    geom_line(size=0.5) + ggtitle("OPEN values")
  
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
    geom_line(size=0.5) + ggtitle("Average True Range")
  
  # Create set to store results of RSI (Relative Strength Indicator)
  RSIset <- do.call(merge, lapply(tickers, function(x) RSI(get(x)[, 6], n=14)))
  # Subset the data (cut based on time), then make it into a dataframe
  RSIset <- subset(RSIset, index(RSIset) >= 
                     paste("2016-", month, "-", "0", day, sep = '', collapse=''))
  dfRSI <- data.frame(Date=index(RSIset), RSIset, row.names=NULL)
  # Make the graph pretty
  colnames(dfRSI) <- c("Date", tickers)
  # Condense for easy graphing
  dfRSICondense = melt(dfRSI, id='Date')
  # Make graph EVEN prettier
  colnames(dfRSICondense)[3] <- "Value"
  # Plot the ATR of tickers
  plot3 <- ggplot(dfRSICondense, aes(Date, Value, color = variable)) + 
    geom_line(size=0.5) + geom_hline(aes(yintercept=30)) + 
    geom_hline(aes(yintercept=70)) + ggtitle("Relative Strength Index")
  
  # Create set to store results of MACD (Moving Average Convergence Divergence)
  MACDset <- do.call(merge, lapply(tickers, function(x) 
    MACD(get(x)[, 6], 8, 17, 9, maType="EMA", percent=FALSE)))
  # Subset the data (cut based on time), then make it into a dataframe
  MACDset <- subset(MACDset, index(MACDset) >= 
                      paste("2016-", month, "-", "0", day, sep = '', collapse=''))
  dfMACD <- data.frame(Date=index(MACDset), MACDset, row.names=NULL)
  # Make the graph pretty
  colnames(dfMACD) <- c("Date", tickers, paste(tickers, "signal"))
  # Condense for easy graphing
  dfMACDCondense = melt(dfMACD, id='Date')
  # Make graph EVEN prettier
  colnames(dfMACDCondense)[3] <- "Value"
  # Plot the MACD of tickers
  plot4 <- ggplot(dfMACDCondense, aes(Date, Value, color = variable)) + 
    geom_line(size=0.5) + ggtitle("Moving Average Convergence Divergence & Signal")
  
  # Print out graphs
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  
  # rm(list = ls())
} else {
  print("Load packages failed")
}
