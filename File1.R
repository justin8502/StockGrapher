if(require(quantmod) && require(ggplot2) && require(reshape2)){
  
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
  
  # Vector to extract the Open values of Stocks
  # We want the 1st column (the dates)
  getsubopen <- c(1)
  getsubatr <- c(1)
  # Setting up vector to extract correctly
  for(i in 0:(numstock-1)) {
    getsubopen <- c(getsubopen, 2+i*6)
    if(i == 0){
      getsubatr <- c(getsubatr, 3)
    } else {
      getsubatr <- c(getsubatr, 3+i*4)
    }
  }
  
  # Plot the Opening values 
  df <- data.frame(Date=index(LargeSet), LargeSet, row.names=NULL)
  df <- df[, getsubopen]
  df2 = melt(df, id='Date')
  plot1 <- ggplot(df2, aes(Date, value, color = variable)) + geom_line(size=1)
  
  # Date to record from
  datefrom <- paste("2016-", month-1, "-", "0", day, sep = '', collapse='')
  # Date to record to (default current date)
  dateto <- paste(Sys.Date(), sep = '', collapse='')
  # Retrieve data
  getSymbols(tickers, from=datefrom, to=dateto)
  # Test stuff
  testATR <- do.call(merge, lapply(tickers, function(x) ATR(get(x), 10)))

  row.names(testATR) <- "Date"
  testATR <- subset(testATR, index(testATR) >= paste("2016-", month, "-", "0", day, sep = '', collapse=''))
  print(testATR)
  df3 <- data.frame(Date=index(testATR), testATR, row.names=NULL)
  df3 <- df3[, getsubatr]
  df4 = melt(df3, id='Date')
  plot2 <- ggplot(df4, aes(Date, value, color = variable)) + geom_line(size=1)
  
  # summary(plot1)
  
  print(plot1)
  print(plot2)
} else {
  print("Load packages failed")
}
