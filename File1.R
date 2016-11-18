if(require(quantmod) && require(ggplot2) && require(reshape2)){
  
  # TODO
  # Cleanup Variable Names
  # Add declaration for MACROS
  # Get ATR to work
  
  # CLEANUP
  graphics.off()
  rm(list = ls())
  
  # MACROS
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
  getsub <- c(1)
  # Setting up vector to extract correctly
  for(i in 0:(numstock-1)) {
    getsub <- c(getsub, 2+i*6)
  }
  
  # Plot the Opening values 
  df <- data.frame(Date=index(LargeSet), LargeSet, row.names=NULL)
  df <- df[, getsub]
  df2 = melt(df, id='Date')
  plot1 <- ggplot(df2, aes(Date, value, color = variable)) + geom_line(size=1)
  
  # Test stuff
  testATR <- do.call(merge, lapply(tickers, function(x) ATR(get(x), 10)))
  print(testATR)
  df3 <- data.frame(Date=index(testATR), testATR, row.names=NULL)
  df3 <- df3[, c(1, 3, 7)]
  df4 = melt(df3, id='Date')
  ggplot(df4, aes(Date, value, color = variable)) + geom_line(size=1)
  
  summary(plot1)

  plot1
} else {
  print("Load packages failed")
}
