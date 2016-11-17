if(require(quantmod) && require(ggplot2) && require(reshape2)){
  
  # CLEANUP
  graphics.off()
  rm(list = ls())
  
  # MACROS
  tickers <- c("YHOO","AAPL","IWM","SMH","OIH","XLY")
  month <- 11
  day <- 1
  

  
  numstock <- length(tickers)
  datefrom <- paste("2016-", month, "-", "0", day, sep = '', collapse='')
  dateto <- paste(Sys.Date(), sep = '', collapse='')
  getSymbols(tickers, from=datefrom, to=dateto)
  LargeSet <- do.call(merge, lapply(tickers, function(x) get(x)))
  
  for(i in 1:numstock) {
    # print(LargeSet[, (i*6)-5])
  }
    
  df <- data.frame(Date=index(LargeSet), LargeSet, row.names=NULL)
  df2 = melt(df, id='Date')
  ggplot(df2, aes(Date, value, color = variable)) + geom_line(size=1)
} else {
  print("Load packages failed")
}