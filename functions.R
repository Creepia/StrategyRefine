#==================CLEAN UP WORKSPACE================

# rm(list=ls())

#===================WORKING ENVIRONMENT===============

# Load or install necessary packages if necessary
want <- c("tidyquant","quantmod","dplyr", "plyr", "magrittr", "ggplot2", "scales", "reshape2", "PerformanceAnalytics","data.table")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

# Working directories
dir <- list()
dir$root <- dirname(getwd())
dir$source <- paste(dir$root,"/source",sep="")
dir$output <- paste(dir$root,"/output",sep="")
dir$result <- paste(dir$root, "/result", sep="")
lapply(dir, function(i) dir.create(i, showWarnings = F))

#===================FETCHING DATA===============

# Output a list with such structure:
# (xts data with "Open" "High" "Low" "Close" "Volume" five columns.)
# data:list - dj30  :list   - AAPL:xts
#                           - AMGN:xts
#           - nasdaq:list   - AAPL:xts
#                           - ABNB:xts
#           - sp500 :list   - A   :xts
fetch_data<-function(source_folder){
  start_time<-Sys.time()
  data<-list()
  
  folders <- dir(source_folder,full.names = T)
  for(folder in folders){
    # folder: "/source/nasdaq"
    # files:  "/source/nasdaq/AAPL.csv" "/source/nasdaq/ABNB.csv" ...
    files<-dir(folder,full.names = T)
    
    # Read files in each folder and convert it into xts file.
    # We do not need the 6th and 8th column which are "Adj Close" and "Ticker".
    # And after we get the xts file, we do not need 1st column which is "Date" anymore.
    stakes<-lapply(files,function(f){
      f<-read.csv(f)
      f<- f[,!((colnames(f) %in% c("Adj.Close","Ticker","adjusted","symbol","X")))]
      xts(f[,!(colnames(f) %in% c("date","Date"))],order.by =as.Date(f[,colnames(f) %in% c("date","Date")], "%Y-%m-%d"))
    })
    
    # Reformat folder and files to a simpler way
    # folder: "nasdaq"
    # files:  "AAPL" "ABNB" ...
    files <- basename(files) %>% strsplit(split=".",fixed=T) %>% lapply(head,1)
    folder <- basename(folder)
    
    # Name each stake in a market, package them into a list, append the list to data then name the list in data.
    names(stakes)<-files
    data<-append(data,list(stakes))
    names(data)[length(data)]<-folder
  }
  
  end_time<-Sys.time()
  paste("All data have been loaded, used",end_time-start_time,"secs.") %>% print()
  return(data)
}

download_data<-function(source_folder,stocks,from,to,set_name="default_set",update_only=F){
  start_time<-Sys.time()
  paste("Downloading",set_name,"with",length(stocks),"items.") %>% print()
  folder<-paste0(source_folder,"/",set_name)
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  lapply(stocks,function(s){
    data<-tq_get(s,get = "stock.prices",from=from,to=to)
    write.csv(data,file=paste0(folder,"/",data$symbol[1],".csv"))
  })
  end_time<-Sys.time()
  paste("All data have been downloaded, used",end_time-start_time,"secs.") %>% print()
}

#===================BASIC STRATEGIES===============
# Strategy function:
# Input a xts data with "Open" "High" "Low" "Adj Close" "Volume" five columns.
# Output a data.frame with "date" "option" "price" three columns. It shows that the timing we should buy or sell according to the strategy.
# The output need further procession to calculate the win rate.

# sma
strat_sma <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 50) stop("x should be longer than 50.")
  sma10 <- SMA(Cl, n = 10)
  sma20 <- SMA(Cl, n = 20)
  sma50 <- SMA(Cl, n = 50)
  # from 51
  for (i in 51:(n-1)) {
    if ((sma10[i - 1] < sma20[i - 1] || sma20[i - 1] < sma50[i - 1])
        && (sma10[i] >= sma20[i] && sma20[i] >= sma50[i])) {
      signal <- "buy"
    } else if ((sma10[i - 1] > sma20[i - 1] || sma20[i - 1] > sma50[i - 1])
               && (sma10[i] <= sma20[i] && sma20[i] <= sma50[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# ema
strat_ema <- function(x) {
  if (length(Cl(x)) < 150) stop("x should be longer than 150.")
  res <- NULL
  ema <- EMA(Cl(x), n = 150)
  df<-data.frame(Cl=Cl(x),ema=ema)
  df<-na.omit(df)
  test<<-df
  for (i in 150:length(df$Cl)) {
    if (df$Cl[i]>df$EMA[i]) {
      signal <- "buy"
    } else if(df$Cl[i]<df$EMA[i]) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = index(x)[i+1],
                                 option = signal,
                                 price = Op(x)[i+1]))
  }
  return(res)
}

# macd
strat_macd <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 35) stop("x should be longer than 35.")
  # MACD is a function from TTR.
  MACDS <- MACD(Cl, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsignal <- MACDS[, "signal"]
  # from 34 = (26 - 1 + 9 - 1) + 1
  for (i in 35:(n-1)) {
    if (macd[i - 1] < macdsignal[i - 1] && macd[i] > macdsignal[i]) {
      signal <- "buy"
    } else if (macd[i - 1] > macdsignal[i - 1] && macd[i] < macdsignal[i]) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# rsi
strat_rsi <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 14) stop(paste("x should be longer than 14."))
  rsi <- data.frame(RSI(Cl, 14))
  for (i in 35:(n-1)) {
    if (TF30(i,i,rsi$rsi)) {
      signal <- "buy"
    } else if (TF70(i,i,rsi$rsi))
    {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# bollinger's bands
strat_bbands <- function(x, n = 20) {
  res <- NULL

  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  nlen <- length(Cl)
  if (nlen < n) stop(paste("x should be longer than", n, "."))
  bbands <- BBands(HLC(x), n = n)
  for (i in (n + 1):(nlen-1)) {
    if (Cl[i - 1] > bbands$dn[i - 1] && Cl[i] < bbands$dn[i]) {
      signal <- "buy"
    } else if (Cl[i - 1] < bbands$up[i - 1] && Cl[i] > bbands$up[i])
    {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# dmi
strat_dmi <- function(x, n = 14){
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  nlen <- length(Cl)
  if (nlen < n) stop(paste("x should be longer than", n, "."))
  adx <- ADX(HLC(x))
  DIp <- adx[, "DIp"]  # green positive Direction Index
  DIn <- adx[, "DIn"]  # red negative Direction Index
  for (i in (n + 2):(nlen-1)) {
    if (DIp[i - 1] < DIn[i - 1] && DIp[i] > DIn[i]) {
      signal <- "buy"
    } else if (DIp[i-1] > DIn[i - 1] && DIp[i] < DIn[i])
    {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# roc
strat_roc <- function(x, n = 20){
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  nlen <- length(Cl)
  if (nlen < n) stop(paste("x should be longer than", n, "."))
  roc <- ROC(Cl, n = n)
  for (i in (n + 2):(nlen-1)) {
    if ((roc[i-1] < 0) && (roc[i] > 0)) {
      signal <- "buy"
    } else if ((roc[i-1] > 0) && (roc[i] < 0))
    {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# smi / stochastic / stc
strat_smi <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 35) stop("x should be longer than 35.")
  # MACD is a function from TTR.
  smi_result <- SMI(HLC(x), n = 13, nFast = 2, nSlow = 25, nSig = 9)
  smi <- smi_result[, "SMI"]
  smisignal <- smi_result[, "signal"]
  # from 34 = (26 - 1 + 9 - 1) + 1
  for (i in 35:(n-1)) {
    if (smi[i - 1] < smisignal[i - 1] && smi[i] > smisignal[i]) {
      signal <- "buy"
    } else if (smi[i - 1] > smisignal[i - 1] && smi[i] < smisignal[i]) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# william index
strat_wpr <- function(x, n = 20){
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  nlen <- length(Cl)
  if (nlen < n) stop(paste("x should be longer than", n, "."))
  wpr <- WPR(HLC(x), n = n)
  for (i in (n + 1):(nlen-1)) {
    if (wpr[i] < 0.2) {
      signal <- "buy"
    } else if (wpr[i] > 0.8)
    {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# sar
# Parabolic Stop And Reverse (SAR)
strat_sar <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  sar <- SAR(cbind(Hi(x), Lo(x)), accel = c(0.02, 0.2))
  for (i in 2:(n-1)) {
    if (sar[i-1]>Cl[i-1] && sar[i]<Cl[i]) {
      signal <- "buy"
    } else if (sar[i-1] < Cl[i-1] && sar[i] > Cl[i]) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# cci
strat_cci <- function(x, n = 20, c = 0.015) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  nlen <- length(Cl)
  if (nlen < 20) stop("x should be longer than 20.")
  cci <- CCI(HLC(x), n = n, c = c)
  for (i in 20:(nlen-1)) {
    if (is.na(cci[i])){
      next()
    }
    if (cci[i] <= -100) {
      signal <- "buy"
    } else if (cci[i] >= 100) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# obv
strat_obv <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- as.numeric(Cl(x))
  Op <- as.numeric(Op(x))
  n <- length(Cl)
  obv <- data.frame(OBV(Cl, Vo(x)))
  for (i in 10:(n-1)) {
    if ((obv[i,] - obv[i-4,]) > 0 && (Cl[i]-Cl[i-4]) > 0) {
      signal <- "buy"
    } else if ((obv[i,] - obv[i-4,]) < 0 && (Cl[i]-Cl[i-4]) < 0) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

#pvt
strat_pvt <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- as.numeric(Cl(x))
  Op <- as.numeric(Op(x))
  Vo <- as.numeric(Vo(x))
  pvt <- ((Cl-lag(Cl,1))/lag(Cl,1))*Vo
  n <- length(Cl)
  for (i in 9:(n-1)) {
    if ((Cl[i]-Cl[i-7]) > 0 && (pvt[i]-pvt[i-7])>0) {
      signal <- "buy"
    } else if ((Cl[i]-Cl[i-7]) < 0 && (pvt[i]-pvt[i-7])<0) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

strat_ARBR <- function(x){
  res <- NULL
  date <- index(x)
  # x <- na.omit(data.frame(x))
  # x <- na.omit(x[which(x[, 1] > 0), ])
  # x <- na.omit(x[which(x[, 5] > 0), ])
  # x <- xts(x, order.by = as.Date(rownames(x)))
  Cl <- as.vector(Cl(x))
  Op <- as.vector(Op(x))
  Hi <- as.vector(Hi(x))
  Lo <- as.vector(Lo(x))
  n <- length(Cl)
  BrC <- lag(Cl, 1)   #昨日收盘价
  list <- c()
  for(i in 2:(n-1)){
    if(Hi[i]==Op[i] || Op[i]==Lo[i] || Hi[i]==BrC[i] || BrC[i]==Lo[i]){
      list <- append(list, i)
    }
  }
  x <- x[-list, ]
  Cl <- as.vector(Cl(x))
  Op <- as.vector(Op(x))
  Hi <- as.vector(Hi(x))
  Lo <- as.vector(Lo(x))
  n <- length(Cl)
  if (n < 26) {
    print("x should be longer than 26. The stock has no records under ARBR strategy")
    return(0)
  }
  BrC <- lag(Cl, 1)   #昨日收盘价
  df_HO <- (Hi - Op)   #用于 AR, 最高价减去开盘价
  df_OL <- (Op - Lo)   #用于 AR, 开盘价减去最低价
  df_HCY <- na.omit((Hi - BrC)) #用于 BR, 最高价减去昨日收盘价
  df_CYL <- na.omit((BrC - Lo)) #用于 BR， 昨日收盘价减去最低价
  AR <- (runSum(df_HO, 26)/runSum(df_OL, 26))*100
  BR <- (runSum(df_HCY, 26)/runSum(df_CYL, 26))*100
  atr <- data.frame(ATR(x))[,2]
  price <- Cl[26]
  nrow <- 0
  date <- index(x)
  for(i in 26:(n-1)){
    if(AR[i]>150 && (Cl[i]-price)>2*atr[i]){
      signal <- "sell"
    } else if(BR[i]<AR[i] && (BR[i]<100 || AR[i]<60)){
      signal <- "buy"
      if(!is.null(res)){
        if(res[nrow,2]!=signal){
          price <- Op[i+1]
        }
      }
    } else{
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
    nrow <- nrow(res)
  }
  return (res)
}


#===================MULTI STRATEGIES===============

# pvt & stc
strat_pvtstc <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- as.numeric(Cl(x))
  Op <- as.numeric(Op(x))
  Vo <- as.numeric(Vo(x))
  pvt <- ((Cl-lag(Cl,1))/lag(Cl,1))*Vo
  n <- length(Cl)
  smi <- SMI(HLC(x), n = 13, nFast = 2, nSlow = 25, nSig = 9)
  smil <- as.numeric(smi[, 1])
  smisignal <- as.numeric(smi[, 2])
  for (i in 35:(n-1)) {
    if ((Cl[i]-Cl[i-7]) > 0 && (pvt[i]-pvt[i-7])>0 && (smi[i - 1] < smisignal[i - 1]) && (smi[i] > smisignal[i])) {
      signal <- "buy"
    } else if ((Cl[i]-Cl[i-7]) < 0 && (pvt[i]-pvt[i-7])<0 && (smi[i - 1] > smisignal[i - 1]) && (smi[i] < smisignal[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# pvt & macd
strat_pvtmacd <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- as.numeric(Cl(x))
  Op <- as.numeric(Op(x))
  Vo <- as.numeric(Vo(x))
  pvt <- ((Cl-lag(Cl,1))/lag(Cl,1))*Vo
  n <- length(Cl)
  MACDS <- MACD(Cl, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsignal <- MACDS[, "signal"]
  for (i in 35:(n-1)) {
    if ((Cl[i]-Cl[i-7]) > 0 && (pvt[i]-pvt[i-7])>0 && macd[i - 1] < macdsignal[i - 1] && macd[i] > macdsignal[i]) {
      signal <- "buy"
    } else if ((Cl[i]-Cl[i-7]) < 0 && (pvt[i]-pvt[i-7])<0 && macd[i - 1] > macdsignal[i - 1] && macd[i] < macdsignal[i]) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# pvt & sma
strat_pvtsma <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- as.numeric(Cl(x))
  Op <- as.numeric(Op(x))
  Vo <- as.numeric(Vo(x))
  pvt <- ((Cl-lag(Cl,1))/lag(Cl,1))*Vo
  n <- length(Cl)
  if (n < 50) stop("x should be longer than 50.")
  sma10 <- SMA(Cl, n = 10)
  sma20 <- SMA(Cl, n = 20)
  sma50 <- SMA(Cl, n = 50)
  for (i in 51:(n-1)) {
    if ((Cl[i]-Cl[i-7]) > 0 && (pvt[i]-pvt[i-7])>0 && (sma10[i - 1] < sma20[i - 1] || sma20[i - 1] < sma50[i - 1])
        && (sma10[i] >= sma20[i] && sma20[i] >= sma50[i])) {
      signal <- "buy"
    } else if ((Cl[i]-Cl[i-7]) < 0 && (pvt[i]-pvt[i-7])<0 && (sma10[i - 1] > sma20[i - 1] || sma20[i - 1] > sma50[i - 1])
               && (sma10[i] <= sma20[i] && sma20[i] <= sma50[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# obv & sma
strat_obvsma <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- as.numeric(Cl(x))
  Op <- as.numeric(Op(x))
  n <- length(Cl)
  obv <- data.frame(OBV(Cl, Vo(x)))
  if (n < 50) stop("x should be longer than 50.")
  sma10 <- SMA(Cl, n = 10)
  sma20 <- SMA(Cl, n = 20)
  sma50 <- SMA(Cl, n = 50)
  for (i in 51:(n-1)) {
    if ((obv[i,] - obv[i-4,]) > 0 && (Cl[i]-Cl[i-4]) > 0 && (sma10[i - 1] < sma20[i - 1] || sma20[i - 1] < sma50[i - 1])
        && (sma10[i] >= sma20[i] && sma20[i] >= sma50[i])) {
      signal <- "buy"
    } else if ((obv[i,] - obv[i-4,]) < 0 && (Cl[i]-Cl[i-4]) < 0 &&  (sma10[i - 1] > sma20[i - 1] || sma20[i - 1] > sma50[i - 1])
               && (sma10[i] <= sma20[i] && sma20[i] <= sma50[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# obv & macd
strat_obvmacd <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- as.numeric(Cl(x))
  Op <- as.numeric(Op(x))
  n <- length(Cl)
  obv <- data.frame(OBV(Cl, Vo(x)))
  MACDS <- MACD(Cl, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsignal <- MACDS[, "signal"]
  for (i in 35:(n-1)) {
    if ((obv[i,] - obv[i-4,]) > 0 && (Cl[i]-Cl[i-4]) > 0 && macd[i - 1] < macdsignal[i - 1] && macd[i] > macdsignal[i]) {
      signal <- "buy"
    } else if ((obv[i,] - obv[i-4,]) < 0 && (Cl[i]-Cl[i-4]) < 0 && macd[i - 1] > macdsignal[i - 1] && macd[i] < macdsignal[i]) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# macd & rsi
strat_macdrsi <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 35) stop("x should be longer than 35.")
  MACDS <- MACD(Cl, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsig <- MACDS[, "signal"]
  rsi <- data.frame(RSI(Cl, 14))
  for (i in 42:(n-1)) {
    if (macd[i-4]<macdsig[i-4] && macd[i]>macdsig[i] && (TF30(i-4,i,rsi$rsi) || (rsi$rsi[i]<50 && rsi$rsi[i]-rsi$rsi[i-3]>15))) {
      signal <- "buy"
    } ## else if #<# #>#
    else if (macd[i-4]>macdsig[i-4] && macd[i]<macdsig[i] && (TF70(i-4,i,rsi$rsi) || (rsi$rsi[i]>50 && rsi$rsi[i-3]-rsi$rsi[i]>15)) && strat_out(x, i)) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame( date = date[i+1],
                                  option = signal,
                                  price = Op[i+1]))
  }
  return(res)
}

# macd & parabolic stop & reverse
strat_macdsar <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 35) stop("x should be longer than 35.")
  MACDS <- MACD(Cl, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsig <- MACDS[, "signal"]
  sar <- SAR(cbind(Hi(x), Lo(x)), accel = c(0.02, 0.2))
  for (i in 42:(n-1)) {
    if (macd[i-4]<macdsig[i-4] && macd[i]>macdsig[i] && (sar[i-4]>Cl[i-4] && sar[i]<Cl[i])) {
      signal <- "buy"
    } else if (macd[i-4]<macdsig[i-4] && macd[i]>macdsig[i] && (sar[i-4]>Cl[i-4] && sar[i]>Cl[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame( date = date[i+1],
                                  option = signal,
                                  price = Op[i+1]))
  }
  return(res)
}

# macd & cci
strat_macdcci <- function(x){
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n <= 40) {
    #stop("x should be longer than 35.")
    print("The stock should be longer than 40.")
    return(res)
    
  }
  # MACD is a function from TTR.
  MACDS <- MACD(Cl, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsignal <- MACDS[, "signal"]
  cci <- data.frame(CCI(x[,2:4], n = 20, c = 0.015))[, 1]

  # from 34 = (26 - 1 + 9 - 1) + 1
  for (i in 40:(n-1)) {
    if (is.na(cci[i])){
      next()
    }
    if (macd[i - 5] < macdsignal[i - 5] && macd[i] > macdsignal[i] && cci[i] <= -100) {
      signal <- "buy"
    } else if (macd[i - 5] > macdsignal[i - 5] && macd[i] < macdsignal[i] && cci[i] >= 100) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# smi/stc, & rsi
strat_smirsi <- function(x, n = 14, nFast = 2, nSlow = 25, nSig = 9) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  nlen <- length(Cl)
  if (nlen < (nSlow+nSig)) stop(paste("x should be longer than", (nSlow+nSig), "."))
  smi <- SMI(HLC(x), n = n, nFast = nFast, nSlow = nSlow, nSig = nSig)
  smil <- smi[, 1]
  smisignal <- smi[, 2]
  rsi <- RSI(Cl, n = n)
  for (i in (nSlow+nSig+9):(nlen-1)) {
    if ((rsi[i] < 35)  && (smil[i-9] < smisignal[i-9] && smil[i] > smisignal[i])) {
      signal <- "buy"
    } else if (rsi[i] > 65 && (smil[i-4] > smisignal[i-4] && smil[i] < smisignal[i]))
    {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# stc & macd
strat_macdsmi <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 35) stop("x should be longer than 35.")
  MACDS <- MACD(Cl, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsig <- MACDS[, "signal"]
  smi <- SMI(HLC(x), n = 13, nFast = 2, nSlow = 25, nSig = 9)
  smil <- smi[, 1]
  smisignal <- smi[, 2]
  for (i in 42:(n-1)) {
    if (macd[i-6]<macdsig[i-6] && macd[i]>macdsig[i] && (smil[i-2] < smisignal[i-2] && smil[i] > smisignal[i])) {
      signal <- "buy"
    } else if (macd[i-6]>macdsig[i-6] && macd[i]<macdsig[i] && (smil[i-2] > smisignal[i-2] && smil[i] < smisignal[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame( date = date[i+1],
                                  option = signal,
                                  price = Op[i+1]))
  }
  return(res)
}

# ema & stc
# sma / ema / wma / 
# bollinger's bands & rsi
strat_bbrsi <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 20) stop("x should be longer than 20.")
  rsi <- RSI(Cl, 14)
  bbands <- BBands(x[,2:4], n = 20)
  # from 51
  for (i in 35:(n-1)) {
    if (rsi[i] < 35 && Cl[i - 2] > bbands$dn[i - 2] && Cl[i] < bbands$dn[i]) {
      signal <- "buy"
    } else if (rsi[i] > 65 && (Cl[i - 2] < bbands$up[i - 2] && Cl[i] > bbands$up[i]) && strat_out(x, i)) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# dmi & stc
strat_dmismi <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 35) stop("x should be longer than 35.")
  smi_result <- SMI(HLC(x), n = 13, nFast = 2, nSlow = 25, nSig = 9)
  smi <- smi_result[, "SMI"]
  smisignal <- smi_result[, "signal"]
  adx <- ADX(HLC(x))
  DIp <- adx[, "DIp"]
  DIn <- adx[, "DIn"]
  for (i in 40:(n-1)) {
    if ((smi[i - 4] < smisignal[i - 4] && smi[i] > smisignal[i]) && (DIp[i - 4] < DIn[i - 4] && DIp[i] > DIn[i])) {
      signal <- "buy"
    } else if ((smi[i - 4] > smisignal[i - 4] && smi[i] < smisignal[i]) && (DIp[i - 4] > DIn[i - 4] && DIp[i] < DIn[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = date[i+1],
                                 option = signal,
                                 price = Op[i+1]))
  }
  return(res)
}

# dmi & macd/ema   
strat_dmimacd <- function(x) {
  res <- NULL
  date <- index(x)
  Cl <- Cl(x)
  Op <- Op(x)
  n <- length(Cl)
  if (n < 35) stop("x should be longer than 35.")
  MACDS <- MACD(Cl, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsig <- MACDS[, "signal"]
  adx <- ADX(HLC(x))
  DIp <- adx[, "DIp"]
  DIn <- adx[, "DIn"]
  for (i in 42:(n-1)) {
    if ((macd[i-4]<macdsig[i-4] && macd[i]>macdsig[i]) && (DIp[i - 4] < DIn[i - 4] && DIp[i] > DIn[i])) {
      signal <- "buy"
    } else if ((macd[i-4]>macdsig[i-4] && macd[i]<macdsig[i]) && (DIp[i - 4] > DIn[i - 4] && DIp[i] < DIn[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame( date = date[i+1],
                                  option = signal,
                                  price = Op[i+1]))
  }
  return(res)
}

# sell out conditions
strat_out <- function(x, i) {
  date <- index(x)
  Cl <- data.frame(Cl(x))[,1] #close price
  Op <- data.frame(Op(x))[,1] #open price
  Hi <- data.frame(Hi(x))[,1] #high price
  Lo <- data.frame(Lo(x))[,1] #low price
  n <- length(Cl) 
  if (i < 35) stop("x should be longer than 34.")
  # 断头铡刀, 阴线包住三根均线，5，10，30日均线
  sma5 <- SMA(Cl, n = 5)
  sma10 <- SMA(Cl, n = 10)
  sma30 <- SMA(Cl, n = 30)
  count <- rep(0, 11)
  # 阴烛
  if (Cl[i] < Op[i]){
    # 断头铡刀
    if (sma5[i] <= Op[i] && sma10[i] <= Op[i] && sma30[i] <= Op[i]
        && sma5[i] >= Cl[i] && sma10[i] >= Cl[i] && sma30[i] >= Cl[i]) {
      count[1] <- count[1] + 1
    } 
    if (Cl[i-1]>Op[i-1] && (Op[i]-Cl[i])>(Cl[i-1]-Op[i-1])){ # 阴包阳
      count[2] <- count[2] + 1
    } 
    if (Cl[i-1]>Op[i-1] && Op[i] > Cl[i-1]){  #乌云盖顶，前一日为阳烛，今日高开低走
      count[3] <- count[3] + 1
    } 
    if (abs(Cl[i-4]-Cl[i])<1.5 && Op[i-3] > Cl[i-3] && Op[i-3] < min(Cl[i-4], Op[i-4])){ # 相差小于1.5认为股价回复到原来水平
      count[4] <- count[4] + 1
    } 
    if (Cl[i-1] > Op[i-1] && (Cl[i-1]-Op[i-1]) > (Op[i]-Cl[i])){  # 高位孕线，前一日为阳烛，阳烛的范围大于今日阴烛的范围
      count[5] <- count[5] + 1
    } 
    if (Cl[i-2] > Op[i-2] && Cl[i-1] > Op[i-1] && 
        (Hi[i-1]-Lo[i-1])>2*(Cl[i-1]-Op[i-1])){ # 黄昏之星，前两日为阳烛，第二日最高价与最低价的差明显大于收盘价开盘价之差，第三日即今日为阴烛
      count[6] <- count[6] + 1
    } 
    if (Cl[i-2] < Op[i-2] && Cl[i-1] < Op[i-1] && Cl[i-3] > Op[i-3]){  # 三只乌鸦，连着三日阴烛，而前一天为阳烛
      count[7] <- count[7] + 1
    } 
    if (Cl[i-1] > Op[i-1] && Op[i] >= Cl[i-1] && (Hi[i]-Op[i])>2*(Op[i]-Cl[i]) && (Op[i]-Cl[i])>(Cl[i]-Lo[i])){
      count[8] <- count[8] + 1
    }
  }
  # 阳烛
  if (Cl[i] >= Op[i]){
    # 吊颈线, 在上涨的情况下，开盘价相较最低价的差至少是开盘收盘价之差的2倍以上，才有卖出信号
    if (Cl[i] > Cl[i-1] && (Op[i]-Lo[i]) >= 2*(Cl[i]-Op[i])){
      count[9] <- count[9] + 1
    } 
    if (abs(Cl[i-4]-Cl[i])<1.5 && Op[i-3] > Cl[i-3] && Op[i-3] < min(Cl[i-4], Op[i-4])){ # 相差小于1.5认为股价回复到原来水平
      count[10] <- count[10] + 1
    } 
    if ((Hi[i]-Cl[i])>=2*(Cl[i]-Op[i]) && Cl[i-1] > Op[i-1] && Op[i] >= Cl[i-1] && (Cl[i]-Op[i])>(Op[i]-Lo[i])){  #射击之星
      count[11] <- count[11] + 1
    }
  }
  # print(count)
  if (sum(count) >= 2){return(1)}
  else {return(0)}
}

strat_macd_rsi_ema <- function(x){
  if (length(Cl(x)) < 150) stop("x should be longer than 150.")
  res=NULL
  MACDS <- MACD(Cl(x), nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  macd <- MACDS[, "macd"]
  macdsignal <- MACDS[, "signal"]
  rsi <- RSI(Cl(x), 14)
  ema <- EMA(Cl(x),150)
  df<-data.frame(Cl=Cl(x),macd=macd,macdsignal=macdsignal,rsi=rsi,ema=ema)
  df<-na.omit(df)
  test<<-df
  for(i in 150:length(df$Cl)){
    if(df$Cl[i]>df$EMA[i] && df$rsi[i]>50 && (df$macd[i - 1] < df$signal[i - 1] && df$macd[i] > df$signal[i])){
      signal <- "buy"
    }else if (df$Cl[i]<df$EMA[i] && df$rsi[i]<50 && (df$macd[i - 1] > df$signal[i - 1] && df$macd[i] < df$signal[i])) {
      signal <- "sell"
    } else {
      next()
    }
    res <- rbind(res, data.frame(date = index(x)[i+1],
                                 option = signal,
                                 price = Op(x)[i+1]))
  }
  return(res)
}

#==========APPLY CLOSURE TO STRAT_FUNCTIONS=========

# check_invalid_closure <- function(FUN){
#     function(x,...){
#     if (!any(class(x) %in% c("xts", "zoo"))) {
#       stop("The class of input should be 'xts' or 'zoo'.")
#     }
#     FUN(x,...)
#     }
# }
# # Add this closure to all the functions starts with "start_".
# strat<-sapply(ls(pattern="^strat_.?"),check_invalid_closure)
# names(strat)<-substr(names(strat),7,100000)

#================OTHER STATIC FUNCTIONS=============

# RSI signal function, change the parameter in necessary 
TF70 <- function(i, j, x){
  for(a in i:j){
    if(x[a] > 65){
      return (1)
    }else {
      return (0)
    }
  }
}


TF30 <- function(i, j, x){
  for(a in i:j){
    if(x[a] < 35){
      return (1)
    }else {
      return (0)
    }
  }
}

AND_Indicator <- function(...){
  funcs<-c(...)
  function(x){
    sigs<-data.frame()
    sigs<-lapply(funcs,function(f) f(x))
    test<<-sigs
    common_dates_options <- Reduce(intersect, lapply(sigs, function(x) paste(x$date, x$option)))
    res <- lapply(sigs, function(x) x[paste(x$date, x$option) %in% common_dates_options, ])
    return(data.frame(res[1]))
  }
}

#===================RATES CALCULATION===============

# Input the data.frame from a strategy function.
# Output a reduced data.frame which "buy" signals and "sell" signals appear iteratively.
perf <- function(record){
  res <- NULL
  leng <- length(record$option)
  start <- 1
  # Find the first "buy" signal
  # After this while, type_trade[start] must be "buy" otherwise the function returns null.
  while (record[start,2] != "buy"){
    start <- start + 1
    if(start >= leng){
      return(res)
    }
  }
  # Find the signals after every changing time and add them to res.
  res <- record[start,]
  for(i in start:leng){
    if(i==start || record[i-1, 2]==record[i, 2]){
      next()
    }
    res <- rbind(res, record[i,])
  }
  return(res)
}

# ret function will return transaction details. a transaction consists of a buy and a sell;
# if the last row of record is "buy", which means there is no "sell" matches "buy".
# it will use the latest close price to represent "sell" price param,
# record is the return value of perf function above, stock should be the particular stock in calculating
ret <- function(record){
  ret <- NULL
  leng <- nrow(record)
  if(leng != 1){
    for (i in seq(1, leng - 1, 2)){
      buy_price = record[i, 3]
      sell_price = record[i + 1, 3]
      trading_cost = buy_price * 0.001 + sell_price * 0.001 # Trading cost is the 0.1% here. Like handling fee? I am not clear about this...
      return = sell_price / (buy_price+trading_cost) - 1 # This should be return_rate
      buy_date = record[i, 1]
      sell_date = record[i + 1, 1]
      date_change = as.numeric(sell_date) - as.numeric(buy_date)
      ret <- rbind(ret, data.frame(buy_date = buy_date,
                                   sell_date = sell_date,
                                   buy_price = buy_price,
                                   sell_price = sell_price,
                                   change = sell_price - buy_price,
                                   return = return,
                                   ann_return = (return/date_change)*252,
                                   trading_cost = trading_cost))
    }}
  return(ret)
}

# This function will be rewrite
# param: stock pool, trading strategy function
# stock_pool is a list with xts datas.
final <- function(stock_pool, FUN, threshold=0.05,save=F,...){
  winrate <- NULL
  FUN <- match.fun(FUN)
  length <- length(stock_pool)
  last <- names(tail(stock_pool,1))
  stock_names <- names(stock_pool)
  num <- length
  i<-1
  # please replace stock_pool to dj30_pool or nasdaq_pool etc.
  for(stock in stock_pool){
    name<-stock_names[i]
    i<-i+1
    iterim <- FUN(stock)
    # whether there is no signal or only 1 signal (only 1 "buy" signal)
    if(all(iterim==0) || is.null(iterim) || nrow(perf(iterim))==1 || is.null(perf(iterim))){
      print(paste("the stock: ", name, "has no full transaction signals under current selected trading strategy"))
      num <- num - 1
      winrate <- rbind(winrate, data.frame(win_num = 0,
                                           trans_num = 0,
                                           winrate = 0,
                                           return = 0,
                                           ann_return = 0))
      if(name==last){
        winrate <- rbind(winrate, data.frame(
          win_num = sum(winrate$win_num),
          trans_num = sum(winrate$trans_num),
          winrate = sum(winrate$win_num) / sum(winrate$trans_num),
          return = sum(winrate$return) / num,
          ann_return = sum(winrate$ann_return) / num
        ))
        break
      }
      next()
    }
    
    re <- ret(perf(iterim))
    # re <- ret(perf(iterim), stock) # consider current value, even there is no sell signal now
    
    win_num <- length(which(re$return > threshold))
    
    ann_return <- mean(re$ann_return)
    return <- mean(re$return)
    trans_num <- nrow(re)
    
    winrate <- rbind(winrate, data.frame(win_num = win_num,
                                         trans_num = trans_num,
                                         winrate = win_num / trans_num,
                                         return = return,
                                         ann_return = ann_return))
    if(name==last){
      winrate <- rbind(winrate, data.frame(
        win_num = sum(winrate$win_num),
        trans_num = sum(winrate$trans_num),
        winrate = sum(winrate$win_num) / sum(winrate$trans_num),
        return = sum(winrate$return) / num,
        ann_return = sum(winrate$ann_return) / num
      ))
    }
    
  }
  # add name to winrate
  rownames(winrate) <- c(stock_names, "overall")
  return(winrate)
}

# Calculates the return rate for a stock with a particular strat_FUN
sing.stock <- function(x, FUN, ...){
  FUN <- match.fun(FUN)
  return(ret(perf(FUN(x))))
}

# The input is the output of sing.stock?
stat_table <- function(x,threshold=0.05){
  result <- data.frame(transaction = length(x[,1]),
                   win_transaction = length(which(x[,6]>threshold)),
                   win_rate = mean(x[,6]>threshold),
                   average_return = mean(x[,6]),
                   average_ann_return = mean(x[,7]))
  rownames(result) <- "result"
  return(result)
}

# Not very clear of this. But I fixed the function.
result_table <- function(stock,strat_FUN){
  data.frame(Sharpe = SharpeRatio(stock, FUN = "StdDev"), 
             alpha = CAPM.alpha(stock, Return.calculate(stock$Open, method = "discrete")), 
             beta = CAPM.beta(stock, Return.calculate(stock$Open, method = "discrete"))) %>% 
    cbind(stat_table(sing.stock(stock,strat_FUN)), .) %>% 
    round(4)
}

