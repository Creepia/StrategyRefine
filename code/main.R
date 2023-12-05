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

#=======================MAIN=====================
# You need to set the environment and load the functions first.
data<-fetch_data(dir$source)

ema_dj30 <- final(data$dj30,strat_ema,threshold = 0.05)
macdrsiema_dj30 <- final(data$dj30,strat_macd_rsi_ema,threshold = 0.05)
macd_dj30 <- final(data$dj30,strat_macd,threshold = 0.05)
pvtsma_dj30 <- final(data$dj30,strat_pvtsma,threshold = 0.05)
pvtsma2_dj30 <- final(data$dj30,AND_Indicator(strat_pvt,strat_sma),threshold = 0.05)

