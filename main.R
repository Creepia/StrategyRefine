#=======================MAIN=====================
# You need to set the environment and load the functions first.
download_data(dir$source,c("aapl","v","meta"),from="2001-01-01",to="2023-12-10")
data<-fetch_data(dir$source)


ema_dj30 <- final(data$dj30,strat_ema,threshold = 0.05)
macdrsiema_dj30 <- final(data$dj30,strat_macd_rsi_ema,threshold = 0.05)
macd_dj30 <- final(data$dj30,strat_macd,threshold = 0.05)
pvtsma_dj30 <- final(data$dj30,strat_pvtsma,threshold = 0.05)
pvtsma2_dj30 <- final(data$dj30,AND_Indicator(strat_pvt,strat_sma),threshold = 0.05)

