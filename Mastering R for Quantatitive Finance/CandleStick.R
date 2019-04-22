
### CandleStick Chart ###

DrawChart <- function(time_frame_in_minutes, number_of_candles = 25, l = 315.5, u = 316.5){
  OHLC <- matrix(NA, 4, number_of_candles)
  OHLC[, number_of_candles] <- hhi
  dev.new(width = 30, height = 15)
  par(bg = rgb(0.9,0.9,0.9))
  plot(x = NULL, y = NULL, xlim = c(1, number_of_candles + 1),
       ylim = c(l, u), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  abline(h = axTicks(2), v = axTicks(1), col=rgb(0.5,0.5,0.5), lty = 3)
  axis(1, at = axTicks(1), las =1, cex.axis =0.6,
       labels = Sys.time() - (5:0)*time_frame_in_minutes)
  axis(2, at = axTicks(2), las = 1, cex.axis = 0.6)
  box()
  
  allpars=par(no.readnonly=TRUE)
  while(TRUE){
    start_ <- Sys.time()
    while(as.numeric(difftime(Sys.time(), start_, units="mins")) < time_frame_in_minutes){
      OHLC[4, number_of_candles] <- get_price()
      OHLC[2, number_of_candles] <- max(OHLC[2, number_of_candles], OHLC[4, number_of_candles])
      OHLC[3, number_of_candles] <- min(OHLC[3, number_of_candles], OHLC[4, number_of_candles])
      frame()
      par(allpars)
      abline(h = axTicks(2), v = axTicks(1), col = rgb(0.5,0.5,0.5), lty=3)
      axis(1, at = axTicks(1), las = 1, cex.axis=0.6, labels = Sys.time() - (5:0)*time_frame_in_minutes)
      box()
      for(i in 1:number_of_candles){
        polygon(c(i, i+1, i+1,i),
                c(OHLC[1,i], OHLC[1,i], OHLC[4,i], OHLC[4,i]),
                col = ifelse(OHLC[1,i] <= OHLC[4,i], rgb(0,0.8,0), rgb(0.8,0,0)))
        lines(c(i+1/2, i+1/2), c(OHLC[2,i], max(OHLC[1,i], OHLC[4,i])))
        lines(c(i+1/2, i+1/2), c(OHLC[3,i], min(OHLC[1,i], OHLC[4,i])))
        
      }
      abline(h = OHLC[4, number_of_candles], col = "green", lty = "dashed")
    }
    OHLC <- OHLC[, 2:number_of_candles]
    OHLC <- cbind(OHLC, NA)
    OHLC[1, number_of_candles] <- OHLC[4, number_of_candles-1]
  }
}

# 작동안됨 
DrawChart(30, 50)