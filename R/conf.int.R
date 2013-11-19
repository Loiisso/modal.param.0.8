conf.int <-
function(resid, y, alpha, yhat){
  #   resid=modal.resid.cv
  # #   y=y
  #   alpha=alpha
  #  yhat=cross.modal
  #  alpha <- 0.3
  
  which.conf <- assym.int(x=resid, alpha=alpha)
  resid.minus <- sort(resid)[which.conf[1]] 
  resid.plus <- sort(resid)[which.conf[2]]
  y.minus <- y + rep(resid.minus, length(y)) # It's negative . therefore we plus it.
  y.plus <- y + rep(resid.plus, length(y))
  
  int.coverage <- length(which(yhat < y.plus & yhat > y.minus))/length(y)
  conf.mod <- resid.plus - resid.minus
  
  return(list(coverage=int.coverage, interval=conf.mod))
}
