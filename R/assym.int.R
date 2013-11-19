assym.int <-
function(x, alpha){
  #   x=resid
  #   alpha=alpha
  #   x <- resid
  #   
  x <- sort(x)
  dens.x <- kde(x, eval.points=x)$estimate
  which.max <- which(dens.x == max(dens.x))
  
  n <- length(x)
  diff <- round(
    n*(alpha)
  )
  k1 <- round(which.max/2)
  warning("Not sure where to start...")
  k2 <- k1 + diff
  
  for (i in 1:length(x)){
    #i=1
    check.1 <- 0
    check.2 <- 0
    
    dens.k1 <- kde(x=x , eval.points=x[k1])$estimate
    dens.k2 <- kde(x=x , eval.points=x[k2])$estimate
    dens.k1plus <-  kde(x=x , eval.points=x[k1 + 1])$estimate
    dens.k2plus <- kde(x=x , eval.points=x[k2 + 1])$estimate
    dens.k1minus <-  kde(x=x , eval.points=x[k1 - 1])$estimate
    dens.k2minus <- kde(x=x , eval.points=x[k2 - 1])$estimate
    
    if (dens.k1 < dens.k2 & dens.k1plus < dens.k2plus){
      k1 <- k1 + 1
      k2 <- k2 + 1
      check.1 <- 1
    }
    if ( dens.k1 > dens.k2 & dens.k1minus > dens.k2minus){
      k1 <- k1 - 1 
      k2 <- k2 - 1
      check.2 <- 1
    }
    if (check.1 == 0 & check.2 == 0){
      break
    }
  }
  return(c(k1,k2))
}
