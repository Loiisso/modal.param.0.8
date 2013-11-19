mode.ders.estim <-
function(y, mode, ks.lib=F, kern.Smooth.lib=F, default.method=T, bw = "nrd"){
  
  n.obs <- length(y)
  if (bw == "default") {bw <- 1.06*sd(y)*n.obs^(-1/5)}
  if (bw == "nrd") {bw <- bw.nrd(y)}
  if (bw == "botev") {bw <- kde.botev(y)$bw}
  if (bw == "SJ") {bw <- bw.SJ(y)}
  if (bw == "nrd0") {bw <- bw.nrd0(y)}
  bw.der0 <- bw #THIS IS IMPORTANT FOR FUCK"S SAKE!
  bw.der3 <- bw*(n.obs^(1/5-1/11)) # hns(y, deriv.order=3) #
  warning("Experiment- derivative order is estimated wih non-canon function")
  
  #ks
  if (ks.lib == T) {
    mode.der0 <- kdde(x=y, h=bw.der0, deriv.order=0, eval.points=mode)$estimate
    mode.der3 <- kdde(x=y, h= bw.der3, deriv.order=3, eval.points=mode )$estimate
  }
  #OR
  #KernSmooth
  if (kern.Smooth.lib == T) {
    dens.der0 <- bkfe(x=y, bandwidth=bw.der0, drv=0)
    dens.der3 <- bkfe(x=y, bandwidth=bw.der3, drv=3)
  }
  #OR
  #Native method
  #USE THIS!
  if (default.method == T){
    mode.der0 <- c()
    for (i in 1:length(mode)){
      temp <- (mode[i] - y)/bw.der0 # Mistake was here
      mode.der0[i] <- (1/n.obs/bw.der0/sqrt(x=2 * pi)*sum(exp(-temp^2/2)))
    }
    
    mode.der3 <- c()
    for (i in 1:length(mode)){
      temp <- (mode[i] - y)/bw.der3 # Mistake was here
      mode.der3[i] <- (1/n.obs/bw.der3^4/sqrt(x=2 * pi)*sum((3*temp-temp^3)*exp(-temp^2/2)))
    }
  }
  
  Return.list <- list(
    bw.start=bw, 
    mode.der0=mode.der0,
    mode.der3=mode.der3
  )
  
  return(Return.list)
}
