param.mod <-
function(
  formula, data, max.iter=500, conv.limits=10^(-9), 
  bw = "nrd", bw.mode.ders.estim=NULL, bw.mode.est=NULL, bw.precise.h=F, scale=2,
  start.rlm.control=list(),
  mode.ders.estim.control=list(),
  mode.est.control=list(),
  ...){
  
  if((!is.null(bw)) & is.null(bw.mode.ders.estim)){
    mode.est.control$bw <- bw
  } else if(!is.null(bw.mode.ders.estim)) {
    mode.est.control$bw <- bw.mode.est
  }
  
  if((!is.null(bw)) & is.null(bw.mode.ders.estim)){
    mode.ders.estim.control$bw <- bw
  } else if(!is.null(bw.mode.ders.estim)){
    mode.ders.estim.control$bw <- bw.mode.ders.estim
  }
  data <- get_all_vars(formula=formula, data=data)
  #1  Resid evaluation process
  rlm.reg <- do.call(
    "rlm",
    c(list(formula=formula,
      data=data,
      x.ret=T,
      y.ret=T),start.rlm.control)
    )
  i.residuals <- rlm.reg$residuals
  
  #2 Mode estimation process
  mode.est.control$y <- i.residuals
  normal.mode <- do.call(mode.est, mode.est.control)
  #3 Estimation of derivatives of density in mode
  # TODO - place mode and i.residuals choice in main function. Don't forget "x" for resid.bw
  mode.ders.estim.control$y <- i.residuals
  mode.ders.estim.control$mode <- normal.mode
  
  mode.ders <- do.call(mode.ders.estim, mode.ders.estim.control)
  # Copied, might work
  #4 Getting "special" (that's the whole point of method in general) width of window
  # This "h" is VERY important ! it is the only thing that is passed down to iterations beneath!
  # Get it wrong and everything goes to the realm of biased and damned
  h <- resid.bw( 
    model.matrix=rlm.reg$x, 
    mode.der0=mode.ders$mode.der0, 
    mode.der3=mode.ders$mode.der3
  )
  h <- h/scale
  warning("Experiment here!: bandwidth is divided by scale - optimal for SJ and botev estimations , 
          making estimates more precise BUT this is only theoretical, works only on belief,
          DO NOT OVERDO - 2 times is quite enough")
  
  density.at.resids <- dnorm(x=i.residuals ,0 ,sd=h) # This is here for Convergence
  
  best.fit <- modal.estim(
    formula=formula, data=data, 
    start.density=density.at.resids, start.h=h, 
    max.iter=max.iter, conv.limits=conv.limits,
    mode.est.control=mode.est.control,
    mode.ders.estim.control=mode.ders.estim.control,...
  )
  
  return(best.fit)
}
