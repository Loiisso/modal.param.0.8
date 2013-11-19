modal.estim <-
function(
  formula, data, start.density, start.h, conv.limits, precise.h=F, 
  max.iter=500,
  mode.est.control=list(),
  mode.ders.estim.control=list(),...
){
  Q.modal.iter <- c()  
  fits <- list()
  bw.vec <- c()
  density.at.resids <- start.density
  h <- start.h
  for (i in 1:max.iter) {
    #i=1
    print(i)
    # Density alg -------------------------------------------------------------
    weighted.fit <- do.call("glm", args=list(
      formula=formula,
      data=na.omit(data.frame(data,density.at.resids)), # NA.action!
      weights=density.at.resids,
      ...# This is here due to enviroment problems
    )
    )
    
    i.residuals <- weighted.fit$residuals
    Q.modal.iter[i] <- sum(dnorm(x=i.residuals ,0 ,sd=start.h))
    # Should we stop?
    if (i > 1){
      if (Q.modal.iter[i]-Q.modal.iter[i-1] < conv.limits) {
        print("Converged")
        break
      }
    }
#     if (precise.h == T){
#       normal.mode <- mode.est(
#         y=i.residuals, 
#         ini=mode.est.ini, tune=mode.est.tune, acc=mode.est.acc, num.ini=mode.est.num.ini,
#         bw=bw
#       )
#       #2
#       mode.ders <- mode.ders.estim(y=i.residuals, default.method=T, ks.lib= F, mode=normal.mode$mode, bw=bw) # Copied, might work
#       #3 This "h" is VERY important ! it is the only thing that is passed down to iterations beneath!
#       # Get it wrong and everything goes to the realm of biased and damned
#       h <- resid.bw( 
#         model.matrix=rlm.reg$x, 
#         mode.der0=mode.ders$mode.der0, 
#         mode.der3=mode.ders$mode.der3
#       )/scale
#     }
    density.at.resids <- dnorm(x=i.residuals ,0 ,sd=h)
  }
  best.fit <- weighted.fit #fits[[which(Q.modal.iter == max(Q.modal.iter))[1]]] Maybe max me?
  best.fit$bw <- h
  return(best.fit)
}
