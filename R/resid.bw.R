resid.bw <-
function(model.matrix, const=T, mode.der0, mode.der3){
  
  n.obs <- nrow(model.matrix)
  X=model.matrix
  
  L <- t(X) %*% X/n.obs * mode.der0 #MATRIX MULT CHECK IT
  K <- apply(X=X, MARGIN=2, FUN=mean) * mode.der3 #MATRIX MULT CHECK IT
  compl.calc <- (6/4/pi^(1/2)/n.obs/(t(K) %*% ginv(L) %*% K))
  h <- compl.calc^(1/7)
  return(h)
}
