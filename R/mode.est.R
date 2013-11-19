mode.est <-
function(y, ini=T, acc=T, num.ini=20, bw = "nrd"){
  
  n.obs <- length(y)
  if (bw == "default") {bw <- 1.06*sd(y)*n.obs^(-1/5)}
  if (bw == "nrd") {bw <- bw.nrd(y)}
  if (bw == "botev") {bw <- kde.botev(y)$bw}
  if (bw == "SJ") {bw <- bw.SJ(y)}
  if (bw == "nrd0") {bw <- bw.nrd0(y)}#-1/5 or 1/5 THAT IS THE QUESTION (Looks like -1/5 is the answer)
  if (acc == T){
    acc <- 10^(-9)
  }
  # ---- Default points for mode estimation
  if (ini == T){
    num.ini <- num.ini #Should be controllable
    temp <- sort(y)
    which.obs <- c(1:num.ini)*floor(n.obs/num.ini)
    ini <- temp[which.obs]
  }
  
  mode <- c()
  mode.obj <- c()
  pot.mode <- c()
  for (i in 1:num.ini){
    pot.mode <- ini[i]
    dif <- 1
    obj <- -1
    while (dif > acc){
      old.obj <- obj
      weights <- dnorm(
        x=y, 
        mean=pot.mode,
        sd=bw
      )
      obj <- sum(weights)
      dif <- obj - old.obj
      pot.mode <- sum(weights * y)/sum(weights)
    }
    mode[i] <- pot.mode
    mode.obj[i] <- obj
  }
  mode.result <- cbind(mode.obj, mode)
  which.best <- which.max(mode.result[,"mode.obj"])
  result.mode <- mode.result[,"mode"][which.best]
  result.obj <- mode.result[,"mode.obj"][which.best]
  result <- data.frame(mode=result.mode, obj=result.obj)
  return(result)
}
