predict.cv <-
function(formula, data , n.tests, optim.call, ...) {
  #     data <- data
  #     formula <- formula 
  #     optim.call <- "rq"
  # n.tests <- 10
  sample <- sample(1:nrow(data), size=nrow(data))
  
  length.test <- round(nrow(data)/n.tests)
  test.vec <- c()
  for ( j in 1:n.tests){
    test.vec <- c(test.vec, rep(j, length.test))
  }
  
  add.smth <- c()
  if (nrow(data) - length.test*n.tests > 0 ){
    add.smth <- rep(1:n.tests, nrow(data) - length.test*n.tests)
  }   
  test.frame <- data.frame(which.obs=sample , test.group=c(test.vec,add.smth)[1:length(sample)])
  
  y.cross <- double(nrow(data))
  for (i in 1:n.tests){
    which.test <- test.frame$which.obs[test.frame$test.group == i]
    print(paste("cross",i))
    result <- do.call(
      optim.call, 
      list(formula=formula, data=data[-which.test,], ...)
    )
    y.cross[which.test] <- predict(object=result, newdata=data[which.test,])
    #plot(density(y.cross - log(data$price))
    #lines(density(y.cross - log(data$price)), col = "red")
    #lines(density(y.cross - log(data$price)), col = "green")
  }
  return(y.cross)
}
