library(glmnet)

altmin <- function(X,y,epsilon = 0.0001,lambda.min.ratio=0.01,verbose = FALSE)
{
  n <- nrow(X)
  altvar = 1;
  t = 0
  while(1)
  {
    t = t+1
    submodel = glmnet(X,y,lambda.min.ratio = altvar*lambda.min.ratio)
    yhat = predict(submodel,X)
    yhat = yhat[,ncol(yhat)]
    newaltvar = sum((y-yhat)^2)^0.5 
    if(verbose)
      cat("Phase",t,"Error",newaltvar/n^0.5,"\n")
    if(abs(altvar - newaltvar) < epsilon)
      break()
    else
      altvar = newaltvar
  }
  beta <- as.matrix(submodel$beta)
  return(beta[,ncol(beta)])
}