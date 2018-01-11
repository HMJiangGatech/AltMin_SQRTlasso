library(glmnet)

altmin <- function(X,y,epsilon = 0.0001,lambda.min.ratio=0.01)
{
  n <- nrow(X)
  altvar = 0.5;
  while(1)
  {
    submodel = glmnet(X,y,lambda.min.ratio = altvar*lambda.min.ratio)
    yhat = predict(submodel,X)
    yhat = yhat[,ncol(yhat)]
    newaltvar = sum((y-yhat)^2)^0.5 * 0.5
    if(abs(altvar - newaltvar) < epsilon)
      break()
    else
      altvar = newaltvar
  }
  return(altvar)
}