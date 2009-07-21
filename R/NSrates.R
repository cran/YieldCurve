`NSrates` <-
function ( betaCoeff, lambdat, maturity )
{
  if(is.vector(betaCoeff)) betaCoeff <- matrix( betaCoeff, 1, 3)
  betaCoeff <- data.matrix( betaCoeff )
  yield.curve <- matrix(0,nrow(betaCoeff), length(maturity) )
  factorBeta1 <- function( lambda, maturity ) 
  {
   ( 1-exp( -lambda * maturity ) ) / ( lambda * maturity ) 
  }
  factorBeta2 <- function(lambda, maturity ) 
  {
   ( 1-exp( -lambda * maturity ) ) / ( lambda * maturity ) - exp( -lambda * maturity ) 
  }
  for (i in 1:nrow(betaCoeff) ) 
   {
    yield.curve[i,] <- betaCoeff[i,1] * rep( 1, length(maturity) ) + betaCoeff[i,2] * factorBeta1(lambdat, maturity) +
    betaCoeff[i,3] * factorBeta2(lambdat, maturity )
   }
  colnames(  yield.curve ) <- as.character(maturity)
  return(yield.curve)
} 

