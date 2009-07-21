Srates <- function( betaCoeff, lambdaValues, maturity, whichRate="Forward" ){

  beta1Spot <- function( maturity, tau1 )
    {
      beta1S <- (1 - exp( - maturity / tau1 ) ) / ( maturity / tau1)
      
    }

  beta2Spot <- function( maturity, tau1 )
    {
      beta2S <- ( ( 1 - exp(-maturity / tau1 ) ) / ( maturity / tau1)  - exp( - maturity / tau1 ) )
      
    }


  beta3Spot <- function( maturity, tau2 )
    {
    beta3S <-  ( ( 1 - exp(-maturity / tau2 ) ) / ( maturity / tau2)  - exp( - maturity / tau2 ) )
    
  }

  beta1Forward <- function( maturity, tau1 )
    {

      beta1F <-  exp( - maturity / tau1 ) 
     
    }


  beta2Forward <- function( maturity, tau1 )
    {
      beta2F <-  exp(-maturity / tau1 )  * ( maturity / tau1)  
     
    }

  beta3Forward <- function( maturity, tau2 )
    {
      beta3F <-  exp(-maturity / tau2 ) * ( maturity / tau2)  
     
    }

  if(is.vector(betaCoeff)) betaCoeff <- matrix( betaCoeff, 1, 4)
  betaCoeff <- data.matrix( betaCoeff )
  if(is.vector(lambdaValues)) lambdaValues <- matrix( lambdaValues, 1, 2)
  lambdaValues <-  data.matrix(lambdaValues)
  

  if( whichRate == "Forward" )
    {
      CurveForward <- matrix(0,nrow(betaCoeff), length(maturity) )
      colnames(CurveForward) <- maturity
      for(i in 1:nrow(betaCoeff))
        {
          CurveForward[i,] <- betaCoeff[i,1] + betaCoeff[i,2] * beta1Forward ( maturity, lambdaValues[i,1] ) + betaCoeff[i,3] * beta2Forward ( maturity, lambdaValues[i,1] ) + betaCoeff[i,4] * beta3Forward ( maturity, lambdaValues[i,2] )
        }
      return( CurveForward )
    }

  if( whichRate == "Spot" )
    {
       CurveSpot <- matrix(0,nrow(betaCoeff), length(maturity) )
       colnames(CurveSpot) <- maturity
       for(i in 1:nrow(betaCoeff))
        {
          CurveSpot[i,] <- betaCoeff[i,1] + betaCoeff[i,2] * beta1Spot ( maturity, lambdaValues[i,1] ) + betaCoeff[i,3] * beta2Spot  ( maturity, lambdaValues[i,1] ) + betaCoeff[i,4] * beta3Spot ( maturity, lambdaValues[i,2])
        }
       return( CurveSpot )
    }
              
}
