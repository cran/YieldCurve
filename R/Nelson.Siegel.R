`Nelson.Siegel` <-
function( rate, maturity, MidTau)
  {
    a1 <- mean(c(MidTau[1],MidTau[2]))
    a2 <- mean(c(MidTau[1],MidTau[3]))
    a3 <- mean(c(MidTau[2],MidTau[3]))
    lambdaValues <- c( MidTau,a1, a2, a3)
    if(is.vector(rate)) rate <- matrix(rate, 1, length(maturity))
    if(is.data.frame(rate)) rate <- data.matrix(rate)
    factorBeta1 <- function( lambda, maturity ) {
     ( 1-exp( -lambda * maturity ) ) / ( lambda * maturity ) 
    }
    factorBeta2 <- function(lambda, maturity ) {
     ( 1-exp( -lambda * maturity ) ) / ( lambda * maturity ) - exp( -lambda * maturity ) 
    }
    beta.estimator <- function( rate, maturity, lambda )
    {
      beta <- lm( rate ~ 1 + factorBeta1(lambda,maturity) + factorBeta2(lambda,maturity) )
      betaPar <- coef(beta)
      NaValues <- na.omit(betaPar)
      if( length(NaValues)<3 ) betaPar <- c(0,0,0)
      names(betaPar) <- c("beta_0", "beta_1", "beta_2")
      EstResults <- list(Par=betaPar, Res=resid(beta))
      return(EstResults)
     }
    FinalResults <- matrix(0, nrow(rate), 4)
    colnames( FinalResults ) <- c("beta_0","beta_1","beta_2","lambda")
    j <- 1
    while(j <= nrow(rate) )
      {
        InterResults <- matrix(0, length(lambdaValues), 5)
        colnames( InterResults ) <- c("beta0","beta1","beta2","lambda","SSR")
        for( i in 1:length(lambdaValues))
          {
            lambdaTemp <- optimize(factorBeta2,interval=c(0.001,1),maturity=lambdaValues[i],maximum=TRUE)$maximum
            InterEstimation<- beta.estimator( rate[j,], maturity, lambda=lambdaTemp)
            BetaCoef <- InterEstimation$Par
	    if( BetaCoef[1]>0 & BetaCoef[1]<20)
              {
                SSR <- sum(InterEstimation$Res^2)
                InterResults[i,] <- c(BetaCoef, lambdaTemp, SSR)
              } else
            {
              InterResults[i,] <- c(BetaCoef,lambdaValues[i],100000)
            } 
          }
        BestRow <- which.min(InterResults[,5])
        FinalResults[j,] <- InterResults[BestRow,1:4]
        j <- j+1
      }
    FinalResults <- as.data.frame(FinalResults)
    return( FinalResults )
  }

