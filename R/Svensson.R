`Svensson` <-
function( rate, maturity, Tau1 = c(3,12), Tau2 = c(60,120))
  {

    beta1Spot <- function( maturity, Tau1 )
    {
      (1 - exp( - maturity / Tau1 ) ) / ( maturity / Tau1)
     }
    beta2Spot <- function( maturity, Tau1 )
    {
      ( ( 1 - exp(-maturity / Tau1 ) ) / ( maturity / Tau1)  - exp( - maturity / Tau1 ) )
     }
    beta3Spot <- function( maturity, Tau2 )
    {
     ( ( 1 - exp(-maturity / Tau2 ) ) / ( maturity / Tau2)  - exp( - maturity / Tau2 ) )
    }
    beta.estimator <- function( rate, maturity, Tau1=Tau1Temp, Tau2=Tau2Temp )
      {
        beta <- lm( rate ~ 1 + beta1Spot(maturity,Tau1) + beta2Spot(maturity,Tau1)+ beta3Spot(maturity,Tau2) )
        betaPar <- coef(beta)
        NaValues <- na.omit(betaPar)
        if( length(NaValues)<4 ) betaPar <- c(0,0,0,0)
        names(betaPar) <- c("beta_0", "beta_1", "beta_2","beta_3")
        EstResults <- list(Par=betaPar, Res=resid(beta))
        return(EstResults)
      }

    Tau1Values <- c( Tau1, mean(c(Tau1[1],Tau1[2])) )
    Tau2Values <- c( Tau2, mean(c(Tau2[1],Tau2[2])) )
    if(is.data.frame(rate)) rate <- data.matrix(rate)
    if(is.vector(rate)) rate <- matrix(rate, 1, length(maturity))
    FinalResults <- matrix(0, nrow(rate), 6)
    FinalResultsTau2 <- matrix(0, length(Tau1Values), 7)   
    colnames( FinalResults ) <- c("beta_0","beta_1","beta_2","beta_3","tau1","tau2" )
    j <- 1
    while(j <= nrow(rate) )
      {
        InterResultsTau1 <- matrix(0,length(Tau1Values), 7)
        InterResultsTau2 <- matrix(0,length(Tau2Values), 7)
        # colnames( InterResults ) <- c("beta0","beta1","beta2","beta_3","Tau1","Tau2","SSR")
        for( i in 1:length(Tau1Values))
          {
            Tau1Temp <- optimize(beta2Spot,interval=c(0.001,max(Tau1)),maturity=Tau1Values[i],maximum=TRUE)$maximum
            for( a in 1:length(Tau2Values))
              {
                Tau2Temp <- optimize(beta3Spot,interval=c(0.001,max(Tau2)),maturity=Tau2Values[a],maximum=TRUE)$maximum 
                InterEstimation <- beta.estimator(rate[j,], maturity, Tau1Temp, Tau2Temp)
                BetaCoef <- InterEstimation$Par
                SSR <- sum(InterEstimation$Res^2)
                InterResultsTau2[a,] <- c(BetaCoef, Tau1Temp, Tau2Temp, SSR)
              }
            BestRowTau2 <- which.min(InterResultsTau2[,7])
            FinalResultsTau2[i,] <- InterResultsTau2[BestRowTau2,]
          }
        BestRow <- which.min(FinalResultsTau2[,7])
        FinalResults[j,] <- FinalResultsTau2[BestRow,1:6]
        j <- j+1
      }
    FinalResults <- as.data.frame(FinalResults)
    return( FinalResults )
  }

