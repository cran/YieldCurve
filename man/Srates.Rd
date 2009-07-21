\name{Srates}
\alias{Srates}
\title{Interest rates of the Svensson's model.}
\description{
Returns the interest rates by Svensson's model.
}
\usage{
Srates(betaCoeff, lambdaValues, maturity, whichRate = "Forward")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{betaCoeff}{ vector or matrix of the beta's coefficients.}
  \item{lambdaValues}{ vector or matrix of values of \eqn{\lambda_1} and \eqn{\lambda_2}.}
  \item{maturity}{maturity of the yield curve of which want to return the interest rates.}
  \item{whichRate}{which rate want to return: "Spot" or "Forward" rates. }
}
\details{
 \code{betaCoeff} is a vector or matrix of the four coefficients of the Svensson's model, while \code{lambdaValues} is a vector or matrix of two lambda values of Svensson's model.
}
\value{
Return interest rates in matrix object with number of rows equal to \code{nrow(betaCoeff)} and number of columns equal to \code{length(maturity)}.
}
\references{ 
Svensson, L.E. (1994), Estimating and Interpreting Forward Interest Rates: Sweden 1992-1994, \emph{IMF Working Paper}, \bold{WP/94/114}.

Nelson, C.R., and A.F. Siegel (1987), Parsimonious Modeling of Yield Curve, \emph{The Journal of Business}, \bold{60}, 473-489.
}
\author{Sergio Salvino Guirreri}

\examples{
data(FedYieldCurve)
tau <- c(3,6,12,60,84,120)
A <- Svensson(FedYieldCurve[1:10,], tau, c(3,12), c(60,120) )
B <- Srates(A[,1:4], A[,5:6], tau, whichRate="Spot")
plot(tau,FedYieldCurve[10,],main="Fitting Svensson's yield curve", type="o")
lines(tau,B[10,], col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),col=c(1,2),lty=1)
grid()
}

\keyword{ htest }
