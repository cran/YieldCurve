\name{NSrates}
\alias{NSrates}
\title{ Interest rates of the Nelson-Siegel's model. }
\description{
Returns the interest rates by Nelson-Siegel's model.
}
\usage{
NSrates(betaCoeff, lambdat, maturity)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{betaCoeff}{ vector or matrix of the beta's coefficients.}
  \item{lambdat}{ value of the estimated lambda}
  \item{maturity}{ maturity of the yield curve of which want to return the interest rates.}
}
\details{
\code{betaCoeff} is a vector or matrix of the three coefficients of the Nelson-Siegel's model
}
\value{
Return interest rates in matrix object with number of rows equal to \code{nrow(betaCoeff)} and number of columns equal to \code{length(maturity)}.
}
\references{ 
Diebold, F.X. and Li, C. (2006), Forecasting the Term Structure of Government Bond Yields, \emph{Journal of Econometrics}, \bold{130}, 337-364. 

Diebold, F.X., Ji, L. and Li, C. (2006), A Three-Factor Yield Curve Model: Non-Affine Structure, Systematic Risk Sources, and Generalized Duration, in L.R. Klein (ed.), \emph{Long-Run Growth and Short-Run Stabilization: Essays in Memory of Albert Ando}. Cheltenham, U.K.: Edward Elgar, 240-274.

Nelson, C.R., and A.F. Siegel (1987), Parsimonious Modeling of Yield Curve, \emph{The Journal of Business}, \bold{60}, 473-489.
}
\author{ Sergio Salvino Guirreri}

\examples{ 
data(FedYieldCurve)
b <- c(11.17514, -3.979371, 0.1302654)
lambda <- c(0.1494588)
tau <- c(3, 6, 12, 60, 84, 120 )
y <- NSrates( b, lambda, tau)
plot(tau,FedYieldCurve[10,],main="Fitting Nelson-Siegel yield curve", type="o")
lines(tau,y, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
col=c(1,2),lty=1)
grid()
}
\keyword{ htest }
