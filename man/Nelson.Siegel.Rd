\name{Nelson.Siegel}
\alias{Nelson.Siegel}

\title{ Estimation of the Nelson-Siegel parameters }
\description{
  Returns the estimated coefficients of the Nelson-Siegel's model.
}

\usage{
Nelson.Siegel(rate, maturity, MidTau)
}

\arguments{
  \item{rate}{vector or matrix which contains the interest rates.}
  \item{maturity}{vector wich contains the maturity ( in months) of the \code{rate}. The vector's length must be the same 
		of the number of columns of the \code{rate}.} 
  \item{MidTau}{vector which indicates medium term maturity to maximize the beta_2 factor. }
}
\details{
The Nelson-Siegel's model to describe the yield curve is:
\deqn{y_t(\tau) = \beta_{0t} + \beta_{1t} \frac{1-\exp(-\lambda \tau)}{\lambda \tau} +
\beta_{2t} \left(\frac{1-\exp(-\lambda \tau)}{\lambda \tau} - \exp(-\lambda \tau)
\right) }
}
\value{
Returns a data frame with the estimated coefficients: \eqn{\beta_{0t}}, \eqn{\beta_{1t}}, \eqn{\beta_{2t}}, and \eqn{\lambda}.  
}
\references{ 
Diebold, F.X. and Li, C. (2006), Forecasting the Term Structure of Government Bond Yields, \emph{Journal of Econometrics}, \bold{130}, 337-364. 

Diebold, F.X., Ji, L. and Li, C. (2006), A Three-Factor Yield Curve Model: Non-Affine Structure, Systematic Risk Sources, and Generalized Duration, in L.R. Klein (ed.), \emph{Long-Run Growth and Short-Run Stabilization: Essays in Memory of Albert Ando}. Cheltenham, U.K.: Edward Elgar, 240-274.

Nelson, C.R., and A.F. Siegel (1987), Parsimonious Modeling of Yield Curve, \emph{The Journal of Business}, \bold{60}, 473-489.

}
\author{Sergio Salvino Guirreri}

\seealso{ NelsonSiegel, Svensson}
\examples{
data(FedYieldCurve)
tau <- c(3, 6, 12, 60, 84, 120)
mediumTerm <- c(12,60,84)
NSParameters <- Nelson.Siegel( rate=FedYieldCurve[1:10,], 
			maturity=tau, MidTau=mediumTerm )
y <- NSrates(NSParameters[5,1:3],
	NSParameters$lambda[5],tau)
plot(tau,FedYieldCurve[5,],main="Fitting Nelson-Siegel yield curve", type="o")
lines(tau,y, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
col=c(1,2),lty=1)
grid()
}


\keyword{models}
\keyword{htest}
