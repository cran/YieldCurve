\name{YieldCurve-package}
\alias{YieldCurve-package}
\alias{YieldCurve}
\docType{package}
\title{
Modelling and estimation of the yield curve
}
\description{
Modelling the yield curve with some parametric models. 
The models implemented are: Nelson-Siegel, Diebold-Li and Svensson. 
The package also includes the data of the term structure of interest rate of Federal Reserve Bank
and European Central Bank.
}
\details{
\tabular{ll}{
Package: \tab YieldCurve\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2009-11-03\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
DieboldLi
}
\author{
Sergio Salvino Guirreri

Maintainer: Sergio Salvino Guirreri <sergioguirreri@gmail.com> <guirreri@dssm.unipa.it>

}
\references{
Diebold, F.X. and Li, C. (2006), Forecasting the Term Structure of Government Bond Yields, \emph{Journal of Econometrics}, \bold{130}, 337-364. 

Diebold, F.X., Ji, L. and Li, C. (2006), A Three-Factor Yield Curve Model: Non-Affine Structure, Systematic Risk Sources, and Generalized Duration, in L.R. Klein (ed.), \emph{Long-Run Growth and Short-Run Stabilization: Essays in Memory of Albert Ando}. Cheltenham, U.K.: Edward Elgar, 240-274.

Nelson, C.R., and A.F. Siegel (1987), Parsimonious Modeling of Yield Curve, \emph{The Journal of Business}, \bold{60}, 473-489.

Svensson, L.E. (1994), Estimating and Interpreting Forward Interest Rates: Sweden 1992-1994, \emph{IMF Working Paper}, \bold{WP/94/114}.

}

\keyword{models}
\keyword{htest}


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
}
