\name{FedYieldCurve}
\alias{FedYieldCurve}
\docType{data}
\title{Federal Reserve interest rates }
\description{
3 months, 6 months, 1 year, 5 year, 7 year and 10 year monthly interest rate of the Federal Reserve from Janury 1982 to June 2009. 
}
\usage{data(FedYieldCurve)}
\format{
  An object with class attributes \code{ts}, \code{mts}.
}

\source{
\acronym{FED}: \url{http://www.federalreserve.gov/}. \cr
Data set is available for download in Excel format at \url{http://www.guirreri.host22.com/}.
}

\examples{
data(FedYieldCurve)
tau <- c(3, 6, 12, 60, 84, 120 )
plot(tau, FedYieldCurve[1,], type="o")
}
\keyword{datasets}
