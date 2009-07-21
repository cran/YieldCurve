\name{ECBYieldCurve}
\alias{ECBYieldCurve}
\docType{data}
\title{ Yield curve data spot rate, AAA-rated bonds, maturities from 3 months to 30 years}
\description{
Government bond, nominal, all triple A issuer companies. The maturity are 3 and 6 months and from 1 year to 30 years with frequency business day, provided by European Central Bank.
}
\usage{data(ECBYieldCurve)}
\format{
  A data frame with 655 observations on the following 32 variables.
  \describe{
    \item{\code{X3M}}{interest rate with maturity 3 months}
    \item{\code{X6M}}{interest rate with maturity 6 months}
    \item{\code{X1Y}}{interest rate with maturity 1 year}
    \item{\code{X2Y}}{interest rate with maturity 2 years}
    \item{\code{X3Y}}{interest rate with maturity 3 years}
    \item{\code{X4Y}}{interest rate with maturity 4 years}
    \item{\code{X5Y}}{interest rate with maturity 5 years}
    \item{\code{X6Y}}{interest rate with maturity 6 years}
    \item{\code{X7Y}}{interest rate with maturity 7 years}
    \item{\code{X8Y}}{interest rate with maturity 8 years}
    \item{\code{X9Y}}{interest rate with maturity 9 years}
    \item{\code{X10Y}}{interest rate with maturity 10 years}
    \item{\code{X11Y}}{interest rate with maturity 11 years}
    \item{\code{X12Y}}{interest rate with maturity 12 years}
    \item{\code{X13Y}}{interest rate with maturity 13 years}
    \item{\code{X14Y}}{interest rate with maturity 14 years}
    \item{\code{X15Y}}{interest rate with maturity 15 years}
    \item{\code{X16Y}}{interest rate with maturity 16 years}
    \item{\code{X17Y}}{interest rate with maturity 17 years}
    \item{\code{X18Y}}{interest rate with maturity 18 years}
    \item{\code{X19Y}}{interest rate with maturity 19 years}
    \item{\code{X20Y}}{interest rate with maturity 20 years}
    \item{\code{X21Y}}{interest rate with maturity 21 years}
    \item{\code{X22Y}}{interest rate with maturity 22 years}
    \item{\code{X23Y}}{interest rate with maturity 23 years}
    \item{\code{X24Y}}{interest rate with maturity 24 years}
    \item{\code{X25Y}}{interest rate with maturity 25 years}
    \item{\code{X26Y}}{interest rate with maturity 26 years}
    \item{\code{X27Y}}{interest rate with maturity 27 years}
    \item{\code{X28Y}}{interest rate with maturity 28 years}
    \item{\code{X29Y}}{interest rate with maturity 29 years}
    \item{\code{X30Y}}{interest rate with maturity 30 years}
  }
}

\source{
\acronym{ECB}: \url{http://www.ecb.europa.eu/stats/money/yc/html/index.en.html}.\cr
Data set is also available for download in Excel format at \url{http://www.guirreri.host22.com/index.php?p=1_2_Documents}. 
}

\examples{
data(ECBYieldCurve)
## maybe str(ECBYieldCurve) ; plot(ECBYieldCurve) 
tau <- c(3/12,6/12,1:30)
par(mfrow=c(1,2))
plot(tau,ECBYieldCurve[1,],type="o", ylim=c(0,5), 
	main="European Union's yield curve",sub="29/12/2006")
grid()
plot(tau,ECBYieldCurve[655,],type="o", ylim=c(0,5), 
	main="European Union's yield curve",sub="24/07/2009")
grid()
}
\keyword{datasets}
