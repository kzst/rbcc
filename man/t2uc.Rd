\name{t2uc}
\alias{t2uc}
\docType{data}
\title{
Sample data for Risk-based Multivariate Control Chart
}

\description{
The optimization is conducted assuming all data points are measured in the laboratory. This study verifies the applicability of risk-based approaches to real industrial problems and compares the results obtained by simulations and experiments, providing information about the achievable cost reduction opportunities granted by simulations.
}

\usage{data("t2uc")}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
    \item{\code{length_optical}}{a numeric vector}
    \item{\code{diameter_optical}}{a numeric vector}
    \item{\code{length_manual}}{a numeric vector}
    \item{\code{diameter_manual}}{a numeric vector}
    \item{\code{length_error}}{a numeric vector}
    \item{\code{diameter_error}}{a numeric vector}
  }
}

\references{
Katona, A. I. (2021). Validation of risk-based quality control techniques: a case study from the automotive industry. Journal of Applied Statistics, 1-20.
}
\examples{
data(t2uc)

}
\keyword{datasets}
