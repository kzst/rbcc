\name{plot.rbmcc}
\alias{plot.rbmcc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot function for Risk-based Multivariate Control Chart
}
\description{
Plot function for Risk-based Multivariate Control Chart
}
\usage{
\method{plot}{rbmcc}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class 'rbmcc'.}
  \item{...}{other graphical parameters.}
}

\references{
Kosztyán, Z. T., and Katona, A. I. (2016). Risk-based multivariate control chart. Expert Systems with Applications, 62, 250-262.
}
\author{
Aamir Saghir, Attila I. Katona, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
}


\seealso{\code{\link{rbmcc}}, \code{\link{rbmcc_opt}}, \code{\link{data_gen}}, \code{\link{summary.rbmcc}}.}

\examples{
# Data generation for matrix X
mu_X <- c(0,1,2)               # vector of means.
va_X  <- c(1,2, 0.5)           # vector of standard deviation.
sk_X <- c(0,0.5, 0.8)          # vector of skewness.
ku_X <- c(3,3.5, 4)            # vector of kurtosis.
obs <- 200                     # Total number of observations of a process.

X <- data_gen (obs, mu_X, va_X, sk_X, ku_X) # generate data pints

# Data generation for measurement error matrix UC

mu_UC <- c(0,0,0)              # vector of means of measurement errors.
va_UC <- c(1,2, 0.5)           # vector of standard deviation of measurement errors.
sk_UC <- c(0,0,0)              # Vector of skewness of measurement errors.
ku_UC <- c(3,3,3)              # Vector of kurtosis of measurement errors.

# example for generation of measurement error matrix
UC <- data_gen(obs,mu_UC, va_UC, sk_UC, ku_UC)

# with default vector of decision costs
C <- c(1,1,1,1)                # vector of decision costs
H <- rbmcc(X, UC, C)           # for subgroups of size 1
plot(H)                        # plot RBMCC

H_opt <- rbmcc_opt(X, UC, C)   # optimal risk-based multivariate control chart

# with vector of proportional decision costs
C <- c(1, 5, 60, 5)        # vector of decision costs
H <- rbmcc(X, UC, C)           # for subgroups of size 1
H_opt <- rbmcc_opt(X, UC, C)   # optimal risk-based multivariate control chart

# with vector of proportional decision costs and sugbroup size 3
C <- c(1, 5, 60, 5)           # vector of decision costs
H <- rbmcc(X, UC, C, 3)           # for subgroups of size 3
H_opt <- rbmcc_opt(X, UC, C, 3)   # optimal risk-based multivariate control chart

# Plot of Hotelling's T2 and optimal risk based multivariate control charts

plot(H_opt)
}
\keyword{multivariate}
\concept{control chart}
