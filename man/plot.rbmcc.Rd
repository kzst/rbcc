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
plot.rbmcc(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class 'rbqcc'.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Kosztyán, Z. T., and Katona, A. I. (2016). Risk-based multivariate control chart. Expert Systems with Applications, 62, 250-262.
}
\author{
Aamir Saghir, Attila I. Katona, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{data_gen}},
\code{\link{rbmcc}},
\code{\link{rbmcc_opt}}
\code{\link{summary.rbmcc}}
}
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

UC <- data_gen(obs,mu_UC, va_UC, sk_UC, ku_UC)     # example for generation of measurement error matrix

# with default vector of decision costs
C <- c(1,1,1,1)                # vector of decision costs
H <- rbmcc(X, UC, C)           # for subgroups of size 1
plot.rbmcc(H)                  # plot RBMCC
}
\keyword{multivariate}
\concept{control chart}
