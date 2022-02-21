\name{data_gen}
\alias{data_gen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Generator for Risk-based Multivariate Control Charts
}
\description{
data_gen function simulate the data set from a specified distribution used in the risk based multivariate control chart.
}
\usage{
data_gen(obs, mu, va, sk, ku)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{obs}{The total number of observations of a process. (numeric value).}
\item{mu}{The means of p characteristics. (numeric vector).}
\item{va}{The variances of p characteristics. (numeric vector).}
\item{sk}{The skewness of distribution of p characteristics. (numeric vector).}
\item{ku}{The kurtosis of distribution of p characteristics. (numeric vector).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Return the data matrix and the measurement error matrix used in the rmbcc function.
}
\references{
Kosztyán, Z. T., and Katona, A. I. (2016). Risk-based multivariate control chart. Expert Systems with Applications, 62, 250-262.
}
\author{
%%  ~~who you are~~
}
\note{
Aamir Saghir, Attila I. Katona, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rbmcc}}
\code{\link{rbmcc_opt}}
}
\examples{
library (qcc)

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
H_opt <- rbmcc_opt(X, UC, C)   # optimal risk-based multivariate control chart

# with vector of proportional decision costs
C <- c(1, 60, 5, 5)        # vector of decision costs
H <- rbmcc(X, UC, C)           # for subgroups of size 1
H_opt <- rbmcc_opt(X, UC, C)   # optimal risk-based multivariate control chart

# with vector of proportional decision costs and sugbroup size 3
C <- c(1, 60, 5, 5)           # vector of decision costs
H <- rbmcc(X, UC, C, 3)           # for subgroups of size 3
H_opt <- rbmcc_opt(X, UC, C, 3)   # optimal risk-based multivariate control chart

# Plot of Hotelling's T2 and optimal risk based multivariate control charts


library(ggplot2)
library(reshape2)

UCL=H_opt$baselimit
UCLopt=H_opt$limit
df <- data.frame(Groups = c(1:length(H_opt$real)), y1= H_opt$real, y2=H_opt$Observed, y3= UCL, y4=UCLopt)
big_data <- melt(df, id = "Groups")
ggplot(big_data, aes(x = Groups,  y = value, color = variable)) +
      geom_line()+ scale_color_manual(labels = c("real","observed","UCL", "UCLopt"), values=c("black", "green1", "blue", "red"))+
      labs (x= "Groups", y= "Group_Statistic")+ theme_bw()+ theme(legend.title = element_blank()) +
      ggtitle(" Multivariate Control chart for Hotelling's and risk-based Statistics")


}
\keyword{multivariate}
\concept{control chart}

