# Function for data generation of real values matrix (X) as well as measurement error matrix (UC)
data_gen <- function(obs, mu, va, sk, ku)
  {
  if (!requireNamespace("PearsonDS", quietly = TRUE)) {
    stop(
      "Package \"PearsonDS\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("gsl", quietly = TRUE)) {
    stop(
      "Package \"gsl\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
X1 <- numeric()                      
for (i in 1 : length (mu)){
  x <- PearsonDS::rpearson (obs, moments= c(mu[i], va[i], sk[i], ku[i])) # samples can be drawn from any parent population (normal as well non-normal)
  X1 <- cbind(X1,x)
}

return (X1)
}


