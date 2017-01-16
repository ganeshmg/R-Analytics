#Sample matrix creation for inverse testing
library(MASS)

k   <- 2000
rho <- .3

S       <- matrix(rep(rho, k*k), nrow=k)
diag(S) <- 1

dat <- mvrnorm(10000, mu=rep(0,k), Sigma=S) ### be patient!

R <- cor(dat)
