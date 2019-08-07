lnlorenz <- function(x, mu, sd){
  et <- function(t) {exp(-(t^2))}
  erf <- function(z) {(2/pi)*integrate(et,0,z)$value}
  inverf <- function(z) {
    iter0 <- 0
    iter <- iter0
    N <- 10000
    tol <- 1E-15
    i <- 1
    k <- numeric(N)
    k[1] <- 1
    s <- numeric(N)
    while (i<=N) {
      tmp <- numeric(N)
      for(m in 1:i){
        tmp[m] <- (k[m]*k[(i)-(m)+1])/(m*(2*(m-1)+1))
      }
      k[i+1] <- sum(tmp)
      iter <- iter + (k[i+1]/(2*(i-1)+1))*((sqrt(pi)*z)/2)^(2*(i-1)+1)
      s[i] <- iter
      i <- i + 1
      if (abs(iter-iter0) < tol) {break}
      iter0 <- iter
    }
    s[i-1]
  }
  lnorminv <- function(x, mu, sd) {exp(mu+sd*(-sqrt(2))*inverf(2*x))}
}

lnlorenz <- function(x,mu=0,sd=1){pnorm(qnorm(x,mu,sd)-sd,mu,sd)}
