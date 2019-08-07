required.packages <- c("reshape2","ggplot2","data.table","WDI","XML","readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/poverty_predictions")

#Asymmetry coefficient
LAC <- function(a,b,c,type=list("GQ","Beta")){
  if(type=="GQ"){
    e <- -(a+b+c+1)
    m <- b^2-4*a
    n <- 2*b*e-4*c
    z <- 1+b/2
    y <- 16*z^2*m-4*m^2
    x <- 16*z^2*n-4*n*m
    w <- 16*z^2*e^2-n^2
    p1 <- (-x-(sqrt(x^2-4*y*w)))/(2*y)
    p2 <- (-x+(sqrt(x^2-4*y*w)))/(2*y)
    if(p1>1 | p1<0){
      P.mu <- p2
    }
    if(p2>1 | p2<0){
      P.mu <- p1
    }
    L.mu <- -0.5*(b*P.mu+e+(m*P.mu^2+n*P.mu+e^2)^(0.5))
  }else{
    p0 <- 0.6
    N <- 10000
    tol <- 1E-15
    i <- 1
    p1 <- p0
    s <- numeric(N)
    while (i<=N) {
      f <- a*(p0^b)*((1-p0)^c)*((b/p0)-c/(1-p0))
      df.dx <- a*(p0^(b-2))*((1-p0)^(c-2))*((b^2)*((p0-1)^2)+b*(p0-1)*((2*c-1)*p0+1)+(c-1)*c*(p0^2))
      p1 <- (p0 - 0.01*(f/df.dx))
      s[i] <- p1
      i <- i + 1
      if (abs(p1-p0) < tol) {break}
      p0 <- p1
    }
    P.mu <- (s[i-1])
    L.mu <- P.mu-a*P.mu^b*(1-P.mu)^c
  }
  AC <- P.mu + L.mu
  return(AC)
}

#Gini coefficient
Gini <- function(a,b,c,type=list("GQ","Beta")){
  if(type=="GQ"){
    e <- -(a+b+c+1)
    m <- b^2-4*a
    n <- 2*b*e-4*c
    r <- (n^2-4*m*e^2)^(0.5)
    if(m>0){
      G <- (e/2)-((n*(b+2))/(4*m))-((r^2)/(8*m*sqrt(m)))*log(abs((2*m+n+2*sqrt(m)*(a+c-1))/(n-2*e*sqrt(m))))
    } else {
      G <- (e/2)-((n*(b+2))/(4*m))+((r^2)/(8*m*sqrt(-m)))*(asin((2*m+n)/r)-asin(n/r))
    }
  }else{
    G <- 2*a*((gamma(b+1)*gamma(c+1))/gamma(b+c+2)) #2*a*beta(b+1,c+1)
  }
  return(G)
}


all_parameters <- fread("project_data/all_parameters.csv")

asymmetries <- cbind(all_parameters, all_parameters[, .(LAC=tryCatch({LAC(A,B,C,type)},error=function(e){return(as.numeric(NA))})
                                                        , Gini=tryCatch({Gini(A,B,C,type)},error=function(e){return(as.numeric(NA))})), by=.(svy,year,type)][,c(4,5)])
