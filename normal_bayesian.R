#known variance
npost_params <- function(x, sig02, m0, s02){
  n <- length(x)
  eff <- sig02/s02
  m <- (n/(n+eff))*mean(x) + (eff/(n+eff))*m0
  v <- sig02/(n+eff)
  out <- c(eff, m, v)
  names(out) <- c('eff', 'mean', 'var')
  return(out)
}

y <- c(94.6, 95.4, 96.2, 94.9, 95.9)
x <- npost_params(y, 0.25, 100, 0.25)
q <- qnorm(0.975, round(x[2],2), sqrt(round(x[3],3)))

#unknown variance
#B
#prior parameters
a <- 3
b <- 200
m = 500
w = 0.1
n = 27
y_bar <- 609.7
s2 <- 401.8

#posterior parameters
a <- a + n/2
b <- b + ((n-1)/2)*s2 + (w*n/(2*(w+n)))*(y_bar-m)^2
m <- (n*y_bar+w*m)/(w+n)

z <- rgamma(1000, a, b)
sig2B <- 1/z
muB <- rnorm(1000, m, sqrt(sig2B/(w+n)))

rm(list = c('a','b','m','n','y_bar','s2','z'))
#A
#prior parameters
a <- 3
b <- 200
m = 500
w = 0.1
n = 30
y_bar <- 622.8
s2 <- 403.1

#posterior parameters
a <- a + n/2
b <- b + ((n-1)/2)*s2 + (w*n/(2*(w+n)))*(y_bar-m)^2
m <- (n*y_bar+w*m)/(w+n)

z <- rgamma(1000, a, b)
sig2A <- 1/z
muA <- rnorm(1000, m, sqrt(sig2A/(w+n)))

mean(muA > muB)
