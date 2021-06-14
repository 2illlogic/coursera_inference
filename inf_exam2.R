rm(list = ls())

#problem 1
x <- 64
n <- 100
theta0 = 0.5
theta1 = x/n

l0 <- dbinom(x,n,theta0)
l1 <- dbinom(x,n,theta1)

lr10 <- l1/l0

#problem 2
x <- 143
n <- 280
theta0 = 0.5
theta1 = 0.52

l0 <- dbinom(x,n,theta0)
l1 <- dbinom(x,n,theta1)

lr10 <- l1/l0
lr01 <- l0/l1

#problem 4
rm(list = ls())
x <- 20
n <- 50
a_pr <- 2
b_pr <- 2

##hypothesis
theta0 <- 0.50

a_post <- a_pr + x
b_post <- b_pr + n - x

l_pr <- dbeta(theta0,a_pr,b_pr)
l_post <- dbeta(theta0,a_post,b_post)

X <- seq(0,1,0.01)
plot(X,dbeta(X,a_post,b_post),type = 'l')
lines(X,dbeta(X,a_pr,b_pr),col='blue')
abline(v = 0.5, type = 'd')

#problem 5
rm(list = ls())
x <- 0
n <- 10
a_pr <- 10
b_pr <- 10

fr <- x/n

a_post <- a_pr + x
b_post <- b_pr + n - x
bayes <- a_post/(a_post + b_post)
