oring <- read.table('http://www.randomservices.org/random/data/Challenger2.txt'
                   ,header = 1)

attach(oring)

plot(T,I)

#store regression
oring.lm <- lm(I ~ T) 

summary(oring.lm)

#plot fitted regression line
lines(T, fitted(oring.lm))

#predict o-ring damage at 31 degress Fahrenheit
coef(oring.lm)
coef(oring.lm)[1] + coef(oring.lm)[2]*31

#posterior 95% prediction interval
predict(oring.lm, data.frame(T=31), interval = 'predict')

#-----------------------------------------------------------------
heights <- read.table('http://www.randomservices.org/random/data/Galton.txt'
                      ,header = 1)
attach(heights)

pairs(heights)

heights.lm <- lm(Height ~ Father + Mother + Gender + Kids)

summary(heights.lm)

heights.lm <- lm(Height ~ Father + Mother + Gender)

summary(heights.lm)

#Quiz----------------------------------------------------------------------
golf <- read.table('http://users.stat.ufl.edu/~winner/data/pgalpga2008.dat'
                   ,header = 0)
names(golf) <- c('D','F','S')
#note: for column S Female = 1, Male = 2
golfF <- subset(golf, S == 1, select = 1:2)
golfM <- subset(golf, S == 2, select = 1:2)
plot(golfF$D,golfF$F)
plot(golfM$D,golfM$F)

attach(golfF)
golfF.lm <- lm(F ~ D)
summary(golfF.lm)

#produce a mean prediction for a new golfer who averages 260 yds driving dist.
coef(golfF.lm)[1] + coef(golfF.lm)[2] * 260

#produce the mean and 95% predictive interval
predict(golfF.lm, data.frame(D = 260), interval = 'predict')

#Honor-------------------------------------------------------------------
golf <- read.table('http://users.stat.ufl.edu/~winner/data/pgalpga2008.dat'
                   ,header = 0)
names(golf) <- c('D','F','S')
attach(golf)
golf$S <- as.integer(golf$S == 2)

attach(golf)
golf.lm <- lm(F ~ D + S)
summary(golf.lm)

#create residual plot
plot(fitted(golf.lm),residuals(golf.lm))
