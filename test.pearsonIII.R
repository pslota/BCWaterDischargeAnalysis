# http://stackoverflow.com/questions/15003362/problems-fitting-log-pearson-iii-in-r
# PIII fit to the logs of the discharges

StMary <- c(565,294,303,569,232,405,228,232,394,238,524,368,464,411,368,487,394,
            337,385,351,518,365,515,280,289,255,334,456,479,334,394,348,428,337,
            311,453,328,564,527,510,371,824,292,345,442,360,371,544,552,651,190,
            202,405,583,725,232,974,456,289,348,564,479,303,603,514,377,318,342,
            593,378,255,292)
StMary <- StMary / 100

LStMary <- -log(StMary)

#LStMary <- ttemp

plotdata <- data.frame(x=LStMary)
ggplot(data=plotdata, aes(x=x))+
   geom_histogram()

m <- mean(LStMary)
v <- var(LStMary)
s <- sd(LStMary)
g <- e1071::skewness(LStMary, type=1)

c(m,v,s,g)
# Correct the sample skew for bias using the recommendation of 
# Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and Log Pearson Type 3 distributions revisited." 
# Water Resources Reseach 13(2): 427-443, as used by Kite

n <- length(StMary)
g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)

# We will use method of moment estimates as starting values for the MLE search

my.shape <- (2/g)^2
my.scale <- sqrt(v)/sqrt(my.shape)*sign(g)
my.scale <- sqrt(v)/sqrt(my.shape)
my.location <- m-my.scale*my.shape

my.param <- list(shape=my.shape, scale=my.scale, location=my.location)
my.param

dPIII<-function(x, shape, location, scale, log=FALSE) PearsonDS::dpearsonIII(x, shape, location, scale, log=log)
pPIII<-function(q, shape, location, scale) PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
qPIII<-function(p, shape, location, scale) PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)


library(fitdistrplus);
library(PearsonDS)
test <-fitdist(LStMary, distr="PIII", method="mle", start=my.param)
test
plot(test)
min(LStMary)
quantile(test, prob=c(.01, .99))
quantile(test, prob=c(.0000001))

dPIII(LStMary, shape=my.shape, scale=my.scale, location=my.location, log=TRUE)

LStMary
my.param <- list(shape=my.shape, scale=my.scale, thres=min(LStMary)-1)

test <-fitdist(LStMary, distr="gamma3", method="mle", start=my.param, control=c(maxit=1000))


