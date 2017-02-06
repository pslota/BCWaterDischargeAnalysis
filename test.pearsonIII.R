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




############
# Try the method of moments as outlined in Bulletin 17b to see if I can reproduce
# the vfa reports from HEC


library(PearsonDS); library(e1071)


cow.csv.1day <- textConnection(
"Dummy,dd,mm,Year,Min,Rank,YearRank,MinRanked,PlotPos,Dummy2
,03,Oct,1965,3.540,1,1997,7.450,97.96,
,03,Aug,1966,5.800,2,1975,6.430,95.92,
,31,Aug,1967,4.110,3,1999,6.010,93.88,
,06,Aug,1968,5.100,4,1971,6.000,91.84,
,16,Aug,1969,4.980,5,1972,5.830,89.80,
,22,Jul,1970,4.250,6,1966,5.800,87.76,
,11,Aug,1971,6.000,7,1983,5.710,85.71,
,30,Oct,1972,5.830,8,2011,5.680,83.67,
,12,Oct,1973,3.110,9,2007,5.640,81.63,
,04,Nov,1974,3.280,10,2009,5.480,79.59,
,25,Jul,1975,6.430,11,1982,5.320,77.55,
,10,Aug,1976,4.620,12,2008,5.300,75.51,
,15,Aug,1977,4.160,13,2000,5.230,73.47,
,31,Jul,1978,4.640,14,1968,5.100,71.43,
,06,Jul,1979,3.840,15,2002,4.980,69.39,
,31,Aug,1980,4.420,16,1969,4.980,67.35,
,09,Sep,1981,4.660,17,2005,4.960,65.31,
,01,Aug,1982,5.320,18,1993,4.890,63.27,
,13,Aug,1983,5.710,19,1990,4.890,61.22,
,21,Jul,1984,4.120,20,1994,4.800,59.18,
,29,Aug,1985,3.360,21,1995,4.670,57.14,
,24,Oct,1986,3.450,22,1981,4.660,55.10,
,09,Aug,1987,3.600,23,1978,4.640,53.06,
,31,Aug,1988,4.140,24,1996,4.630,51.02,
,07,Oct,1989,4.080,25,1976,4.620,48.98,
,15,Aug,1990,4.890,26,1991,4.610,46.94,
,21,Aug,1991,4.610,27,1980,4.420,44.90,
,08,Aug,1992,3.740,28,2010,4.390,42.86,
,03,Oct,1993,4.890,29,1970,4.250,40.82,
,21,Aug,1994,4.800,30,2001,4.210,38.78,
,24,Jun,1995,4.670,31,1977,4.160,36.73,
,26,Aug,1996,4.630,32,1988,4.140,34.69,
,19,Aug,1997,7.450,33,1984,4.120,32.65,
,21,Sep,1998,3.960,34,1967,4.110,30.61,
,17,Sep,1999,6.010,35,1989,4.080,28.57,
,02,Sep,2000,5.230,36,1998,3.960,26.53,
,18,Aug,2001,4.210,37,1979,3.840,24.49,
,05,Nov,2002,4.980,38,1992,3.740,22.45,
,28,Sep,2003,2.680,39,1987,3.600,20.41,
,15,Aug,2004,2.770,40,2012,3.550,18.37,
,09,Aug,2005,4.960,41,1965,3.540,16.33,
,13,Sep,2006,2.580,42,1986,3.450,14.29,
,16,Jul,2007,5.640,43,1985,3.360,12.24,
,05,Aug,2008,5.300,44,1974,3.280,10.20,
,30,Aug,2009,5.480,45,1973,3.110,8.16,
,30,Aug,2010,4.390,46,2004,2.770,6.12,
,13,Sep,2011,5.680,47,2003,2.680,4.08,
,07,Oct,2012,3.550,48,2006,2.580,2.04,")

cow.csv <- textConnection(  # 3 day rolling average from HEC
"Dummy,dd,mm,Year,Min,Rank,YearRank,MinRanked,PlotPos,Dummy2
,03,Oct,1965,3.747,1,1997,7.520,97.96,
,12,Aug,1966,5.937,2,1975,6.713,95.92,
,13,Sep,1967,4.110,3,1971,6.293,93.88,
,07,Aug,1968,5.223,4,1999,6.120,91.84,
,26,Aug,1969,4.980,5,1972,6.000,89.80,
,23,Jul,1970,4.343,6,1966,5.937,87.76,
,12,Aug,1971,6.293,7,2011,5.840,85.71,
,30,Oct,1972,6.000,8,1983,5.747,83.67,
,10,Sep,1973,4.470,9,2007,5.680,81.63,
,05,Nov,1974,3.443,10,2009,5.533,79.59,
,26,Jul,1975,6.713,11,1982,5.360,77.55,
,10,Aug,1976,4.977,12,2008,5.330,75.51,
,15,Aug,1977,4.643,13,2000,5.300,73.47,
,01,Aug,1978,4.700,14,1968,5.223,71.43,
,07,Jul,1979,3.920,15,2002,5.107,69.39,
,31,Aug,1980,4.430,16,1993,5.060,67.35,
,06,Sep,1981,4.833,17,2005,5.020,65.31,
,22,Aug,1982,5.360,18,1969,4.980,63.27,
,21,Aug,1983,5.747,19,1976,4.977,61.22,
,21,Jul,1984,4.420,20,1990,4.967,59.18,
,22,Aug,1985,3.617,21,1994,4.880,57.14,
,24,Oct,1986,3.490,22,1981,4.833,55.10,
,10,Aug,1987,3.733,23,1995,4.800,53.06,
,01,Sep,1988,4.417,24,1991,4.707,51.02,
,08,Oct,1989,4.120,25,1978,4.700,48.98,
,15,Aug,1990,4.967,26,1996,4.650,46.94,
,29,Jul,1991,4.707,27,1977,4.643,44.90,
,08,Aug,1992,3.790,28,1973,4.470,42.86,
,04,Sep,1993,5.060,29,2010,4.460,40.82,
,22,Aug,1994,4.880,30,1980,4.430,38.78,
,25,Jun,1995,4.800,31,1984,4.420,36.73,
,26,Aug,1996,4.650,32,1988,4.417,34.69,
,24,Aug,1997,7.520,33,1970,4.343,32.65,
,06,Sep,1998,3.997,34,2001,4.227,30.61,
,18,Sep,1999,6.120,35,1989,4.120,28.57,
,02,Sep,2000,5.300,36,1967,4.110,26.53,
,20,Aug,2001,4.227,37,1998,3.997,24.49,
,05,Nov,2002,5.107,38,1979,3.920,22.45,
,04,Oct,2003,2.750,39,1992,3.790,20.41,
,16,Aug,2004,2.823,40,1965,3.747,18.37,
,09,Aug,2005,5.020,41,1987,3.733,16.33,
,14,Sep,2006,2.617,42,1985,3.617,14.29,
,17,Jul,2007,5.680,43,2012,3.597,12.24,
,07,Aug,2008,5.330,44,1986,3.490,10.20,
,21,Sep,2009,5.533,45,1974,3.443,8.16,
,17,Sep,2010,4.460,46,2004,2.823,6.12,
,15,Sep,2011,5.840,47,2003,2.750,4.08,
,08,Oct,2012,3.597,48,2006,2.617,2.04,")


cow <- read.csv(cow.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)


cow.quant.csv.1dat <- textConnection(
"Dummy,HEC.quant,ignore,HEC.prob,x1,x2,x3
,7.140,7.263,99.0,7.995,6.556,
,6.348,6.406,95.0,6.971,5.904,
,5.932,5.967,90.0,6.447,5.552,
,5.437,5.455,80.0,5.840,5.122,
,4.530,4.530,50.0,4.792,4.286,
,3.693,3.677,20.0,3.918,3.442,
,3.290,3.261,10.0,3.521,3.018,
,2.976,2.934,5.0,3.218,2.688,
,2.645,2.582,2.0,2.898,2.340,
,2.438,2.358,1.0,2.698,2.125,
,2.257,2.161,0.5,2.523,1.939,
,2.050,1.933,0.2,2.320,1.730,")

cow.quant.csv <- textConnection(
"Dummy,HEC.quant,ignore,HEC.prob,x1,x2,x3
,7.154,7.263,99.0,7.960,6.599,
,6.435,6.489,95.0,7.036,6.004,
,6.047,6.080,90.0,6.548,5.674,
,5.576,5.593,80.0,5.973,5.264,
,4.689,4.689,50.0,4.951,4.446,
,3.847,3.831,20.0,4.072,3.596,
,3.433,3.404,10.0,3.666,3.160,
,3.108,3.064,5.0,3.352,2.816,
,2.763,2.696,2.0,3.020,2.452,
,2.544,2.461,1.0,2.810,2.224,
,2.354,2.252,0.5,2.625,2.028,
,2.135,2.011,0.2,2.412,1.806,")



cow.quant <- read.csv(cow.quant.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)
head(cow.quant)

# compute the first few moments
library(e1071)
mean <- mean(log10(cow$Min))
mean
sd   <- sd  (log10(cow$Min))
sd
skew<- e1071::skewness(log10(cow$Min), type=2)
skew


# estimate parameters of log III Pearson curve
alpha <- 4/skew^2        # shape
beta <- sign(skew)*sqrt( sd^2/alpha)   # scale
tau  <- mean - alpha*beta  # location
c(shape=alpha, location=tau, scale=beta)

prob <- cow.quant$HEC.prob/100
prob

log.quant <- qPIII(prob, alpha, tau, beta)
quant <- 10^log.quant
quant

quant - cow.quant$HEC.quant


# try and fit using method of moments with fitdistr

x<- cow
x$value <- log10(x$Min)
         m <- mean(x$value)
         v <- var (x$value)
         s <- sd  (x$value)
         g <- e1071::skewness(x$value, type=2)

         # This can be corrected, but HEC Bulletin 17b does not do these corrections
         # Correct the sample skew for bias using the recommendation of 
         # Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and Log Pearson Type 3 distributions revisited." 
         # Water Resources Reseach 13(2): 427-443, as used by Kite
         #n <- length(x$value)
         #g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)
         # We will use method of moment estimates as starting values for the MLE search

         my.shape <- (2/g)^2
         my.scale <- sqrt(v)/sqrt(my.shape)*sign(g)
         my.location <- m-my.scale*my.shape

         start=list(shape=my.shape, location=my.location, scale=my.scale)

alpha <- 4/myskew^2        # shape
beta <- sign(myskew)*sqrt( sd^2/alpha)   # scale
tau  <- mean - alpha*beta  # location

my.shape
my.scale
my.location
start
start




mPIII <- function(order, shape, location, scale){
   # compute the empirical first 3 moments of the PIII distribution
   if(order==1) return( location + shape*scale)
   if(order==2) return(scale*scale*shape)
   if(order==3) return(2/sqrt(shape)*sign(beta))
}
mPIII(1, my.shape, my.location, my.scale)
mean
mPIII(2, my.shape, my.location, my.scale)
sd^2
mPIII(3, my.shape, my.location, my.scale)
g


ePIII <- function(x,order){
   # compute (centered) empirical centered moments of the data
   if(order==1) return(mean(x))
   if(order==2) return(var(x))
   if(order==3) return(e1071::skewness(log10(cow$Min), type=2))
}
ePIII(x$value, 1)
ePIII(x$value, 2)
ePIII(x$value, 3)

fit <- fitdistrplus::fitdist(x$value, distr="PIII", start=start,
                         method="mme",  order=1:3, memp=ePIII, control=list(maxit=1000))
names(fit)  
fit$distname

  mmedist(x$value, distr="PIII", start=start,
                          order=1:3, memp=ePIII,  control=list(maxit=1000))

  start
