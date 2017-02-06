
# Load the packages needed for these functions are
#
library(car         ) # recode function
library(e1071       ) # adjustment for fitting skewness of Pearson III distribution
library(fitdistrplus) # fit the weibull and pearson III distributions for volume freq anal
library(ggplot2     ) # created plots
library(openxlsx    ) # open the Excel spreadsheet to compare my results to old methods
library(PearsonDS   ) # Pearson III distribution functions
library(plyr        ) # split-apply-combine paradigm
library(reshape2    ) # melt and cast data frames
library(scales      ) # modify scales in ggplot
library(zoo         ) # compute rolling averages


# Define the density, distribution, quantile and moment functions for Pearson III distribution.

dPIII <<-function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)
pPIII <<-function(q, shape, location, scale) PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
qPIII <<-function(p, shape, location, scale) PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)

mPIII <<-function(order, shape, location, scale){
   # compute the empirical first 3 moments of the PIII distribution
   if(order==1) return( location + shape*scale)
   if(order==2) return(scale*scale*shape)
   if(order==3) return(2/sqrt(shape)*sign(scale))
}

