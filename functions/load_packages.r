
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
library(zoo         ) # compute rolling averages
