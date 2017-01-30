# Functions for the Analysis of Streamflow data
# Created as part of Contract GSA-IS17JHQ0177
# Author  Carl James Schwarz
#         StatMathComp Consulting by Schwarz, Inc.
#         cschwarz.statsfuca@gmail.com
# Change Log
#     2017-01-30 CJS First Edition


# Packages needed for these functions are
#
#  car       - recode function
#  e1071     - adjustment for fitting skewness of Pearson III distribution
#  fitdistrplus - fit the weibull and pearson III distributions for volume freq anal
#  ggplot2  - created plots
#  openxlsx - open the Excel spreadsheet to compare my results to old methods
#  PearsonDS- Pearson III distribution functions
#  plyr     - split-apply-combine paradigm
#  rehape2  - melt and cast data frames
#  zoo      - compute rolling averages
