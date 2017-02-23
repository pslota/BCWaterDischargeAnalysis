# Copyright 2017 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Sample script demonstrating the analysis

# turn off prompting for plots (not needed outside of the demo)
devAskNewPage(ask = FALSE)


library(ggplot2)   # for plotting
library(plyr)      # for summarizing
library(reshape2)  # for melting


options(width=200)  # how wide to print output

# load the functions used in the analysis
# This will also load the relevant libraries -- see the loadlibraries file in the Functions folder for details
library(BCWaterDischargeAnalysis)

# Define variables that provide options for the analysis

Station.Code <- '08HA011'
Station.Name <- 'Upper Cowichan River'
Station.Area <- 826    # square km's

start.year   <- 1965  # when do you want the analysis to start at.
end.year     <- 2012  # what is last year with complete data

# Get the data
# The raw data is in the package in the inst/extdata subdirectory.
# We access the raw data using the system.file() command.
# See http://r-pkgs.had.co.nz/data.html for details.
# Normally, you would just give the actual file name in the read.csv() function below.

input.csv <- system.file("extdata", "2012.csv", package = "testdat")

flow <- read.csv(input.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)

# Create a date variable
flow$Date <- as.Date(paste(flow$Year,flow$Month, flow$Day, sep="-"), "%Y-%m-%d")

# basic structure of flow
str(flow)

# Create the directory for the results of the analysis
# If you are comparing to the spreadsheet, the spreadsheet must also be in this directory.
# Similarly, if you are comparing to the HEC-SSP results, the vfa.rpt file must also be in this directory
#report.dir <- file.path("08HA011","MinFlows") # directory for output files and save objects
report.dir <- '.'  # current director
dir.create(report.dir)
cat("Reports and saved files will be found in ", report.dir, "\n")

#----------------------------------------------------------------
# Some preliminary screening

# Are there any illegal dates that could not be created?
cat("Number of illegal date values ", sum(is.na(flow$Date)), "\n")
flow[ is.na(flow$Date),]     # sorry Microsoft, but feb 29 1900 is NOT a leap year

dim(flow)  # size before removing illegal dates
flow <- flow[ !is.na(flow$Date),]
dim(flow)  # size after removing illegal dates

flow$Year <- as.numeric(format(flow$Date, "%Y"))
# Table of simple statistics by year to help check for outliers, etc
flow.sum <- plyr::ddply(flow[ flow$Year >= start.year & flow$Year <=end.year,], "Year", plyr::summarize,
         n.days   = length(Year),
         n.Q      = sum (!is.na(Q)),
         n.miss.Q = sum ( is.na(Q)),
         min.Q    = min (Q, na.rm=TRUE),
         max.Q    = max (Q, na.rm=TRUE),
         mean.Q   = mean(Q,na.rm=TRUE),
         sd.Q     = sd  (Q,na.rm=TRUE))
flow.sum

# visuallize the min, max, and mean
plotdata <- reshape2::melt(flow.sum,
             id.var='Year',
             measure.var=c("min.Q","max.Q","mean.Q","sd.Q"),
             variable.name='Statistic',
             value.name='Value')
ggplot2::ggplot(data=plotdata, aes(x=Year, y=Value))+
  ggtitle("Summary statistics about Q over time")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  facet_wrap(~Statistic, ncol=2, scales="free_y")


#------------------------------------------------------------------------------
# Some preliminary plots to check for outliers etc
ggplot2::ggplot(data=flow[flow$Year >= start.year,], aes(x=Date, y=Q))+
   ggtitle("Q by date")+
   theme(plot.title = element_text(hjust = 0.5))+
   geom_line(aes(group=Year))

ggplot2::ggplot(data=flow[flow$Year >= start.year,], aes(x=Date, y=log(Q)))+
   ggtitle("log(Q) by date")+
   theme(plot.title = element_text(hjust = 0.5))+
   geom_line(aes(group=Year))

ggplot2::ggplot(data=flow[flow$Year >= start.year,], aes(x=Date, y=log(Q)))+
   ggtitle("Q by date")+
   theme(plot.title = element_text(hjust = 0.5))+
   geom_line()+
   facet_wrap(~Year, scales="free_x")




#--------------------------------------------------------------
# Compute the statistics on an annual basis


na.rm=list(na.rm.global=TRUE)

# Create the directory for the results of the analysis
# If you are comparing to the spreadsheet, the spreadsheet must also be in this directory.
# Similarly, if you are comparing to the HEC-SSP results, the vfa.rpt file must also be in this directory

report.dir <- '.'  # current director
dir.create(report.dir)
cat("Reports and saved files will be found in ", report.dir, "\n")

help(compute.Q.stat.annual)

# the defaults are usually good enough
stat.annual <- compute.Q.stat.annual(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year)

# but you have control over which files to be written and NA removal etc. See help file
stat.annual <- compute.Q.stat.annual(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year,
                          write.stat.csv=TRUE,        # write out statistics
                          write.stat.trans.csv=TRUE,  # write out statistics in transposed format
                          plot.stat.trend=TRUE,
                          report.dir=report.dir,
                          na.rm=na.rm)


names(stat.annual)

head(stat.annual$Q.stat.annual)
tail(stat.annual$Q.stat.annual)

head(stat.annual$Q.stat.annual.trans)

head(stat.annual$dates.missing.flows)

stat.annual$file.stat.csv
stat.annual$file.stat.trans.csv
stat.annual$file.stat.trend.pdf


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the long-term statistics

stat.longterm <- compute.Q.stat.longterm(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year)

names(stat.longterm)

head(stat.longterm$Q.stat.longterm)
tail(stat.longterm$Q.stat.longterm)

head(stat.longterm$Q.stat.longterm.trans)

stat.longterm$file.stat.csv
stat.longterm$file.stat.trans.csv




#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the long-term percentile statistics

percentile.longterm <- compute.Q.percentile.longterm(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year)


names(percentile.longterm)

head(percentile.longterm$Q.percentile.stat)
tail(percentile.longterm$Q.percentile.stat)

head(percentile.longterm$Q.percentile.stat.trans)

percentile.longterm$file.stat.csv
percentile.longterm$file.stat.trans.csv



#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the volume frequency analysis (similar to HEC SSP)

vfa.analysis <- compute.volume.frequency.analysis(
                      Station.Code=Station.Code,
                      flow        =flow,
                      start.year  =start.year,
                      end.year    =end.year)

help(compute.volume.frequency.analysis)


vfa.analysis <- compute.volume.frequency.analysis(
                      Station.Code=Station.Code,
                      flow        =flow,
                      start.year  =start.year,
                      end.year    =end.year,
                      use.water.year=TRUE,
                      use.log=FALSE,
                      use.max=FALSE,
                      fit.distr="PIII",
                      write.stat.csv=TRUE,
                      write.plotdata.csv=TRUE,
                      write.quantiles.csv=TRUE,
                      report.dir=report.dir)
names(vfa.analysis)

vfa.analysis$file.stat.csv

head(vfa.analysis$Q.stat)
head(vfa.analysis$Q.stat.trans)

# you can access the frequence plot directly
vfa.analysis$freqplot

# you can change features of the plot
new.plot <- vfa.analysis$freqplot + ggtitle("A new title")

# you can save this revised plot in the usual way.
ggsave(plot=new.plot,
       file=file.path(report.dir, paste(Station.Code,"-annual-vfa-frequency-plot.png",sep="")), h=6, w=6, units="in", dpi=300)

vfa.analysis$fitted.quantiles.trans

