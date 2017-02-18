# Analysis of Streamflow data
# Created as part of Contract GSA-IS17JHQ0177
# Author  Carl James Schwarz
#         StatMathComp Consulting by Schwarz, Inc.
#         cschwarz.statsfuca@gmail.com
# Change Log
#     2017-01-30 CJS First Edition

options(width=200)  # how wide to print output

# load the functions used in the analysis
# This will also load the relevant libraries -- see the loadlibraries file in the Functions folder for details
invisible(lapply(file.path("../../Functions",list.files(path=file.path("../../Functions"), pattern = "(?i)[.]r$", recursive = TRUE)),
       function(x){
         cat("Loading ",x,"\n");
         source(x)}))

# Define variables that provide options for the analysis

Station.Code <- '08LG048'
Station.Name <- 'Coldwater River near Brookmere'
Station.Area <- 316    # square km's

start.year   <- 1966  # when do you want the analysis to start at.
end.year     <- 2014  # what is last year with complete data

start.year.vfa <- start.year+2 ############ NOTE ######### vfa start year is two years later than Excel workbook
end.year.vfa   <- end.year

# Get the data
flow <- read.csv(file.path("08LG048_Date.csv"), header=TRUE, as.is=TRUE, strip.white=TRUE, skip=1)
head(flow)
xtabs(~PARAM, data=flow, exclude=NULL, na.action=na.pass)
# only select param==1
flow <- flow[ flow$PARAM==1,]
flow$Q <- flow$Value

# Create a date variable
flow$Date <- as.Date(flow$Date, "%Y/%m/%d")
sum(is.na(flow$Date))


# basic structure of flow
str(flow)

# Create the directory for the results of the analysis
# If you are comparing to the spreadsheet, the spreadsheet must also be in this directory.
# Similarly, if you are comparing to the HEC-SSP results, the vfa.rpt file must also be in this directory
#report.dir <- file.path("08HA011","MinFlows") # directory for output files and save objects
report.dir <- '.'  # current director
dir.create(report.dir)
cat("Reports and saved files will be found in ", report.dir, "\n")
E.filename <- file.path(report.dir,"08LG048_COLDWATER_STREAMFLOW_SUMMARY.xlsx")


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

stat.annual <- compute.Q.stat.annual(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year,
                          write.stat.csv=TRUE,        # write out statistics
                          write.stat.trans.csv=TRUE,  # write out statistics in transposed format
                          report.dir=report.dir,
                          na.rm=na.rm)


names(stat.annual)

head(stat.annual$Q.stat.annual)
tail(stat.annual$Q.stat.annual)

head(stat.annual$Q.stat.annual.trans)
head(stat.annual$dates.missing.flows)

stat.annual$file.stat.csv
stat.annual$file.stat.trans.csv

# Compare the annual statistics with those in the Excel spreasheet
compare.annual <- compare.annual.stat(Station.Code,
                                      Q.filename=stat.annual$file.stat.csv,
                                      E.filename=E.filename,
                                      SW_translate=SW_translate,
                                      report.dir=report.dir,
                                      write.plots.pdf=TRUE,
                                      write.comparison.csv=TRUE)

names(compare.annual)

compare.annual$stats.in.Q.not.in.E
compare.annual$stats.in.E.not.in.Q

head(compare.annual$diff.stat)
compare.annual$diff.stat[ grepl("cumq", compare.annual$diff.stat$stat, ignore.case=TRUE),]

# some difference
compare.annual$diff.stat[ is.na(compare.annual$diff.stat$pdiff),]
compare.annual$diff.stat[ compare.annual$diff.stat$pdiff >= .01 & !is.na(compare.annual$diff.stat$pdiff),]

compare.annual$diff.stat$file.plots.pdf

names(compare.annual$plot.list)
l_ply(compare.annual$plot.list, function(x){plot(x)})




#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the long-term statistics

stat.longterm <- compute.Q.stat.longterm(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year,
                          write.stat.csv=TRUE,        # write out statistics
                          write.stat.trans.csv=TRUE,
                          report.dir=report.dir)  # write out statistics in transposed format

names(stat.longterm)

head(stat.longterm$Q.stat.longterm)
tail(stat.longterm$Q.stat.longterm)

head(stat.longterm$Q.stat.longterm.trans)

stat.longterm$file.stat.csv
stat.longterm$file.stat.trans.csv


# Compare the longterm statistics with those in the Excel spreasheet
compare.longterm <- compare.longterm.stat(Station.Code,
                                      Q.filename=stat.longterm$file.stat.csv,
                                      E.filename=E.filename,
                                      report.dir=report.dir,
                                      write.comparison.csv=TRUE,
                                      write.plots.pdf=TRUE)
names(compare.longterm)

compare.longterm$stats.in.Q.not.in.E
compare.longterm$stats.in.E.not.in.Q

head(compare.longterm$diff.stat)
compare.longterm$diff.stat[ is.na(compare.longterm$diff.stat$pdiff),]

names(compare.longterm$plot.list)
l_ply(compare.longterm$plot.list, function(x){plot(x)})


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the long-term percentile statistics

percentile.longterm <- compute.Q.percentile.longterm(Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow=flow,
                          start.year=start.year,
                          end.year=end.year,
                          write.stat.csv=TRUE,        # write out statistics
                          write.stat.trans.csv=TRUE,  # write out statistics in transposed format
                          report.dir=report.dir)

names(percentile.longterm)

head(percentile.longterm$Q.percentile.stat)
tail(percentile.longterm$Q.percentile.stat)

head(percentile.longterm$Q.percentile.stat.trans)

percentile.longterm$file.stat.csv
percentile.longterm$file.stat.trans.csv


# Compare the longterm percentile statistics with those in the Excel spreasheet
compare.percentile.longterm <- compare.percentile.longterm.stat(Station.Code,
                                      Q.filename=percentile.longterm$file.stat.csv,
                                      E.filename=E.filename,
                                      write.comparison.csv=TRUE,
                                      write.plots.pdf=TRUE,
                                      report.dir=report.dir)
names(compare.percentile.longterm)

compare.percentile.longterm$stats.in.Q.not.in.E
compare.percentile.longterm$stats.in.E.not.in.Q

head(compare.percentile.longterm$diff.stat)

names(compare.percentile.longterm$plot.list)
l_ply(compare.percentile.longterm$plot.list, function(x){plot(x)})




#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Compute the volume frequency analysis (similar to HEC SSP)

#****** Notice that HEC starts 2 years later ******
vfa.analysis <- compute.volume.frequency.analysis( Station.Code=Station.Code, flow,
                      start.year=start.year.vfa, end.year=end.year.vfa, use.water.year=FALSE,
                      roll.avg.days=c(1,3,7,15),
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

vfa.analysis$freqplot
ggsave(plot=vfa.analysis$freqplot,
       file=file.path(report.dir, paste(Station.Code,"-annual-vfa-frequency-plot.png",sep="")), h=6, w=6, units="in", dpi=300)

vfa.analysis$fitted.quantiles.trans

HEC.filename <- file.path(report.dir,"COLDWATER_FREQ.rpt")
compare.with.HEC <- compare.frequency.with.hec(Station.Code,
                                               Q.file.stat=vfa.analysis$file.stat.csv,
                                               Q.file.plotdata=vfa.analysis$file.plotdata.csv,
                                               Q.file.quantile=vfa.analysis$file.quantile.csv,
                                               HEC.filename=HEC.filename,
                                               report.dir=report.dir,
                                               write.comparison.csv=TRUE,
                                               write.plots.pdf=TRUE)
names(compare.with.HEC)

compare.with.HEC$stats.in.Q.not.in.HEC
compare.with.HEC$stats.in.HEC.not.in.Q

head(compare.with.HEC$diff.stat)

names(compare.with.HEC$plot.list)
l_ply(compare.with.HEC$plot.list, function(x){plot(x)})

# show the differences between the MLE and moment estimates (?) from HEC
compare.with.HEC$diff.stat[ grepl('-q$', compare.with.HEC$diff.stat$Measure),]




