## ------------------------------------------------------------------------
library(ggplot2)
library(plyr)       # split-apply-combine paradigm
library(reshape2)   # for melting and casting

library(BCWaterDischargeAnalysis)


data(WSC.08HA011)
head( WSC.08HA011[ as.numeric(format(WSC.08HA011$Date, "%Y"))==1965,])

## ------------------------------------------------------------------------
Station.Code <- 'WSC-08HA011'
Station.Name <- 'Cowichan River near Duncan'
Station.Area <- 826    # square km's

start.year   <- 1965  # when do you want the analysis to start at.
end.year     <- 2012  # what is last year with complete data

## ------------------------------------------------------------------------
cat("Number of illegal date values ", sum(is.na(WSC.08HA011$Date)), "\n")
WSC.08HA011[ is.na(WSC.08HA011$Date),]     # sorry Microsoft, but feb 29 1900 is NOT a leap year

dim(WSC.08HA011)  # size before removing illegal dates
WSC.08HA011 <- WSC.08HA011[ !is.na(WSC.08HA011$Date),]
dim(WSC.08HA011)  # size after removing illegal dates

# get a simple summary of the data
WSC.08HA011$Year <- as.numeric(format(WSC.08HA011$Date, "%Y")) # add the year to the data frame
# Table of simple statistics by year to help check for outliers, etc
WSC.08HA011.sum <- plyr::ddply(WSC.08HA011[ WSC.08HA011$Year >= start.year & WSC.08HA011$Year <=end.year,], "Year", plyr::summarize,
         n.days   = length(Year),
         n.Q      = sum (!is.na(Q)),
         n.miss.Q = sum ( is.na(Q)),
         min.Q    = min (Q, na.rm=TRUE),
         max.Q    = max (Q, na.rm=TRUE),
         mean.Q   = mean(Q,na.rm=TRUE),
         sd.Q     = sd  (Q,na.rm=TRUE))
WSC.08HA011.sum

## ------------------------------------------------------------------------
# visuallize the min, max, and mean
plotdata <- reshape2::melt(WSC.08HA011.sum,
             id.var='Year',
             measure.var=c("min.Q","max.Q","mean.Q","sd.Q"),
             variable.name='Statistic',
             value.name='Value')
ggplot2::ggplot(data=plotdata, aes(x=Year, y=Value))+
  ggtitle("Summary statistics about Q over time")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()+
  facet_wrap(~Statistic, ncol=2, scales="free_y")


## ---- warning=FALSE------------------------------------------------------
stat.annual <- compute.Q.stat.annual(
                          Station.Code  =Station.Code,
                          Station.Area  =Station.Area,
                          flow          =WSC.08HA011,
                          start.year    =start.year,
                          end.year      =end.year)


## ------------------------------------------------------------------------
names(stat.annual)

## ------------------------------------------------------------------------
head(stat.annual$Q.stat.annual[, 1:10])

## ------------------------------------------------------------------------
stat.annual$file.stat.csv
stat.annual$file.stat.trans.csv
stat.annual$file.stat.trend.pdf

## ---- warning=FALSE------------------------------------------------------
stat.longterm <- compute.Q.stat.longterm(
                          Station.Code =Station.Code,
                          Station.Area =Station.Area,
                          flow         =WSC.08HA011,
                          start.year   =start.year,
                          end.year     =end.year)


## ------------------------------------------------------------------------
names(stat.longterm)

## ------------------------------------------------------------------------
head(stat.longterm$Q.stat.longterm)

## ------------------------------------------------------------------------
stat.longterm$file.stat.csv
stat.longterm$file.stat.trans.csv

## ---- warning=FALSE------------------------------------------------------
percentile.longterm <- compute.Q.percentile.longterm(
                          Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow        =WSC.08HA011,
                          start.year  =start.year,
                          end.year    =end.year)


## ------------------------------------------------------------------------
names(percentile.longterm)

## ------------------------------------------------------------------------
head(percentile.longterm$Q.percentile.stat)

## ------------------------------------------------------------------------
percentile.longterm$file.stat.csv
percentile.longterm$file.stat.trans.csv

## ---- warning=FALSE------------------------------------------------------
vfa.analysis <- compute.volume.frequency.analysis( 
                      Station.Code   =Station.Code, 
                      flow           =WSC.08HA011,
                      start.year     =start.year, 
                      end.year       =end.year)


## ------------------------------------------------------------------------
names(vfa.analysis)

## ------------------------------------------------------------------------
vfa.analysis$freqplot

## ------------------------------------------------------------------------
vfa.analysis$fitted.quantiles.trans

## ------------------------------------------------------------------------
vfa.analysis$file.stat.csv 
vfa.analysis$file.quantile.csv
vfa.analysis$file.frequency.plot


