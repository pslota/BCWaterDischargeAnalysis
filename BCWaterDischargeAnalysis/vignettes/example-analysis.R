## ------------------------------------------------------------------------
library(ggplot2)
library(plyr)       # split-apply-combine paradigm
library(reshape2)   # for melting and casting

library(BCWaterDischargeAnalysis)


data(S08HA011)
head( S08HA011[ as.numeric(format(S08HA011$Date, "%Y"))==1965,])

## ------------------------------------------------------------------------
Station.Code <- '08HA011'
Station.Name <- 'Upper Cowichan River'
Station.Area <- 826    # square km's

start.year   <- 1965  # when do you want the analysis to start at.
end.year     <- 2012  # what is last year with complete data

## ------------------------------------------------------------------------
cat("Number of illegal date values ", sum(is.na(S08HA011$Date)), "\n")
S08HA011[ is.na(S08HA011$Date),]     # sorry Microsoft, but feb 29 1900 is NOT a leap year

dim(S08HA011)  # size before removing illegal dates
S08HA011 <- S08HA011[ !is.na(S08HA011$Date),]
dim(S08HA011)  # size after removing illegal dates

# get a simple summary of the data
S08HA011$Year <- as.numeric(format(S08HA011$Date, "%Y")) # add the year to the data frame
# Table of simple statistics by year to help check for outliers, etc
S08HA011.sum <- plyr::ddply(S08HA011[ S08HA011$Year >= start.year & S08HA011$Year <=end.year,], "Year", plyr::summarize,
         n.days   = length(Year),
         n.Q      = sum (!is.na(Q)),
         n.miss.Q = sum ( is.na(Q)),
         min.Q    = min (Q, na.rm=TRUE),
         max.Q    = max (Q, na.rm=TRUE),
         mean.Q   = mean(Q,na.rm=TRUE),
         sd.Q     = sd  (Q,na.rm=TRUE))
S08HA011.sum

## ------------------------------------------------------------------------
# visuallize the min, max, and mean
plotdata <- reshape2::melt(S08HA011.sum,
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
                          flow          =S08HA011,
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
                          flow         =S08HA011,
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
                          flow        =S08HA011,
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
                      Station.Code   =Station.Code, S08HA011,
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


## ------------------------------------------------------------------------
data(S08HA011)
head( S08HA011[ as.numeric(format(S08HA011$Date, "%Y"))==1965,])

## ------------------------------------------------------------------------
Station.Code <- '08HA011'
Station.Name <- 'Upper Cowichan River'
Station.Area <- 826    # square km's

start.year   <- 1965  # when do you want the analysis to start at.
end.year     <- 2012  # what is last year with complete data

# add Year to the data frame
S08HA011$Year <- as.numeric(format(S08HA011$Date, "%Y")) # add the year to the data frame

## ------------------------------------------------------------------------
# Create 1% missing values (approximately)
set.seed(3423432)
sum(is.na(S08HA011[ start.year <= S08HA011$Year & S08HA011$Year <= end.year, "Q"]))

missing <- sample(1:nrow(S08HA011), .0003*nrow(S08HA011))
S08HA011$Q[ missing ] <- NA
sum(is.na(S08HA011[ start.year <= S08HA011$Year & S08HA011$Year <= end.year, "Q"]))

## ------------------------------------------------------------------------
# get a simple summary of the data
# Table of simple statistics by year to help check for outliers, etc
S08HA011.sum <- plyr::ddply(S08HA011[ S08HA011$Year >= start.year & S08HA011$Year <=end.year,], "Year", plyr::summarize,
         n.days   = length(Year),
         n.Q      = sum (!is.na(Q)),
         n.miss.Q = sum ( is.na(Q)),
         min.Q    = min (Q, na.rm=TRUE),
         max.Q    = max (Q, na.rm=TRUE),
         mean.Q   = mean(Q,na.rm=TRUE),
         sd.Q     = sd  (Q,na.rm=TRUE))
S08HA011.sum

## ---- warning=FALSE------------------------------------------------------
stat.annual <- compute.Q.stat.annual(
                          Station.Code  =Station.Code,
                          Station.Area  =Station.Area,
                          flow          =S08HA011,
                          start.year    =start.year,
                          end.year      =end.year)


## ------------------------------------------------------------------------
head(stat.annual$Q.stat.annual[, 1:10])

## ---- warning=FALSE------------------------------------------------------
stat.annual.ignore.missing <- compute.Q.stat.annual(
                          Station.Code  =Station.Code,
                          Station.Area  =Station.Area,
                          flow          =S08HA011,
                          start.year    =start.year,
                          end.year      =end.year,
                          na.rm=list(na.rm.global=TRUE))
head(stat.annual.ignore.missing$Q.stat.annual[, 1:10])

## ---- warning=FALSE------------------------------------------------------
stat.longterm <- compute.Q.stat.longterm(
                          Station.Code =Station.Code,
                          Station.Area =Station.Area,
                          flow         =S08HA011,
                          start.year   =start.year,
                          end.year     =end.year)
head(stat.longterm$Q.stat.longterm)

## ---- warning=FALSE------------------------------------------------------
stat.longterm.missing <- compute.Q.stat.longterm(
                          Station.Code =Station.Code,
                          Station.Area =Station.Area,
                          flow         =S08HA011,
                          start.year   =start.year,
                          end.year     =end.year,
                          na.rm=list(na.rm.global=FALSE))
head(stat.longterm.missing$Q.stat.longterm)

## ---- warning=FALSE------------------------------------------------------
percentile.longterm <- compute.Q.percentile.longterm(
                          Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow        =S08HA011,
                          start.year  =start.year,
                          end.year    =end.year)
head(percentile.longterm$Q.percentile.stat)

## ---- warning=FALSE------------------------------------------------------
percentile.longterm.missing <- compute.Q.percentile.longterm(
                          Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow        =S08HA011,
                          start.year  =start.year,
                          end.year    =end.year,
                          na.rm=list(na.rm.global=FALSE))
head(percentile.longterm.missing$Q.percentile.stat)

## ---- warning=FALSE------------------------------------------------------
vfa.analysis <- compute.volume.frequency.analysis( 
                      Station.Code   =Station.Code, S08HA011,
                      start.year     =start.year, 
                      end.year       =end.year)
vfa.analysis$fitted.quantiles.trans

## ---- warning=FALSE------------------------------------------------------
vfa.analysis.missing <- compute.volume.frequency.analysis( 
                      Station.Code   =Station.Code, S08HA011,
                      start.year     =start.year, 
                      end.year       =end.year,
                      na.rm=list(na.rm.global=FALSE))
vfa.analysis.missing$fitted.quantiles.trans

## ---- warning=FALSE------------------------------------------------------
plyr::ddply(vfa.analysis.missing$plotdata, "Measure", plyr::summarize, n.years=length(Measure))
plyr::ddply(vfa.analysis$plotdata        , "Measure", plyr::summarize, n.years=length(Measure))

