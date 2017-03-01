## ------------------------------------------------------------------------
library(plyr)       # split-apply-combine paradigm

library(BCWaterDischargeAnalysis)


data(S08HA011)
S08HA011$Year <- as.numeric(format(S08HA011$Date, "%Y"))
head( S08HA011[ S08HA011$Year ==1965,])

## ------------------------------------------------------------------------
Station.Code <- '08HA011'
Station.Name <- 'Cowichen River near Duncan'
Station.Area <- 826    # square km's

start.year   <- 1965  # when do you want the analysis to start at.
end.year     <- 2012  # what is last year with complete data

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


## ---- warning=FALSE------------------------------------------------------
stat.annual$dates.missing.flow

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

