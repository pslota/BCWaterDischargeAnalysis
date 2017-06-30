## ------------------------------------------------------------------------
library(plyr)       # split-apply-combine paradigm

library(BCWaterDischargeAnalysis)


data(WSC.08HA011)
WSC.08HA011$Year <- as.numeric(format(WSC.08HA011$Date, "%Y"))
head( WSC.08HA011[ WSC.08HA011$Year ==1965,])

## ------------------------------------------------------------------------
Station.Code <- 'WSC-08HA011'
Station.Name <- 'Cowichan River near Duncan'
Station.Area <- 826    # square km's

start.year   <- 1965  # when do you want the analysis to start at.
end.year     <- 2012  # what is last year with complete data

## ------------------------------------------------------------------------
# Create 1% missing values (approximately)
set.seed(3423432)
sum(is.na(WSC.08HA011[ start.year <= WSC.08HA011$Year & WSC.08HA011$Year <= end.year, "Q"]))

missing <- sample(1:nrow(WSC.08HA011), .0003*nrow(WSC.08HA011))
WSC.08HA011$Q[ missing ] <- NA
sum(is.na(WSC.08HA011[ start.year <= WSC.08HA011$Year & WSC.08HA011$Year <= end.year, "Q"]))

## ------------------------------------------------------------------------
# get a simple summary of the data
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

## ---- warning=FALSE------------------------------------------------------
stat.annual <- compute.Q.stat.annual(
                          Station.Code  =Station.Code,
                          Station.Area  =Station.Area,
                          flow          =WSC.08HA011,
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
                          flow          =WSC.08HA011,
                          start.year    =start.year,
                          end.year      =end.year,
                          na.rm=list(na.rm.global=TRUE))
head(stat.annual.ignore.missing$Q.stat.annual[, 1:10])

## ---- warning=FALSE------------------------------------------------------
stat.longterm <- compute.Q.stat.longterm(
                          Station.Code =Station.Code,
                          #Station.Area =Station.Area,
                          flow         =WSC.08HA011,
                          start.year   =start.year,
                          end.year     =end.year)
head(stat.longterm$Q.cy.stat.longterm)

## ---- warning=FALSE------------------------------------------------------
stat.longterm.missing <- compute.Q.stat.longterm(
                          Station.Code =Station.Code,
                          #Station.Area =Station.Area,
                          flow         =WSC.08HA011,
                          start.year   =start.year,
                          end.year     =end.year,
                          na.rm=list(na.rm.global=FALSE))
head(stat.longterm.missing$Q.cy.stat.longterm)

## ---- warning=FALSE------------------------------------------------------
percentile.longterm <- compute.Q.percentile.longterm(
                          Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow        =WSC.08HA011,
                          start.year  =start.year,
                          end.year    =end.year)
head(percentile.longterm$Q.percentile.stat)

## ---- warning=FALSE------------------------------------------------------
percentile.longterm.missing <- compute.Q.percentile.longterm(
                          Station.Code=Station.Code,
                          Station.Area=Station.Area,
                          flow        =WSC.08HA011,
                          start.year  =start.year,
                          end.year    =end.year,
                          na.rm=list(na.rm.global=FALSE))
head(percentile.longterm.missing$Q.percentile.stat)

## ---- warning=FALSE------------------------------------------------------
vfa.analysis <- compute.volume.frequency.analysis( 
                      Station.Code   =Station.Code, 
                      flow           =WSC.08HA011,
                      start.year     =start.year, 
                      end.year       =end.year)
vfa.analysis$fitted.quantiles.trans

## ---- warning=FALSE------------------------------------------------------
vfa.analysis.missing <- compute.volume.frequency.analysis( 
                      Station.Code   =Station.Code, 
                      flow           =WSC.08HA011,
                      start.year     =start.year, 
                      end.year       =end.year,
                      na.rm=list(na.rm.global=FALSE))
vfa.analysis.missing$fitted.quantiles.trans

## ---- warning=FALSE------------------------------------------------------
plyr::ddply(vfa.analysis.missing$plotdata, "Measure", plyr::summarize, n.years=length(Measure))
plyr::ddply(vfa.analysis$plotdata        , "Measure", plyr::summarize, n.years=length(Measure))

