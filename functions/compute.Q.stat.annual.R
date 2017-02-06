#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis
# 
#   2017-01-30 CJS First edition

compute.Q.stat.annual <- function(Station.code='XXXXX', 
                          Station.Area=NA, 
                          flow, 
                          start.year=9999, 
                          end.year=0,
                          write.stat.csv=FALSE,        # write out statistics 
                          write.stat.trans.csv=FALSE,  # write out statistics in transposed format
                          write.flow.summary.csv=TRUE, # write out a summary of period of record
                          plot.stat.trend=TRUE,       # should you plot all of stat trends?
                          report.dir=".",
                          na.rm=list(na.rm.global=FALSE)){
#  Compute statistics on an annual (calendar and water) year basis
#  Input
#    Station.Code - character string indentifying the station with the flow
#    Station.Area - area of water basin behind the station needed for some statistics
#    flow - data frame with variables
#              Date - date (as R data variable type, usually constructed as using as.Date()
#              Q    - flow values
#           All other variables in the data frame will be ignored.
#           Data does NOT have to be sorted by Date order.
#
#           A water year runs from 1 October of the previous year to 30 Sept of the current year
#           So the 2001 water year runs from 2000-10-01 to 2001-09-30.
#
#           The initial data can extend before and after the start.year and end.year. This is useful for
#           computing the 3, 7, and 30 day rolling averages at the start of the calendar year.
#           This is also useful for computing statistics on the water year for the start.year. 
#
#           It is recommended that data be present from the October prior to 1 January of the start year
#           to reduce the total number of missing data.
#
#           Missing flow values can be entered by leaving out the date, or entering NA for that date.
#
#    start.year, end year - starting and ending year for statistics e.g. start.year=1960, end.year=2013
#    write.stat.csv   - write out statistics to csv file - file name is returned
#    write.stat.trans.csv - write out transposed statistics to csv file - file name is returned
#    directory where the csv files for the statistics and transposed statistics are saved
#    na.rm- list of what to do with missing values. Only specify the ones you wish to change in the list
#           using a call similar to compute.Q.stat.annual(... na.rm=list(na.rm.global=TRUE))
#           na.rm.global - all NA's removed before statistics computed
#
#
#  Output: List with the following objects
#    Q.stat.annual - statistics on water and calendar year
#    Q.stat.annual.trans - statistics on water and calendary year in transposed format
#    dates.missing.flows - vector with dates that are missing flow values
#    file.stat   - file name of csv file created
#    file.stat.trans - file name of transposed statistics.
#
#############################################################
#  Some basic error checking on the input parameters
#
   Version <- '2017-02-04'
   library(car)         # recode function
   library(plyr)        # split-apply-combine 
   library(reshape2)    # reorganize data (melting and casting)
   library(zoo)         # rolling averages

   if( !is.character(Station.Code))  {stop("Station Code must be a character string.")}
   if(length(Station.Code)>1)        {stop("Station.Code cannot have length > 1")}
   if( !is.numeric(Station.Area))    {stop("Station.Area must be numeric")}
   if(length(Station.Area)>1)        {stop("Station.Area cannot have length > 1")}
   if( !is.data.frame(flow))         {stop("Flow is not a data frame.")}
   if(! all(c("Date","Q") %in% names(flow))){
                                      stop("Flow dataframe doesn't contain the variables Date and Q.")}
   if( ! inherits(flow$Date[1], "Date")){
                                      stop("Date column in Flow data frame is not a date.")}
   if( !is.numeric(flow$Q))          {stop("Q column in flow dataframe is not numeric.")}
   if( any(flow$Q <0, na.rm=TRUE))   {stop('flow cannot have negative values - check your data')}
   if(! (is.numeric(start.year) & is.numeric(end.year))){
                                      stop("start.year and end.year not numberic.")}
   if(! (start.year <= end.year))    {stop("start.year > end.year")}
   if( !is.logical(write.stat.csv))  {stop("write.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.stat.trans.csv)){stop("write.stat.trans.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.flow.summary.csv)){stop("write.flow.summary.csv must be logical (TRUE/FALSE")}
   if( !is.logical(plot.stat.trend)) {stop("plot.stat.trend must be logical (TRUE/FALSE")}

   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}
   if( !is.list(na.rm))              {stop("na.rm is not a list") }
   if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
   my.na.rm <- list(na.rm.global=FALSE)
   if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
   my.na.rm[names(na.rm)]<- na.rm
   na.rm <- my.na.rm  # set the na.rm for the rest of the function.

  #  Generate all dates between min and max dates and merge with flow
  #  data frame to generate any dates that were missing.
  #  This will automatically generate NA for the days that were not in the file
   temp <- data.frame(Date=seq(min(flow$Date,na.rm=TRUE),
                               max(flow$Date,na.rm=TRUE),1))
   flow <- merge(flow, temp, all.y=TRUE)

#  Compute the 3, 7, and 30 day rolling average values
   flow$Q.03DAvg <- zoo::rollapply( flow$Q,  3, mean, fill=NA, align="right")
   flow$Q.07DAvg <- zoo::rollapply( flow$Q,  7, mean, fill=NA, align="right")
   flow$Q.30DAvg <- zoo::rollapply( flow$Q, 30, mean, fill=NA, align="right")

#  Truncate flow data to between 1 October of (start.year-1) and 31 December of end.year
#  We go back to 1 Oct of start.year-1 to enable water year computations if data is present
   flow <- flow[ flow$Date >= as.Date(paste(start.year-1,'10-01',sep='-'), "%Y-%m-%d") &
                 flow$Date <= as.Date(paste(end.year    ,'12-31',sep='-'), '%Y-%m-%d'),]

#  Generate all dates between 1 Oct start.year-1 and 31 Dec end.year and merge with flow
#  data frame to generate any dates that were missing.
#  This will automatically generate NA for the days that were not in the file
   temp <- data.frame(Date=seq(as.Date(paste(start.year-1,'10-01',sep='-'), "%Y-%m-%d"),
                               as.Date(paste(end.year    ,'12-31',sep='-'), '%Y-%m-%d'), 1))
   flow <- merge(flow, temp, all.y=TRUE)

#  create the year (annual and water) and month variables
   flow$Year  <- as.numeric(format(flow$Date, "%Y"))
   flow$Month <- as.numeric(format(flow$Date, '%m'))
   flow$WYear <- flow$Year
   flow$WYear[ flow$Month %in% 10:12] <- flow$WYear[flow$Month %in% 10:12 ] +1

#  which dates have missing flows.
   dates.missing.flows <- flow$Date[ is.na(flow$Q) & 
                               as.numeric(format(flow$Date,"%Y"))>=start.year &
                               as.numeric(format(flow$Date,"%Y"))<=end.year  ]
   
#  simple summary statistics   
   flow.sum <- plyr::ddply(flow[ flow$Year >= start.year & flow$Year <=end.year,], "Year", plyr::summarize,
         n.days   = length(Year),
         n.Q      = sum (!is.na(Q)),
         n.miss.Q = sum ( is.na(Q)),
         min.Q    = min (Q, na.rm=TRUE),
         max.Q    = max (Q, na.rm=TRUE),
         mean.Q   = mean(Q,na.rm=TRUE),
         sd.Q     = sd  (Q,na.rm=TRUE))
   flow.sum

#  Compute statistics on annual basis
#  
   Q.stat.annual <- plyr::ddply(flow[ flow$Year >= start.year,], "Year", function(fy, Station.Area, na.rm){
     # process each year's flow values (fy)
     ANNUAL_MIN_01Day_SW    = min(fy$Q, na.rm=na.rm$na.rm.global)	      # Annual Min Daily Q 
     ANNUAL_MINDOY_01Day_SW = as.numeric(format( fy$Date[which.min( fy$Q)], "%j"))        # Date of Annual Min Daily Q
     if(length(ANNUAL_MINDOY_01Day_SW)==0) ANNUAL_MINDOY_01Day_SW <- NA

     ANNUAL_MIN_03Day_SW	   = min(fy$Q.03DAvg, na.rm=na.rm$na.rm.global) # Min Annual Rolling 3 day avg 
     ANNUAL_MINDOY_03Day_SW = as.numeric(format( fy$Date[which.min( fy$Q.03DAvg)], "%j")) # Date of Min Annual Rolling 3 day avg
     if(length(ANNUAL_MINDOY_03Day_SW)==0) ANNUAL_MINDOY_03Day_SW <- NA

     ANNUAL_MIN_07Day_SW	   = min(fy$Q.07DAvg, na.rm=na.rm$na.rm.global) # Min Annual Rolling 7 day avg 
     ANNUAL_MINDOY_07Day_SW = as.numeric(format( fy$Date[which.min( fy$Q.07DAvg)], "%j")) # Date of Min Annual Rolling 7 day avg
     if(length(ANNUAL_MINDOY_07Day_SW)==0) ANNUAL_MINDOY_07Day_SW <- NA

     ANNUAL_MIN_30Day_SW	 = min(fy$Q.30DAvg, na.rm=na.rm$na.rm.global) # Min Annual Rolling 30 day avg 
     ANNUAL_MINDOY_30Day_SW= as.numeric(format( fy$Date[which.min( fy$Q.30DAvg)], "%j")) # Date of Min Annual Rolling 30 day avg
     if(length(ANNUAL_MINDOY_30Day_SW)==0) ANNUAL_MINDOY_30Day_SW <- NA

     ANNUAL_MIN_DAILY_SW   = min (fy$Q, na.rm=na.rm$na.rm.global)	    # Annual Min Daily Q 	Annual Min Daily Q
     ANNUAL_MAX_DAILY_SW	 = max (fy$Q, na.rm=na.rm$na.rm.global)      # Annual Max Daily Q
     ANNUAL_MEAN_DAILY_SW  = mean(fy$Q, na.rm=na.rm$na.rm.global)     # Annual Mean Discharge (Based on Daily avgs)
     ANNUAL_TOTALQ_DAILY_SW= mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Q)*60*60*24    # Yearly sum of daily avg (cms) *60*60*24 # deal with missing values
     ANNUAL_CUMQ_DAILY_SW  = ANNUAL_TOTALQ_DAILY_SW/(60*60*24)      # Yearly sum of daily avg (cms) # deal with missing values
     ANNUAL_YIELDMM_DAILY_SW=mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Q)*60*60*24 /Station.Area/1000   #	(Annual Mean*60*60*24*365.25)/(area in km2*1000000))*1000

     # Get the cumulative Q values 
     # Notice that if missing values are removed, the individual values are replaced by zero
     fy$Q2 <- fy$Q
     if(na.rm$na.rm.globa) fy$Q2[ is.na(fy$Q2)] <- 0
     fy$CumQ <- cumsum(fy$Q2)    
     # what is the first date where 25, 50 and 75% of totalQ (see above) are found
     ANNUAL_Date_25P_CUMQ_DAILY_SW <- fy$Date[ which.max( fy$CumQ > 0.25 *ANNUAL_CUMQ_DAILY_SW)]
     ANNUAL_Date_50P_CUMQ_DAILY_SW <- fy$Date[ which.max( fy$CumQ > 0.50 *ANNUAL_CUMQ_DAILY_SW)]
     ANNUAL_Date_75P_CUMQ_DAILY_SW <- fy$Date[ which.max( fy$CumQ > 0.75 *ANNUAL_CUMQ_DAILY_SW)]
     ANNUAL_Date_25P_CUMQ_DAILY_SW <- as.numeric(format(ANNUAL_Date_25P_CUMQ_DAILY_SW, "%j"))
     ANNUAL_Date_50P_CUMQ_DAILY_SW <- as.numeric(format(ANNUAL_Date_50P_CUMQ_DAILY_SW, "%j"))
     ANNUAL_Date_75P_CUMQ_DAILY_SW <- as.numeric(format(ANNUAL_Date_75P_CUMQ_DAILY_SW, "%j"))
    
     # Assign seasons to the month (JFM, AMJ, JJA, OND)
     fy$Season <- car::recode(fy$Month, 
                       "1:3='JFM'; 4:6='AMJ'; 7:9='JAS'; 10:12='OND'; else=NA")
     fy$Season <- factor(fy$Season, levels=c("JFM",'AMJ','JAS','OND'), order=TRUE)
   
     # compute totalQ and assign variable names for total Q
     # JFM_TOTALQ_DAILY_SW	Jan+Feb+Mar  sum of daily avg (cms) *60*60*24
     # AMJ_TOTALQ_DAILY_SW	Apr+May+Jun  sum of daily avg (cms) *60*60*24
     # JAS_TOTALQ	DAILY_SW  Jul+Aug+Sep  sum of daily avg (cms) *60*60*24
     # OND_TOTALQ_DAILY_SW  Oct+Nov+Dec  sum of daily avg (cms) *60*60*24
     season.stats <- plyr::ddply(fy, "Season", function(fy, na.rm){
                               vname.totalq  <- paste(fy$Season[1],"_TOTALQ_DAILY_SW",sep="")
                               totalq <- mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Season)*60*60*24  # deal with missing values
                               data.frame(vname.totalq, totalq, stringsAsFactors=FALSE)
                               },na.rm=na.rm)
     season.stats$vname.totalq <- factor(season.stats$vname.totalq, levels=season.stats$vname.totalq, order=TRUE) # keep ordering

     # compute Yield and assign variable names for yield
     # JFM_YIELDMM_DAILY_SW	(JFM_TOTALQ_DAILY_SW/(area in km2*1000000))*1000
     # AMJ_YIELDMM_DAILY_SW	(AMJ_TOTALQ_DAILY_SW/(area in km2*1000000))*1000
     # JAS_YIELDMM_DAILY_SW	(JAS_TOTALQ_DAILY_SW/(area in km2*1000000))*1000
     # OND_YIELDM_DAILY_SWM	(OND_TOTALQ_DAILY_SW/(area in km2*1000000))*1000
     season.stats$vname.yieldmm <- sub('TOTALQ','YIELDMM', season.stats$vname.totalq)
     season.stats$yieldmm       <- season.stats$totalq/Station.Area/1000
     season.stats$vname.yieldmm <- factor(season.stats$vname.yieldmm, levels=season.stats$vname.yieldmm, order=TRUE) # keep ordering
  
     # extract the variables for adding to the data frame later
     Season_TOTALQ_DAILY_SW <- reshape2::acast(season.stats, .~vname.totalq, value.var='totalq')
     Season_YIELDMM_DAILY_SW<- reshape2::acast(season.stats, .~vname.yieldmm,value.var='yieldmm')
    
     # Monthly values 
     # JAN_MIN_DAILY_SW	  Min Daily Avg Q for Jan     
     # JAN_MAX_DAILY_SW	  Max Daily Avg Q for Jan     
     # JAN_MEAN_DAILY_SW	Mean Monthly Q for Jan 
     # JAN_P50_DAILY_SW	  Median Monthly Q for Jan   
     # JAN_P90_SW	Low Flow, 90th Percentile for Jan  -- see contract - this is the 10th percentile
     # JAN_P80_SW	Low Flow, 80th Percentile for Jan  -- see contract - this is the 20th percentile
   
     month.stats <- plyr::ddply(fy, "Month", function(fy, na.rm){
                        month.min = min   (fy$Q, na.rm=na.rm$na.rm.global)
                        month.max = max   (fy$Q, na.rm=na.rm$na.rm.global)
                        month.mean= mean  (fy$Q, na.rm=na.rm$na.rm.global)
                        month.p50 = median(fy$Q, na.rm=na.rm$na.rm.global)
                        month.p90 = try(quantile(fy$Q, prob=.10, na.rm=na.rm$na.rm.global), TRUE)  # quantile bombs is na present and na.rm=FALSE !
                        if (class(month.p90) == "try-error") month.p90 <- NA
                        month.p80 = try(quantile(fy$Q, prob=.20, na.rm=na.rm$na.rm.global), TRUE)
                        if (class(month.p80) == "try-error") month.p80 <- NA
                        data.frame(month.min, month.max, month.mean,
                                   month.p50, month.p80, month.p90, stringsAsFactors=FALSE)
                        },na.rm=na.rm)
     month.name<- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
     # create variable names for each type of category
     month.stats$vname.min <- paste(month.name[month.stats$Month],"_MIN_DAILY_SW", sep="")
     month.stats$vname.max <- paste(month.name[month.stats$Month],"_MAX_DAILY_SW", sep="")
     month.stats$vname.mean<- paste(month.name[month.stats$Month],"_MEAN_DAILY_SW",sep="")
     month.stats$vname.p50 <- paste(month.name[month.stats$Month],"_P50_DAILY_SW",sep="")
     month.stats$vname.p80 <- paste(month.name[month.stats$Month],"_P80_DAILY_SW",sep="")
     month.stats$vname.p90 <- paste(month.name[month.stats$Month],"_P90_DAILY_SW",sep="")

     month.stats$vname.min <- factor(month.stats$vname.min, levels=month.stats$vname.min, order=TRUE)
     month.stats$vname.max <- factor(month.stats$vname.max, levels=month.stats$vname.max, order=TRUE)
     month.stats$vname.mean<- factor(month.stats$vname.mean,levels=month.stats$vname.mean,order=TRUE)
     month.stats$vname.p50 <- factor(month.stats$vname.p50, levels=month.stats$vname.p50, order=TRUE)
     month.stats$vname.p80 <- factor(month.stats$vname.p80, levels=month.stats$vname.p80, order=TRUE)
     month.stats$vname.p90 <- factor(month.stats$vname.p90, levels=month.stats$vname.p90, order=TRUE)

     Month_MIN_DAILY_SW <- reshape2::acast(month.stats, .~vname.min , value.var='month.min')
     Month_MAX_DAILY_SW <- reshape2::acast(month.stats, .~vname.max , value.var='month.max')
     Month_MEAN_DAILY_SW<- reshape2::acast(month.stats, .~vname.mean, value.var='month.mean')
     Month_P50_DAILY_SW <- reshape2::acast(month.stats, .~vname.p50,  value.var='month.p50')
     Month_P80_DAILY_SW <- reshape2::acast(month.stats, .~vname.p80,  value.var='month.p80')
     Month_P90_DAILY_SW <- reshape2::acast(month.stats, .~vname.p90,  value.var='month.p90')

     # return the annual results
     res <- data.frame(
             ANNUAL_MIN_01Day_SW,
             ANNUAL_MINDOY_01Day_SW, 
             ANNUAL_MIN_03Day_SW,	   
             ANNUAL_MINDOY_03Day_SW, 
             ANNUAL_MIN_07Day_SW,	   
             ANNUAL_MINDOY_07Day_SW, 
             ANNUAL_MIN_30Day_SW,	
             ANNUAL_MINDOY_30Day_SW,
             ANNUAL_MIN_DAILY_SW,   
             ANNUAL_MAX_DAILY_SW,	 
             ANNUAL_MEAN_DAILY_SW,  
             ANNUAL_TOTALQ_DAILY_SW,
             ANNUAL_YIELDMM_DAILY_SW,
             ANNUAL_CUMQ_DAILY_SW,
             ANNUAL_Date_25P_CUMQ_DAILY_SW, 
             ANNUAL_Date_50P_CUMQ_DAILY_SW,
             ANNUAL_Date_75P_CUMQ_DAILY_SW,
             Season_TOTALQ_DAILY_SW,
             Season_YIELDMM_DAILY_SW,
             Month_MIN_DAILY_SW,
             Month_MAX_DAILY_SW,
             Month_MEAN_DAILY_SW,
             Month_P50_DAILY_SW,
             Month_P80_DAILY_SW,
             Month_P90_DAILY_SW,
             stringsAsFactors=FALSE)
   }, Station.Area=Station.Area, na.rm=na.rm)

#  Compute the statistics on a water year basis  (October -> Sept)

  Q.stat.wyear <- plyr::ddply(flow[ flow$WYear >= start.year,], "WYear", function(fy, Station.Area, na.rm){
     # process each waters year's flow values (fy)
     # Oct_to_Sept_TOTALQ_DAILY_SW	Oct through Sept sum of daily avg (cms) *60*60*24
     # Oct_to_Sept_YIELDMM_DAILY_SW	(Oct_to_Sep_TotalQ/(area in km2*1000000))*1000

     Oct_to_Sep_TOTALQ_DAILY_SW <- mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Q)*60*60*24 
     Oct_to_Sep_YIELDMM_DAILY_SW<- Oct_to_Sep_TOTALQ_DAILY_SW /Station.Area/1000

     # half water-year statistics
     # Assign seasons to the month (AMJJAS ONDJFM)
     fy$Season <- car::recode(fy$Month, 
                       "1:3='ONDJFM'; 4:9='AMJJAS'; 10:12='ONDJFM'; else=NA")
     fy$Season <- factor(fy$Season, levels=c("ONDJFM",'AMJJAS'), order=TRUE)
   
     # compute totalQ and assign variable names for total Q
     # AMJJAS_TotalQ_DAILY_SW	Apr through Sep  sum of daily avg (cms) *60*60*24
     # ONDJFM_TOTALQ_DAILY_SW 	Oct through Mar sum of daily avg (cms) *60*60*24
     season.stats <- plyr::ddply(fy, "Season", function(fy, na.rm){
                               vname.totalq  = paste(fy$Season[1],"_TOTALQ_DAILY_SW",sep="")
                               totalq = mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Season)*60*60*24  # deal with missing values
                               data.frame(vname.totalq, totalq, stringsAsFactors=FALSE)
                               }, na.rm=na.rm)
     season.stats$vname.totalq <- factor(season.stats$vname.totalq, levels=season.stats$vname.totalq, order=TRUE) # keep ordering

     # compute Yield and assign variable names for yield
     # AMJJAS_YIELDMM_DAILY_SW	(AMJJAS_TotalQ/(area in km2*1000000))*1000
     # ONDJFM_YIELDMM_DAILY_SW	(ONDJFM_TotalQ/(area in km2*1000000))*1000
     season.stats$vname.yieldmm <- sub('TOTALQ','YIELDMM', season.stats$vname.totalq)
     season.stats$yieldmm       <- season.stats$totalq/Station.Area/1000
     season.stats$vname.yieldmm <- factor(season.stats$vname.yieldmm, levels=season.stats$vname.yieldmm, order=TRUE) # keep ordering
  
     # extract the variables for adding to the data frame later
     Season_TOTALQ_DAILY_SW  <- reshape2::acast(season.stats, .~vname.totalq, value.var='totalq')
     Season_YIELDMM_DAILY_SW <- reshape2::acast(season.stats, .~vname.yieldmm,value.var='yieldmm')

     # return the results
     res <- data.frame(
             Oct_to_Sep_TOTALQ_DAILY_SW,
             Oct_to_Sep_YIELDMM_DAILY_SW, 
             Season_TOTALQ_DAILY_SW,
             Season_YIELDMM_DAILY_SW,
             stringsAsFactors=FALSE)
  }, Station.Area=Station.Area,  na.rm=na.rm)

  # merge the annual and water year statistics
  Q.stat <- merge(Q.stat.annual, Q.stat.wyear, by.x='Year', by.y="WYear", all.x=TRUE)

  file.summary.csv <- NA
  if(write.flow.summary.csv){
     # write out the flow summary
     file.summary.csv <- file.path(report.dir, paste(Station.Code,"-period-record-summary.csv", sep=""))
     write.csv(flow.sum,file=file.summary.csv, row.names=FALSE)
  }
  
  # See if you want to write out the summary tables?
  file.stat.csv <- NA
  if(write.stat.csv){
     # Write out the summary table for comparison to excel spreadsheet
     file.stat.csv <- file.path(report.dir, paste(Station.Code,"-annual-summary-stat.csv", sep=""))
     write.csv(Q.stat,file=file.stat.csv, row.names=FALSE)
  }

  # Write out the annual summary table in transposed format?
  file.stat.trans.csv<- NA
  Year <- Q.stat[,"Year"]
  Q.stat.trans <- t(Q.stat[, !grepl('^Year', names(Q.stat))])
  colnames(Q.stat.trans) <- paste("Y",Year,sep="")
  if(write.stat.trans.csv){
    file.stat.trans.csv <-file.path(report.dir,paste(Station.Code,"-annual-summary-stat-trans.csv",sep=""))
    write.csv(Q.stat.trans, file=file.stat.trans.csv, row.names=TRUE)
  }
   
  # Make a plot of all of the statistics over time and save to a pdf file
  file.stat.trend.pdf <- NA
  if(plot.stat.trend){
     file.stat.trend.pdf <- file.path(report.dir, paste(Station.Code,"-annual-trend.pdf",sep=""))
     plotdata <- reshape2::melt(Q.stat, id.var="Year", variable.name="Statistic", value.name="Value")
     plotdata$statgroup <- NA
     plotdata$transform <- ""
     set.annual.min  <- grepl("^ANNUAL_MIN_", plotdata$Statistic) 
     plotdata$statgroup [set.annual.min] <- 'Annual Minimums'

     set.annual.mindoy  <- grepl("^ANNUAL_MINDOY", plotdata$Statistic) 
     plotdata$statgroup [set.annual.mindoy] <- 'Annual Day of Year for Minimums'

     set.month.min     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb,"_MIN",sep=""))
     plotdata$statgroup [set.month.min] <- 'Monthly minimums'
     plotdata$transform [set.month.min] <- "log"

     set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb,"_MAX",sep=""))
     set.month.max     <- set.month.max | grepl("ANNUAL_MAX", plotdata$Statistic)
     plotdata$statgroup [set.month.max] <- 'Monthly/Annual maximums'
     plotdata$transform [set.month.max] <- "log"
     
     set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb,"_MEAN",sep=""))
     set.month.mean     <- set.month.mean | grepl("ANNUAL_MEAN", plotdata$Statistic)
     plotdata$statgroup [set.month.mean] <- 'Monthly/Annual means'
     plotdata$transform [set.month.mean] <- "log"

     set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb,"_P50",sep=""))
     plotdata$statgroup [set.month.p50] <- 'Monthly P50'
     plotdata$transform [set.month.p50] <- "log"
     
     set.month.p80     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb,"_P80",sep=""))
     plotdata$statgroup [set.month.p80] <- 'Monthly P80'
     plotdata$transform [set.month.p80] <- "log"
     
     set.month.p90     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb,"_P90",sep=""))
     plotdata$statgroup [set.month.p90] <- 'Monthly P90'
     plotdata$transform [set.month.p90] <- "log"

     set.yieldmm     <- grepl("YIELDMM", plotdata$Statistic)
     plotdata$statgroup [set.yieldmm] <- 'Yield (MM)'
     plotdata$transform [set.yieldmm] <- "log"

     set.totalq     <- grepl("TOTALQ", plotdata$Statistic)
     plotdata$statgroup [set.totalq] <- 'Total Q'
     plotdata$transform [set.totalq] <- "log"
 
     set.annual.date     <- grepl("ANNUAL_Date", plotdata$Statistic)
     plotdata$statgroup [set.annual.date] <- 'Annual Date of total discharge milestones'

     plotdata$statgroup[ is.na(plotdata$statgroup)] <- "Misc statistics"
     pdf(file.stat.trend.pdf, h=8, w=11)
        d_ply(plotdata, "statgroup", function(x){
           myplot <- ggplot(data=x, aes(x=Year, y=Value, group=Statistic, color=Statistic, linetype=Statistic))+
             ggtitle(paste(Station.Code, " - Trend for ", x$statgroup[1]))+
             geom_point()+
             geom_line()+xlab("Year")+ylab("Value of statistic")
           if(x$transform[1] == 'log'){
             myplot <- myplot + ylab("Value of log(statistic)")+scale_y_continuous(trans='log10')
           }
           plot(myplot)
        })                           
      dev.off()
  }
  return(list( Q.flow.summary=flow.sum,
               Q.stat.annual=Q.stat,
               Q.stat.annual.trans=Q.stat.trans,
               dates.missing.flows=dates.missing.flows,
               file.stat.csv=file.stat.csv,
               file.stat.trans.csv=file.stat.trans.csv,
               file.stat.trend.pdf=file.stat.trend.pdf,
               file.summary.csv=file.summary.csv,
               na.rm = na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function

