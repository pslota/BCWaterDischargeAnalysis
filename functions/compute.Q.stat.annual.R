#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis
# 
#   2017-01-30 CSJ First edition

compute.Q.stat.annual <- function(Station.code='XXXXX', 
                          Station.Area=NA, 
                          flow, 
                          start.year=9999, 
                          end.year=0,
                          write.stat=FALSE,        # write out statistics 
                          write.stat.trans=FALSE,  # write out statistics in transposed format
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
#    write.stat   - write out statistics to csv file - file name is returned
#    write.stat.trans - write out transposed statistics to csv file - file name is returned
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
   library(car)         # recode function
   library(plyr)        # split-apply-combine 
   library(reshape2)    # reorganize data (melting and casting)
   library(zoo)         # rolling averages

   if( !is.character(Station.Code))  {stop("Station Code muste be a character string.")}
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
   if( !is.logical(write.stat))      {stop("write.stat must be logical (TRUE/FALSE")}
   if( !is.logical(write.stat.trans)){stop("write.stat.trans must be logical (TRUE/FALSE")}
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

#  Compute statistics on annual basis
#  
   Q.stat.annual <- plyr::ddply(flow[ flow$Year >= start.year,], "Year", function(fy, Station.Area, na.rm){
     # process each year's flow values (fy)
     SW_1_day_MIN    = min(fy$Q, na.rm=na.rm$na.rm.global)	      # Annual Min Daily Q 
     SW_1_day_MINDOY = as.numeric(format( fy$Date[which.min( fy$Q)], "%j"))        # Date of Annual Min Daily Q
     if(length(SW_1_day_MINDOY)==0) SW_1_day_MINDOY <- NA
     SW_3_day_MIN	   = min(fy$Q.03DAvg, na.rm=na.rm$na.rm.global) # Min Annual Rolling 3 day avg 
     SW_3_day_MINDOY = as.numeric(format( fy$Date[which.min( fy$Q.03DAvg)], "%j")) # Date of Min Annual Rolling 3 day avg
     if(length(SW_3_day_MINDOY)==0) SW_3_day_MINDOY <- NA
     SW_7_day_MIN	   = min(fy$Q.07DAvg, na.rm=na.rm$na.rm.global) # Min Annual Rolling 7 day avg 
     SW_7_day_MINDOY = as.numeric(format( fy$Date[which.min( fy$Q.07DAvg)], "%j")) # Date of Min Annual Rolling 7 day avg
     if(length(SW_7_day_MINDOY)==0) SW_7_day_MINDOY <- NA
     SW_30_day_MIN	 = min(fy$Q.30DAvg, na.rm=na.rm$na.rm.global) # Min Annual Rolling 30 day avg 
     SW_30_day_MINDOY= as.numeric(format( fy$Date[which.min( fy$Q.30DAvg)], "%j")) # Date of Min Annual Rolling 30 day avg
     if(length(SW_30_day_MINDOY)==0) SW_30_day_MINDOY <- NA
     SW_ANNUAL_MIN   = min (fy$Q, na.rm=na.rm$na.rm.global)	    # Annual Min Daily Q 	Annual Min Daily Q
     SW_ANNUAL_MAX	 = max (fy$Q, na.rm=na.rm$na.rm.global)      # Annual Max Daily Q
     SW_ANNUAL_MEAN  = mean(fy$Q, na.rm=na.rm$na.rm.global)     # Annual Mean Discharge (Based on Daily avgs)
     SW_ANNUAL_TOTALQ= mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Q)*60*60*24    # Yearly sum of daily avg (cms) *60*60*24 # deal with missing values
     SW_ANNUAL_YIELDMM=mean(fy$Q, na.rm=na.rm$na.rm.global)*60*60*24*365.25/Station.Area/1000   #	(Annual Mean*60*60*24*365.25)/(area in km2*1000000))*1000

     # Assign seasons to the month (JFM, AMJ, JJA, OND)
     fy$Season <- car::recode(fy$Month, 
                       "1:3='JFM'; 4:6='AMJ'; 7:9='JAS'; 10:12='OND'; else=NA")
     fy$Season <- factor(fy$Season, levels=c("JFM",'AMJ','JAS','OND'), order=TRUE)
   
     # compute totalQ and assign variable names for total Q
     # SW_JFM_TOTALQ	Jan+Feb+Mar sum of daily avg (cms) *60*60*24
     # SW_AMJ_TOTALQ	Apr+May+Jun  sum of daily avg (cms) *60*60*24
     # SW_JAS_TOTALQ	Jul+Aug+Sep  sum of daily avg (cms) *60*60*24
     # SW_OND_TOTALQ	Oct+Nov+Dec  sum of daily avg (cms) *60*60*24
     season.stats <- plyr::ddply(fy, "Season", function(fy, na.rm){
                               vname.totalq  <- paste("SW_",fy$Season[1],"_TOTALQ",sep="")
                               totalq <- mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Season)*60*60*24  # deal with missing values
                               data.frame(vname.totalq, totalq, stringsAsFactors=FALSE)
                               },na.rm=na.rm)
     season.stats$vname.totalq <- factor(season.stats$vname.totalq, levels=season.stats$vname.totalq, order=TRUE) # keep ordering

     # compute Yield and assign variable names for yield
     # SW_JFM_YIELDMM	(JFM_TotalQ/(area in km2*1000000))*1000
     # SW_AMJ_YIELDMM	(AMJ_TotalQ/(area in km2*1000000))*1000
     # SW_JAS_YIELDMM	(JAS_TotalQ/(area in km2*1000000))*1000
     # SW_OND_YIELDMM	(OND_TotalQ/(area in km2*1000000))*1000
     season.stats$vname.yieldmm <- sub('TOTALQ','YIELDMM', season.stats$vname.totalq)
     season.stats$yieldmm       <- season.stats$totalq/Station.Area/1000
     season.stats$vname.yieldmm <- factor(season.stats$vname.yieldmm, levels=season.stats$vname.yieldmm, order=TRUE) # keep ordering
  
     # extract the variables for adding to the data frame later
     SW_Season_TOTALQ <- reshape2::acast(season.stats, .~vname.totalq, value.var='totalq')
     SW_Season_YIELDMM<- reshape2::acast(season.stats, .~vname.yieldmm,value.var='yieldmm')
    
     # Monthly values 
     # JAN_MIN_DAILY_SW	Min Daily Avg Q for Jan     
     # JAN_MAX_DAILY_SW	Max Daily Avg Q for Jan     
     # JAN_MEAN_SW	Mean Monthly Q for Jan 
     # JAN_P50_SW	Median Monthly Q for Jan   
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
     month.stats$vname.mean<- paste(month.name[month.stats$Month],"_MEAN_SW",sep="")
     month.stats$vname.p50 <- paste(month.name[month.stats$Month],"_P50_SW",sep="")
     month.stats$vname.p80 <- paste(month.name[month.stats$Month],"_P80_SW",sep="")
     month.stats$vname.p90 <- paste(month.name[month.stats$Month],"_P90_SW",sep="")

     month.stats$vname.min <- factor(month.stats$vname.min, levels=month.stats$vname.min, order=TRUE)
     month.stats$vname.max <- factor(month.stats$vname.max, levels=month.stats$vname.max, order=TRUE)
     month.stats$vname.mean<- factor(month.stats$vname.mean,levels=month.stats$vname.mean,order=TRUE)
     month.stats$vname.p50 <- factor(month.stats$vname.p50, levels=month.stats$vname.p50, order=TRUE)
     month.stats$vname.p80 <- factor(month.stats$vname.p80, levels=month.stats$vname.p80, order=TRUE)
     month.stats$vname.p90 <- factor(month.stats$vname.p90, levels=month.stats$vname.p90, order=TRUE)

     SW_Month_MIN <- reshape2::acast(month.stats, .~vname.min , value.var='month.min')
     SW_Month_MAX <- reshape2::acast(month.stats, .~vname.max , value.var='month.max')
     SW_Month_MEAN<- reshape2::acast(month.stats, .~vname.mean, value.var='month.mean')
     SW_Month_P50 <- reshape2::acast(month.stats, .~vname.p50,  value.var='month.p50')
     SW_Month_P80 <- reshape2::acast(month.stats, .~vname.p80,  value.var='month.p80')
     SW_Month_P90 <- reshape2::acast(month.stats, .~vname.p90,  value.var='month.p90')

     # return the annual results
     res <- data.frame(
             SW_1_day_MIN,
             SW_1_day_MINDOY, 
             SW_3_day_MIN,	   
             SW_3_day_MINDOY, 
             SW_7_day_MIN,	   
             SW_7_day_MINDOY, 
             SW_30_day_MIN,	
             SW_30_day_MINDOY,
             SW_ANNUAL_MIN,   
             SW_ANNUAL_MAX,	 
             SW_ANNUAL_MEAN,  
             SW_ANNUAL_TOTALQ,
             SW_ANNUAL_YIELDMM,
             SW_Season_TOTALQ,
             SW_Season_YIELDMM,
             SW_Month_MIN,
             SW_Month_MAX,
             SW_Month_MEAN,
             SW_Month_P50,
             SW_Month_P80,
             SW_Month_P90,
             stringsAsFactors=FALSE)
   }, Station.Area=Station.Area, na.rm=na.rm)

#  Compute the statistics on a water year basis  (October -> Sept)

  Q.stat.wyear <- plyr::ddply(flow[ flow$WYear >= start.year,], "WYear", function(fy, Station.Area, na.rm){
     # process each waters year's flow values (fy)
     # SW_Oct_to_Sept_TOTALQ	Oct through Sept sum of daily avg (cms) *60*60*24
     # SW_Oct_to_Sept_YIELDMM	(Oct_to_Sep_TotalQ/(area in km2*1000000))*1000

     SW_Oct_to_Sept_TOTALQ <- mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Q)*60*60*24 
     SW_Oct_to_Sept_YIELDMM<- SW_Oct_to_Sept_TOTALQ /Station.Area/1000

     # half water-year statistics
     # Assign seasons to the month (AMJJAS ONDJFM)
     fy$Season <- car::recode(fy$Month, 
                       "1:3='ONDJFM'; 4:9='AMJJAS'; 10:12='ONDJFM'; else=NA")
     fy$Season <- factor(fy$Season, levels=c("ONDJFM",'AMJJAS'), order=TRUE)
   
     # compute totalQ and assign variable names for total Q
     # SW_AMJJAS_TotalQ	Apr through Sep  sum of daily avg (cms) *60*60*24
     # SW_ONDJFM_TOTALQ	Oct through Mar sum of daily avg (cms) *60*60*24
     season.stats <- plyr::ddply(fy, "Season", function(fy, na.rm){
                               vname.totalq  = paste("SW_",fy$Season[1],"_TOTALQ",sep="")
                               totalq = mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Season)*60*60*24  # deal with missing values
                               data.frame(vname.totalq, totalq, stringsAsFactors=FALSE)
                               }, na.rm=na.rm)
     season.stats$vname.totalq <- factor(season.stats$vname.totalq, levels=season.stats$vname.totalq, order=TRUE) # keep ordering

     # compute Yield and assign variable names for yield
     # SW_AMJJAS_YIELDMM	(AMJJAS_TotalQ/(area in km2*1000000))*1000
     # SW_ONDJFM_YIELDMM	(ONDJFM_TotalQ/(area in km2*1000000))*1000
     season.stats$vname.yieldmm <- sub('TOTALQ','YIELDMM', season.stats$vname.totalq)
     season.stats$yieldmm       <- season.stats$totalq/Station.Area/1000
     season.stats$vname.yieldmm <- factor(season.stats$vname.yieldmm, levels=season.stats$vname.yieldmm, order=TRUE) # keep ordering
  
     # extract the variables for adding to the data frame later
     SW_Season_TOTALQ <- reshape2::acast(season.stats, .~vname.totalq, value.var='totalq')
     SW_Season_YIELDMM<- reshape2::acast(season.stats, .~vname.yieldmm,value.var='yieldmm')

     # return the results
     res <- data.frame(
             SW_Oct_to_Sept_TOTALQ,
             SW_Oct_to_Sept_YIELDMM, 
             SW_Season_TOTALQ,
             SW_Season_YIELDMM,
             stringsAsFactors=FALSE)
  }, Station.Area=Station.Area,  na.rm=na.rm)

  # merge the annual and water year statistics
  Q.stat <- merge(Q.stat.annual, Q.stat.wyear, by.x='Year', by.y="WYear", all.x=TRUE)

  # See if you want to write out the summary tables?
  file.stat <- NA
  if(write.stat){
     # Write out the summary table for comparison to excel spreadsheet
     file.stat<- file.path(report.dir, paste(Station.Code,"-annual-summary-stat.csv", sep=""))
     write.csv(Q.stat,file=file.stat, row.names=FALSE)
  }

  # Write out the annual summary table in transposed format?
  file.stat.trans<- NA
  Year <- Q.stat[,"Year"]
  Q.stat.trans <- t(Q.stat[, !grepl('^Year', names(Q.stat))])
  colnames(Q.stat.trans) <- paste("Y",Year,sep="")
  if(write.stat.trans){
    file.stat.trans <-file.path(report.dir,paste(Station.Code,"-annual-summary-stat-trans.csv",sep=""))
    write.csv(Q.stat.trans, file=file.stat.trans, row.names=TRUE)
  }
  return(list(Q.stat.annual=Q.stat,
               Q.stat.annual.trans=Q.stat.trans,
               dates.missing.flows=dates.missing.flows,
               file.stat=file.stat,
               file.stat.trans=file.stat.trans,
               na.rm = na.rm,
               Date=Sys.time()))
} # end of function

