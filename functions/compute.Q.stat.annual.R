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
                        write.lowflow.csv=TRUE,      # write out a summary of low flows
                        plot.stat.trend=TRUE,        # should you plot all of stat trends?
                        plot.cumdepart=TRUE,         # plot cumulative departure curves
                        report.dir=".",
                        na.rm=list(na.rm.global=FALSE),
                        csv.nddigits=3,              # decimal digits for csv files for statistics
                        debug=FALSE){           
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
 Version <- '2017-02-15'
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
 if( !is.logical(write.lowflow.csv)){ stop("write.lowflow.csv must be logical (TRUE/FALSE)")}
 if( !is.logical(plot.stat.trend)) {stop("plot.stat.trend must be logical (TRUE/FALSE")}
 if( !is.logical(plot.cumdepart))  {stop("plot.cumdepart must be logical (TRUE/FALSE")}

 if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}
 if( !is.numeric(csv.nddigits))  { stop("csv.ndddigits needs to be numeric")}
 csv.nddigits <- round(csv.nddigits[1])  # number of decimal digits for rounding in csv files
 
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
       median.Q = median(Q,na.rm=TRUE),
       sd.Q     = sd  (Q,na.rm=TRUE))
 flow.sum

 if(debug)browser()
#  Compute statistics on calendar year basis
#  
 Q.stat.cy <- plyr::ddply(flow[ flow$Year >= start.year,], "Year", function(fy, Station.Area, na.rm){
   # process each year's flow values (fy)
   CY_MIN_01Day_SW    = min(fy$Q, na.rm=na.rm$na.rm.global)	      # CY Min Daily Q 
   CY_MINDOY_01Day_SW = as.numeric(format( fy$Date[which.min( fy$Q)], "%j"))        # Date of CY Min Daily Q
   if(length(CY_MINDOY_01Day_SW)==0) CY_MINDOY_01Day_SW <- NA

   CY_MIN_03Day_SW	   = min(fy$Q.03DAvg, na.rm=na.rm$na.rm.global) # Min CY Rolling 3 day avg 
   CY_MINDOY_03Day_SW = as.numeric(format( fy$Date[which.min( fy$Q.03DAvg)], "%j")) # Date of Min CY Rolling 3 day avg
   if(length(CY_MINDOY_03Day_SW)==0) CY_MINDOY_03Day_SW <- NA

   CY_MIN_07Day_SW	   = min(fy$Q.07DAvg, na.rm=na.rm$na.rm.global) # Min CY Rolling 7 day avg 
   CY_MINDOY_07Day_SW = as.numeric(format( fy$Date[which.min( fy$Q.07DAvg)], "%j")) # Date of Min CY Rolling 7 day avg
   if(length(CY_MINDOY_07Day_SW)==0) CY_MINDOY_07Day_SW <- NA

   CY_MIN_30Day_SW	 = min(fy$Q.30DAvg, na.rm=na.rm$na.rm.global) # Min CY Rolling 30 day avg 
   CY_MINDOY_30Day_SW= as.numeric(format( fy$Date[which.min( fy$Q.30DAvg)], "%j")) # Date of Min CY Rolling 30 day avg
   if(length(CY_MINDOY_30Day_SW)==0) CY_MINDOY_30Day_SW <- NA

   CY_MIN_DAILY_SW   = min (fy$Q, na.rm=na.rm$na.rm.global)	    # CY Min Daily Q 	CY Min Daily Q
   CY_MAX_DAILY_SW	 = max (fy$Q, na.rm=na.rm$na.rm.global)      # CY Max Daily Q
   CY_MEAN_DAILY_SW  = mean(fy$Q, na.rm=na.rm$na.rm.global)     # CY Mean Discharge (Based on Daily avgs)
   CY_TOTALQ_DAILY_SW= mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Q)*60*60*24    # Yearly sum of daily avg (cms) *60*60*24 # deal with missing values
   CY_CUMQ_DAILY_SW  = CY_TOTALQ_DAILY_SW/(60*60*24)      # Yearly sum of daily avg (cms) # deal with missing values
   CY_YIELDMM_DAILY_SW=mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Q)*60*60*24 /Station.Area/1000   #	(CY Mean*60*60*24*365.25)/(area in km2*1000000))*1000

   # Get the cumulative Q values 
   # Notice that if missing values are removed, the individual values are replaced by zero
   fy$Q2 <- fy$Q
   if(na.rm$na.rm.globa) fy$Q2[ is.na(fy$Q2)] <- 0
   fy$CumQ <- cumsum(fy$Q2)    
   # what is the first date where 25, 50 and 75% of totalQ (see above) are found
   CY_Date_25P_CUMQ_DAILY_SW <- fy$Date[ match(TRUE, fy$CumQ > 0.25 *CY_CUMQ_DAILY_SW)]
   CY_Date_50P_CUMQ_DAILY_SW <- fy$Date[ match(TRUE, fy$CumQ > 0.50 *CY_CUMQ_DAILY_SW)]
   CY_Date_75P_CUMQ_DAILY_SW <- fy$Date[ match(TRUE, fy$CumQ > 0.75 *CY_CUMQ_DAILY_SW)]
   CY_Date_25P_CUMQ_DAILY_SW <- as.numeric(format(CY_Date_25P_CUMQ_DAILY_SW, "%j"))
   CY_Date_50P_CUMQ_DAILY_SW <- as.numeric(format(CY_Date_50P_CUMQ_DAILY_SW, "%j"))
   CY_Date_75P_CUMQ_DAILY_SW <- as.numeric(format(CY_Date_75P_CUMQ_DAILY_SW, "%j"))
  
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
   # JAN_P10_SW	Low Flow, 90th Percentile for Jan  -- see contract - this is the 10th percentile
   # JAN_P20_SW	Low Flow, 80th Percentile for Jan  -- see contract - this is the 20th percentile
 
   month.stats <- plyr::ddply(fy, "Month", function(fy, na.rm){
                      month.min = min   (fy$Q, na.rm=na.rm$na.rm.global)
                      month.max = max   (fy$Q, na.rm=na.rm$na.rm.global)
                      month.mean= mean  (fy$Q, na.rm=na.rm$na.rm.global)
                      month.p50 = median(fy$Q, na.rm=na.rm$na.rm.global)
                      month.p10 = try(quantile(fy$Q, prob=.10, na.rm=na.rm$na.rm.global), TRUE)  # quantile bombs is na present and na.rm=FALSE !
                      if (class(month.p10) == "try-error") month.p10 <- NA
                      month.p20 = try(quantile(fy$Q, prob=.20, na.rm=na.rm$na.rm.global), TRUE)
                      if (class(month.p20) == "try-error") month.p20 <- NA
                      data.frame(month.min, month.max, month.mean,
                                 month.p50, month.p20, month.p10, stringsAsFactors=FALSE)
                      },na.rm=na.rm)
   month.name<- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
   # create variable names for each type of category
   month.stats$vname.min <- paste(month.name[month.stats$Month],"_MIN_DAILY_SW", sep="")
   month.stats$vname.max <- paste(month.name[month.stats$Month],"_MAX_DAILY_SW", sep="")
   month.stats$vname.mean<- paste(month.name[month.stats$Month],"_MEAN_DAILY_SW",sep="")
   month.stats$vname.p50 <- paste(month.name[month.stats$Month],"_P50_DAILY_SW",sep="")
   month.stats$vname.p20 <- paste(month.name[month.stats$Month],"_P20_DAILY_SW",sep="")
   month.stats$vname.p10 <- paste(month.name[month.stats$Month],"_P10_DAILY_SW",sep="")

   month.stats$vname.min <- factor(month.stats$vname.min, levels=month.stats$vname.min, order=TRUE)
   month.stats$vname.max <- factor(month.stats$vname.max, levels=month.stats$vname.max, order=TRUE)
   month.stats$vname.mean<- factor(month.stats$vname.mean,levels=month.stats$vname.mean,order=TRUE)
   month.stats$vname.p50 <- factor(month.stats$vname.p50, levels=month.stats$vname.p50, order=TRUE)
   month.stats$vname.p20 <- factor(month.stats$vname.p20, levels=month.stats$vname.p20, order=TRUE)
   month.stats$vname.p10 <- factor(month.stats$vname.p10, levels=month.stats$vname.p10, order=TRUE)

   Month_MIN_DAILY_SW <- reshape2::acast(month.stats, .~vname.min , value.var='month.min')
   Month_MAX_DAILY_SW <- reshape2::acast(month.stats, .~vname.max , value.var='month.max')
   Month_MEAN_DAILY_SW<- reshape2::acast(month.stats, .~vname.mean, value.var='month.mean')
   Month_P50_DAILY_SW <- reshape2::acast(month.stats, .~vname.p50,  value.var='month.p50')
   Month_P20_DAILY_SW <- reshape2::acast(month.stats, .~vname.p20,  value.var='month.p20')
   Month_P10_DAILY_SW <- reshape2::acast(month.stats, .~vname.p10,  value.var='month.p10')

   # return the CY results
   res <- data.frame(
           CY_MIN_01Day_SW,
           CY_MINDOY_01Day_SW, 
           CY_MIN_03Day_SW,	   
           CY_MINDOY_03Day_SW, 
           CY_MIN_07Day_SW,	   
           CY_MINDOY_07Day_SW, 
           CY_MIN_30Day_SW,	
           CY_MINDOY_30Day_SW,
           CY_MIN_DAILY_SW,   
           CY_MAX_DAILY_SW,	 
           CY_MEAN_DAILY_SW,  
           CY_TOTALQ_DAILY_SW,
           CY_YIELDMM_DAILY_SW,
           CY_CUMQ_DAILY_SW,
           CY_Date_25P_CUMQ_DAILY_SW, 
           CY_Date_50P_CUMQ_DAILY_SW,
           CY_Date_75P_CUMQ_DAILY_SW,
           Season_TOTALQ_DAILY_SW,
           Season_YIELDMM_DAILY_SW,
           Month_MIN_DAILY_SW,
           Month_MAX_DAILY_SW,
           Month_MEAN_DAILY_SW,
           Month_P50_DAILY_SW,
           Month_P20_DAILY_SW,
           Month_P10_DAILY_SW,
           stringsAsFactors=FALSE)
 }, Station.Area=Station.Area, na.rm=na.rm)

#  Compute the statistics on a water year basis  (October -> Sept)

Q.stat.wy <- plyr::ddply(flow[ flow$WYear >= start.year,], "WYear", function(fy, Station.Area, na.rm){
   # process each waters year's flow values (fy)
   WY_MIN_01Day_SW    = min(fy$Q, na.rm=na.rm$na.rm.global)	                # WY Min Daily Q 
   WY_MINDOY_01Day_SW = as.numeric(fy$Date[which.min( fy$Q)]-fy$Date[1]+1)    # Date of WY Min Daily Q
   if(length(WY_MINDOY_01Day_SW)==0) WY_MINDOY_01Day_SW <- NA

   WY_MIN_03Day_SW	   = min(fy$Q.03DAvg, na.rm=na.rm$na.rm.global)                  # Min WY Rolling 3 day avg 
   WY_MINDOY_03Day_SW =  as.numeric(fy$Date[which.min( fy$Q.03DAvg)] - fy$Date[1]+1) # Date of Min WY Rolling 3 day avg
   if(length(WY_MINDOY_03Day_SW)==0) WY_MINDOY_03Day_SW <- NA

   WY_MIN_07Day_SW	   = min(fy$Q.07DAvg, na.rm=na.rm$na.rm.global)                   # Min WY Rolling 7 day avg 
   WY_MINDOY_07Day_SW = as.numeric(fy$Date[which.min( fy$Q.07DAvg)] - fy$Date[1]+1)   # Date of Min WY Rolling 7 day avg
   if(length(WY_MINDOY_07Day_SW)==0) WY_MINDOY_07Day_SW <- NA

   WY_MIN_30Day_SW	 = min(fy$Q.30DAvg, na.rm=na.rm$na.rm.global)                     # Min WY Rolling 30 day avg 
   WY_MINDOY_30Day_SW= as.numeric(fy$Date[which.min( fy$Q.30DAvg)] - fy$Date[1]+1)    # Date of Min WY Rolling 30 day avg
   if(length(WY_MINDOY_30Day_SW)==0) WY_MINDOY_30Day_SW <- NA

   WY_MIN_DAILY_SW   = min (fy$Q, na.rm=na.rm$na.rm.global)	    # WY Min Daily Q 	WY Min Daily Q
   WY_MAX_DAILY_SW	 = max (fy$Q, na.rm=na.rm$na.rm.global)     # WY Max Daily Q
   WY_MEAN_DAILY_SW  = mean(fy$Q, na.rm=na.rm$na.rm.global)     # WY Mean Discharge (Based on Daily avgs)
   # WY_TOTALQ_DAILY_SW	Oct through Sept sum of daily avg (cms) *60*60*24
   # WY_YIELDMM_DAILY_SW	(Oct_to_Sep_TotalQ/(area in km2*1000000))*1000

   WY_TOTALQ_DAILY_SW <- mean(fy$Q, na.rm=na.rm$na.rm.global)*length(fy$Q)*60*60*24 
   WY_YIELDMM_DAILY_SW<- WY_TOTALQ_DAILY_SW /Station.Area/1000

   WY_CUMQ_DAILY_SW  = WY_TOTALQ_DAILY_SW/(60*60*24)      # Yearly sum of daily avg (cms) # deal with missing values

   # Get the cumulative Q values 
   # Notice that if missing values are removed, the individual values are replaced by zero
   fy$Q2 <- fy$Q
   if(na.rm$na.rm.globa) fy$Q2[ is.na(fy$Q2)] <- 0
   fy$CumQ <- cumsum(fy$Q2)    
   # what is the first date where 25, 50 and 75% of totalQ (see above) are found
   WY_Date_25P_CUMQ_DAILY_SW <- fy$Date[ match(TRUE, (fy$CumQ > 0.25 *WY_CUMQ_DAILY_SW))]
   WY_Date_50P_CUMQ_DAILY_SW <- fy$Date[ match(TRUE, (fy$CumQ > 0.50 *WY_CUMQ_DAILY_SW))]
   WY_Date_75P_CUMQ_DAILY_SW <- fy$Date[ match(TRUE, (fy$CumQ > 0.75 *WY_CUMQ_DAILY_SW))]
   WY_Date_25P_CUMQ_DAILY_SW <- as.numeric(WY_Date_25P_CUMQ_DAILY_SW - fy$Date[1] +1)
   WY_Date_50P_CUMQ_DAILY_SW <- as.numeric(WY_Date_50P_CUMQ_DAILY_SW - fy$Date[1] +1)
   WY_Date_75P_CUMQ_DAILY_SW <- as.numeric(WY_Date_75P_CUMQ_DAILY_SW - fy$Date[1] +1)
   #browser()
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
           WY_MIN_01Day_SW,
           WY_MINDOY_01Day_SW, 
           WY_MIN_03Day_SW,	   
           WY_MINDOY_03Day_SW, 
           WY_MIN_07Day_SW,	   
           WY_MINDOY_07Day_SW, 
           WY_MIN_30Day_SW,	
           WY_MINDOY_30Day_SW,
           WY_MIN_DAILY_SW,   
           WY_MAX_DAILY_SW,	 
           WY_MEAN_DAILY_SW,  
           WY_TOTALQ_DAILY_SW,
           WY_YIELDMM_DAILY_SW,
           WY_CUMQ_DAILY_SW,
           WY_Date_25P_CUMQ_DAILY_SW, 
           WY_Date_50P_CUMQ_DAILY_SW,
           WY_Date_75P_CUMQ_DAILY_SW,
           Season_TOTALQ_DAILY_SW,
           Season_YIELDMM_DAILY_SW,
           stringsAsFactors=FALSE)
}, Station.Area=Station.Area,  na.rm=na.rm)


# compute the number of days in a year outside of the 25th or 75th percentile for each day.
flow$jdate <- as.numeric(format(flow$Date, "%j")) # the julian date
daily.quant <- plyr::ddply(flow, "jdate", plyr::summarize,
                           p25=quantile(Q, prob=0.25, na.rm=TRUE),
                           p75=quantile(Q, prob=0.75, na.rm=TRUE))
flow <- merge(flow, daily.quant, by='jdate') # merge back with the original data
outside.quant <- plyr::ddply(flow, "Year", summarize,
                             CY_N_BELOW_25=sum(Q < p25, na.rm=TRUE),
                             CY_N_ABOVE_75=sum(Q > p75, na.rm=TRUE),
                             CY_N_OUTSIDE_25_75 = CY_N_BELOW_25 + CY_N_ABOVE_75)

# merge the annual and water year statistics
Q.stat <- merge(Q.stat.cy, Q.stat.wy, by.x='Year', by.y="WYear", all.x=TRUE)
Q.stat <- merge(Q.stat, outside.quant, by="Year", all.x=TRUE)

file.summary.csv <- NA
if(write.flow.summary.csv){
   # write out the flow summary
   file.summary.csv <- file.path(report.dir, paste(Station.Code,"-period-record-summary.csv", sep=""))
   temp <- flow.sum
   temp[,5:ncol(flow.sum)] <- round(temp[,5:ncol(flow.sum)], csv.nddigits)
   write.csv(temp,file=file.summary.csv, row.names=FALSE)
}

# See if you want to write out the summary tables?
file.stat.csv <- NA
if(write.stat.csv){
   if(debug)browser()
   # Write out the summary table for comparison to excel spreadsheet
   file.stat.csv <- file.path(report.dir, paste(Station.Code,"-annual-summary-stat.csv", sep=""))
   temp <- Q.stat
   temp <- round(temp, csv.nddigits)
   write.csv(temp,file=file.stat.csv, row.names=FALSE)
}

# Write out the annual summary table in transposed format?
file.stat.trans.csv<- NA
Year <- Q.stat[,"Year"]
Q.stat.trans <- t(Q.stat[, !grepl('^Year', names(Q.stat))])
colnames(Q.stat.trans) <- paste("Y",Year,sep="")
if(write.stat.trans.csv){
  file.stat.trans.csv <-file.path(report.dir,paste(Station.Code,"-annual-summary-stat-trans.csv",sep=""))
  temp <- Q.stat.trans
  temp <- round(temp, csv.nddigits)
  write.csv(temp, file=file.stat.trans.csv, row.names=TRUE)
}

# Write out the low flow summary 
file.lowflow.csv <- NA
if(write.lowflow.csv){
   file.lowflow.csv <- file.path(report.dir,paste(Station.Code,"-lowflow-summary.csv",sep=""))
   select <-          grepl("CY_MIN", names(Q.stat))
   select <- select | grepl("WY_MIN", names(Q.stat))
   temp <- Q.stat[,c("Year", names(Q.stat)[select])]
   temp <- round(temp, csv.nddigits)
   # now convert columns that are doy to an actual date
   for(i in 1:ncol(temp)){
      if(grepl("CY_MINDOY", names(temp)[i])){
        temp[,i] <- as.Date(paste(temp$Year,'-01-01',sep=""))+temp[,i]-1
      }
   }
   for(i in 1:ncol(temp)){
     if(grepl("WY_MINDOY", names(temp)[i])){
       temp[,i] <- as.Date(paste(temp$Year-1,'-10-01',sep=""))+temp[,i]-1
     }
   }
   write.csv(temp, file=file.lowflow.csv, row.names=FALSE)
}
 
# make a plot of the cumulative departures
  plot_cumdepart <- function(Q.stat, variable){
      # find the grand mean over all of the years ignoring all missing values
      grand.mean <- mean( Q.stat[, variable], na.rm=TRUE)
      plotdata <- Q.stat[,c("Year",variable)]
      plotdata <- plotdata[complete.cases(plotdata),]   # remove all missing values
      plotdata$diff.from.mean <- plotdata[, variable] - grand.mean
      plotdata$cum.diff.from.mean <- cumsum(plotdata$diff.from.mean)
      plot1 <- ggplot2::ggplot(data=plotdata, aes(x=Year, y=cum.diff.from.mean))+
         ggtitle(paste(Station.Code," - cumulative departure curve for ",variable,sep=""))+
         geom_hline(yintercept=0)+
         geom_segment(aes(x=Year, y=0, xend=Year, yend=diff.from.mean), size=2)+
         geom_line()+
         ylab("Departure from the mean")
      plot1
  }

file.cumdepart.pdf <- NA
if(plot.cumdepart){
   # cumulative departure plots
   file.cumdepart.pdf <- file.path(report.dir, paste(Station.Code,"-cumulative departure.pdf",sep=""))
   var.list <- c("CY_MEAN_DAILY_SW","WY_MEAN_DAILY_SW", "CY_YIELDMM_DAILY_SW", "WY_YIELDMM_DAILY_SW",
                  "ONDJFM_YIELDMM_DAILY_SW","AMJJAS_YIELDMM_DAILY_SW"   )
   pdf(file=file.cumdepart.pdf, h=8, w=11)
   l_ply(var.list, function(x, Q.stat){
      plot1 <- plot_cumdepart(Q.stat, x)
      plot(plot1)
   },Q.stat=Q.stat)
   dev.off()
}
  
# Make a plot of all of the statistics over time and save to a pdf file
file.stat.trend.pdf <- NA
if(plot.stat.trend){
   file.stat.trend.pdf <- file.path(report.dir, paste(Station.Code,"-annual-trend.pdf",sep=""))
   pdf(file.stat.trend.pdf, h=8, w=11)
   
   plot_trend <- function(plotdata, select){
     x <- plotdata[select,]
     myplot <- ggplot(data=x, aes(x=Year, y=Value, group=Statistic, color=Statistic, linetype=Statistic))+
       ggtitle(paste(Station.Code, " - Trend for ", x$statgroup[1]))+
       geom_point()+
       geom_line()+xlab("Year")+ylab(x$Ylabel[1])
     if(x$transform[1] == 'log'){
       myplot <- myplot + ylab(paste("log(",x$Ylabel[1],")",sep=""))+scale_y_continuous(trans='log10')
     }
     plot(myplot)
   }

   plotdata <- reshape2::melt(Q.stat, id.var="Year", variable.name="Statistic", value.name="Value")
   plotdata$statgroup <- NA
   plotdata$transform <- ""
   plotdata$Ylabel    <- 'Value'

   set.annual.min  <- grepl("^CY_MIN_", plotdata$Statistic) 
   plotdata$statgroup [set.annual.min] <- 'CY Annual Minimums'
   plotdata$Ylabel    [set.annual.min] <- 'Flow (cms)'
   plot_trend(plotdata, set.annual.min)

   set.annual.mindoy  <- grepl("^CY_MINDOY", plotdata$Statistic) 
   plotdata$statgroup [set.annual.mindoy] <- 'Annual Day of CY for Minimums'
   plotdata$Ylabel    [set.annual.mindoy] <- 'Day into year'
   plot_trend(plotdata, set.annual.mindoy)

   # make a plot for each 3 month period. Include the daily min in period with similar range.
   annual.avg.min <- mean( plotdata$Value[ plotdata$Statistic=='CY_MIN_DAILY_SW'], na.rm=TRUE)
   set.month.min      <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_MIN",sep=""))
   if(range(plotdata$Value[set.month.min],na.rm=TRUE)[1] < annual.avg.min  &
      range(plotdata$Value[set.month.min],na.rm=TRUE)[2] > annual.avg.min){
      set.month.min = set.month.min | grepl("^CY_MIN_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.min] <- 'Monthly Minimums - JFM'
   plotdata$transform [set.month.min] <- "log"
   plotdata$Ylabel    [set.month.min] <- "Flow (cms)"
   plot_trend(plotdata, set.month.min)

   set.month.min      <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_MIN",sep=""))
   if(range(plotdata$Value[set.month.min],na.rm=TRUE)[1] < annual.avg.min  &
      range(plotdata$Value[set.month.min],na.rm=TRUE)[2] > annual.avg.min){
     set.month.min = set.month.min | grepl("^CY_MIN_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.min] <- 'Monthly Minimums - AMJ'
   plotdata$transform [set.month.min] <- "log"
   plotdata$Ylabel    [set.month.min] <- "Flow (cms)"
   plot_trend(plotdata, set.month.min)

   set.month.min      <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_MIN",sep=""))
   if(range(plotdata$Value[set.month.min],na.rm=TRUE)[1] < annual.avg.min  &
      range(plotdata$Value[set.month.min],na.rm=TRUE)[2] > annual.avg.min){
     set.month.min = set.month.min | grepl("^CY_MIN_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.min] <- 'Monthly Minimums - JAS'
   plotdata$transform [set.month.min] <- "log"
   plotdata$Ylabel    [set.month.min] <- "Flow (cms)"
   plot_trend(plotdata, set.month.min)
   
   set.month.min      <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:20],"_MIN",sep=""))
   if(range(plotdata$Value[set.month.min],na.rm=TRUE)[1] < annual.avg.min  &
      range(plotdata$Value[set.month.min],na.rm=TRUE)[2] > annual.avg.min){
     set.month.min = set.month.min | grepl("^CY_MIN_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.min] <- 'Monthly Minimums - OND'
   plotdata$transform [set.month.min] <- "log"
   plotdata$Ylabel    [set.month.min] <- "Flow (cms)"
   plot_trend(plotdata, set.month.min)
   
   
   
   # make a plot for each 3 month period. Include the daily max in period with similar range.
   annual.avg.max <- mean( plotdata$Value[ plotdata$Statistic=='CY_MAX_DAILY_SW'], na.rm=TRUE)
   set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_MAX",sep=""))
   if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
      range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.max = set.month.max | grepl("^CY_MAX_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.max] <- 'Monthly/Annual Maximums  -  JFM'
   plotdata$transform [set.month.max] <- "log"
   plotdata$Ylabel    [set.month.max] <- "Flow (cms)"
   plot_trend(plotdata, set.month.max)
   
   set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_MAX",sep=""))
   if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
      range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
     set.month.max = set.month.max | grepl("^CY_MAX_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.max] <- 'Monthly/Annual Maximums  -  AMJ'
   plotdata$transform [set.month.max] <- "log"
   plotdata$Ylabel    [set.month.max] <- "Flow (cms)"
   plot_trend(plotdata, set.month.max)
   
   set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_MAX",sep=""))
   if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
      range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
     set.month.max = set.month.max | grepl("^CY_MAX_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.max] <- 'Monthly/Annual Maximums  -  JAS'
   plotdata$transform [set.month.max] <- "log"
   plotdata$Ylabel    [set.month.max] <- "Flow (cms)"
   plot_trend(plotdata, set.month.max)
   
   set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:12],"_MAX",sep=""))
   if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
      range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
     set.month.max = set.month.max | grepl("^CY_MAX_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.max] <- 'Monthly/Annual Maximums  -  OND'
   plotdata$transform [set.month.max] <- "log"
   plotdata$Ylabel    [set.month.max] <- "Flow (cms)"
   plot_trend(plotdata, set.month.max)
   
   
   # make a plot for each 3 month period. Include the daily max in period with similar range.
   annual.avg.mean <- mean( plotdata$Value[ plotdata$Statistic=='CY_MEAN_DAILY_SW'], na.rm=TRUE)
   set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb[1:3],"_MEAN",sep=""))
   if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
      range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
     set.month.mean = set.month.mean | grepl("^CY_MEAN_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.mean] <- 'Monthly/Annual Means  -  JFM'
   plotdata$transform [set.month.mean] <- "log"
   plotdata$Ylabel    [set.month.mean] <- "Flow (cms)"
   plot_trend(plotdata, set.month.mean)
   
   set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb[4:6],"_MEAN",sep=""))
   if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
      range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
     set.month.mean = set.month.mean | grepl("^CY_MEAN_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.mean] <- 'Monthly/Annual Means  -  AMJ'
   plotdata$transform [set.month.mean] <- "log"
   plotdata$Ylabel    [set.month.mean] <- "Flow (cms)"
   plot_trend(plotdata, set.month.mean)
   
   set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb[7:9],"_MEAN",sep=""))
   if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
      range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
     set.month.mean = set.month.mean | grepl("^CY_MEAN_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.mean] <- 'Monthly/Annual Means  -  JAS'
   plotdata$transform [set.month.mean] <- "log"
   plotdata$Ylabel    [set.month.mean] <- "Flow (cms)"
   plot_trend(plotdata, set.month.mean)
   
   set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb[10:12],"_MEAN",sep=""))
   if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
      range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
     set.month.mean = set.month.mean | grepl("^CY_MEAN_DAILY_SW", plotdata$Statistic)   }
   plotdata$statgroup [set.month.mean] <- 'Monthly/Annual Means  -  OND'
   plotdata$transform [set.month.mean] <- "log"
   plotdata$Ylabel    [set.month.mean] <- "Flow (cms)"
   plot_trend(plotdata, set.month.mean)
   

   set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_P50",sep=""))
   plotdata$statgroup [set.month.p50] <- 'Monthly P50  -  JFM'
   plotdata$transform [set.month.p50] <- "log"
   plotdata$Ylabel    [set.month.p50] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p50)
   
   set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_P50",sep=""))
   plotdata$statgroup [set.month.p50] <- 'Monthly P50  -  AMJ'
   plotdata$transform [set.month.p50] <- "log"
   plotdata$Ylabel    [set.month.p50] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p50)
   
   set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_P50",sep=""))
   plotdata$statgroup [set.month.p50] <- 'Monthly P50  -  JAS'
   plotdata$transform [set.month.p50] <- "log"
   plotdata$Ylabel    [set.month.p50] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p50)
   
   set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:12],"_P50",sep=""))
   plotdata$statgroup [set.month.p50] <- 'Monthly P50  - OND'
   plotdata$transform [set.month.p50] <- "log"
   plotdata$Ylabel    [set.month.p50] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p50)
   
   
   set.month.p20     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_P20",sep=""))
   plotdata$statgroup [set.month.p20] <- 'Monthly P20  -  JFM'
   plotdata$transform [set.month.p20] <- "log"
   plotdata$Ylabel    [set.month.p20] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p20)
   
   set.month.p20     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_P20",sep=""))
   plotdata$statgroup [set.month.p20] <- 'Monthly P20  -  AMJ'
   plotdata$transform [set.month.p20] <- "log"
   plotdata$Ylabel    [set.month.p20] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p20)
   
   set.month.p20     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_P20",sep=""))
   plotdata$statgroup [set.month.p20] <- 'Monthly P20  -  JAS'
   plotdata$transform [set.month.p20] <- "log"
   plotdata$Ylabel    [set.month.p20] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p20)
   
   set.month.p20     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:12],"_P20",sep=""))
   plotdata$statgroup [set.month.p20] <- 'Monthly P20  -  OND'
   plotdata$transform [set.month.p20] <- "log"
   plotdata$Ylabel    [set.month.p20] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p20)
   
   
   set.month.p10     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_P10",sep=""))
   plotdata$statgroup [set.month.p10] <- 'Monthly P10  -  JFM'
   plotdata$transform [set.month.p10] <- "log"
   plotdata$Ylabel    [set.month.p10] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p10)
   
   set.month.p10     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_P10",sep=""))
   plotdata$statgroup [set.month.p10] <- 'Monthly P10  -  AMJ'
   plotdata$transform [set.month.p10] <- "log"
   plotdata$Ylabel    [set.month.p10] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p10)
   
   set.month.p10     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_P10",sep=""))
   plotdata$statgroup [set.month.p10] <- 'Monthly P10  -  JAS'
   plotdata$transform [set.month.p10] <- "log"
   plotdata$Ylabel    [set.month.p10] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p10)
   
   set.month.p10     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:12],"_P10",sep=""))
   plotdata$statgroup [set.month.p10] <- 'Monthly P10  -  OND'
   plotdata$transform [set.month.p10] <- "log"
   plotdata$Ylabel    [set.month.p10] <- "Flow (cms)"
   plot_trend(plotdata, set.month.p10)
   
   
   set.yieldmm     <- grepl("YIELDMM", plotdata$Statistic) & 
                      (substr(plotdata$Statistic,1,3) %in% c("AMJ","JAS"))
   plotdata$statgroup [set.yieldmm] <- 'CY Yield (MM)   -  Spring/Summer'
   plotdata$transform [set.yieldmm] <- "log"
   plotdata$Ylabel    [set.yieldmm] <- 'Yield (MM)'
   plot_trend(plotdata, set.yieldmm)
   
   set.yieldmm     <- grepl("YIELDMM", plotdata$Statistic) & 
                      (substr(plotdata$Statistic,1,3) %in% c("JFM","OND"))
   plotdata$statgroup [set.yieldmm] <- 'CY Yield (MM)   -  Fall/Winter'
   plotdata$transform [set.yieldmm] <- "log"
   plotdata$Ylabel    [set.yieldmm] <- 'Yield (MM)'
   plot_trend(plotdata, set.yieldmm)
   
   set.yieldmm     <- grepl("YIELDMM", plotdata$Statistic) & 
                     (substr(plotdata$Statistic,1,2) %in% c("CY","WY"))
   plotdata$statgroup [set.yieldmm] <- 'CY Yield (MM)   -  Calendar or Water Year'
   plotdata$transform [set.yieldmm] <- "log"
   plotdata$Ylabel    [set.yieldmm] <- 'Yield (MM)'
   plot_trend(plotdata, set.yieldmm)
   

   set.totalq     <- grepl("TOTALQ", plotdata$Statistic) & 
                    (substr(plotdata$Statistic,1,3) %in% c("AMJ","JAS"))
   plotdata$statgroup [set.totalq] <- 'Total Q  -  Spring/Summer'
   plotdata$transform [set.totalq] <- "log"
   plotdata$Ylabel    [set.totalq] <- 'Total Q (cms)'
   plot_trend(plotdata, set.totalq)
   set.totalq     <- grepl("TOTALQ", plotdata$Statistic) & 
                    (substr(plotdata$Statistic,1,3) %in% c("JFM","OND"))
   plotdata$statgroup [set.totalq] <- 'Total Q   - Fall/Winter'
   plotdata$transform [set.totalq] <- "log"
   plotdata$Ylabel    [set.totalq] <- 'Total Q (cms)'
   plot_trend(plotdata, set.totalq)
   set.totalq     <- grepl("TOTALQ", plotdata$Statistic) & 
                    (substr(plotdata$Statistic,1,2) %in% c("CY","WY"))
   plotdata$statgroup [set.totalq] <- 'Total Q   -  Calendar and Water Year'
   plotdata$transform [set.totalq] <- "log"
   plotdata$Ylabel    [set.totalq] <- 'Total Q (cms)'
   plot_trend(plotdata, set.totalq)
   
   set.annual.date     <- grepl("CY_Date", plotdata$Statistic)
   plotdata$statgroup [set.annual.date] <- 'Annual Day of CY for Total Discharge Milestones'
   plotdata$Ylabel    [set.annual.date] <- "Day into the year"
   plot_trend(plotdata, set.annual.date)

   # more plots on the water year basis    
   set.annual.min  <- grepl("^WY_MIN_", plotdata$Statistic) 
   plotdata$statgroup [set.annual.min] <- 'WY Annual Minimums'
   plotdata$Ylabel    [set.annual.min] <- 'Flow (cms)'
   plot_trend(plotdata, set.annual.min)

   set.annual.mindoy  <- grepl("^WY_MINDOY", plotdata$Statistic) 
   plotdata$statgroup [set.annual.mindoy] <- 'Annual Day of WY for Minimums'
   plotdata$Ylabel    [set.annual.mindoy] <- 'Day into year'
   plot_trend(plotdata, set.annual.mindoy)

   set.min.max.mean <- grepl("WY_MIN_", plotdata$Statistic) |
                       grepl("WY_MAX_", plotdata$Statistic) |
                       grepl("WY_MEAN_",plotdata$Statistic)
   plotdata$statgroup [set.min.max.mean] <- 'WY Minimum, Maximum, Mean'
   plotdata$transform [set.min.max.mean] <- "log"
   plotdata$Ylabel    [set.min.max.mean] <- "Flow (cms)"
   plot_trend(plotdata, set.min.max.mean)

   set.annual.date     <- grepl("WY_Date", plotdata$Statistic)
   plotdata$statgroup [set.annual.date] <- 'Annual Day of WY for Total Discharge Milestones'
   plotdata$Ylabel    [set.annual.date] <- "Day into the year"
   plot_trend(plotdata, set.annual.date)
   
   set.outside         <- grepl("CY_N_", plotdata$Statistic)
   plotdata$statgroup [set.outside] <- 'Days Outside 25/75 Daily Percentiles'
   plotdata$Ylabel    [set.outside] <- 'Days'
   plot_trend(plotdata, set.outside)
   
   set.misc <- is.na(plotdata$statgroup)
   plotdata$statgroup[ set.misc] <- "Misc Statistics"
   plotdata$Ylabel   [ set.misc] <- 'Cumulative Flow (cms)'
   plot_trend(plotdata, set.misc)
                 
   dev.off()
}
return(list( Q.flow.summary=flow.sum,
             Q.stat.annual=Q.stat,
             Q.stat.annual.trans=Q.stat.trans,
             dates.missing.flows=dates.missing.flows,
             file.stat.csv=file.stat.csv,
             file.stat.trans.csv=file.stat.trans.csv,
             file.stat.trend.pdf=file.stat.trend.pdf,
             file.cumdepart.pdf=file.cumdepart.pdf,
             file.summary.csv=file.summary.csv,
             file.lowflow.csv=file.lowflow.csv,
             na.rm = na.rm,
             Version=Version,
             Date=Sys.time()))
} # end of function

