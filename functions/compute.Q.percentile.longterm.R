# Compute the longterm percentile statistics  between start and end years.
#
#  2017-01-30 CJS First Edition

compute.Q.percentile.longterm <- function(
                          Station.code='XXXXX', 
                          Station.Area=NA, 
                          flow, 
                          start.year=9999, 
                          end.year=0,
                          write.stat.csv=FALSE,        # write out statistics 
                          write.stat.trans.csv=FALSE,  # write out statistics in transposed format
                          report.dir=".",
                          na.rm=list(na.rm.global=TRUE)
                          ){
#  Input
#    Station.Code - character string indentifying the station with the flow
#    Station.Area - area of water basin behind the station needed for some statistics
#    flow - data frame with variables
#              Date - date (as R data variable type, usually constructed as using as.Date()
#              Q    - flow values
#           All other variables in the data frame will be ignored.
#           Data does NOT have to be sorted by Date order.
#
#           All missing values are automatically excluded
#
#    start.year, end year - starting and ending year for statistics e.g. start.year=1960, end.year=2013
#    write.stat.csv   - write out statistics to csv file - file name is returned
#    write.stat.trans.csv - write out transposed statistics to csv file - file name is returned
#
#
#  Output: List with the following objects
#    Q.stat.longterm - longterm statistics 
#    Q.stat.longterm.trans - longterm statistics in transposed format
#    file.stat   - file name of csv file created
#    file.stat.trans - file name of transposed statistics.
#
#############################################################
#  Some basic error checking on the input parameters
#
   Version <- '2017-02-01'
   library(plyr)        # split-apply-combine 
   library(reshape2)    # reorganize data (melting and casting)

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
   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}

   if( !is.list(na.rm))              {stop("na.rm is not a list") }
   if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
   my.na.rm <- list(na.rm.global=FALSE)
   if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
   my.na.rm[names(na.rm)]<- na.rm
   na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
#  create the year (annual ) and month variables
   flow$Year  <- as.numeric(format(flow$Date, "%Y"))
   flow$Month <- as.numeric(format(flow$Date, '%m'))

   Q.per.longterm <- plyr::ddply(flow[flow$Year >= start.year & flow$Year <=end.year,], "Month", function(fy){
        per.list <- c(1,2,5,seq(10,95,5),99)  # these the percentiles from the TOP
        per <- matrix(quantile(fy$Q, prob=1-per.list/100,na.rm=na.rm$na.rm.global), nrow=1)
        colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
        data.frame(per, stringsAsFactors=FALSE)
   })
   Q.per.longterm$Month <- month.abb[Q.per.longterm$Month]

#  Write out the summary table for comparison to excel spreadsheet
   file.stat.csv <- NA
   if(write.stat.csv){
      file.stat.csv <- file.path(report.dir,paste(Station.Code,"-longterm-percentile-stat.csv", sep=""))
      write.csv(Q.per.longterm, file=file.stat.csv, row.names=FALSE)
   }
   
#  Write out thesummary table in transposed format
   Month <- Q.per.longterm[,"Month"]
   Q.per.longterm.trans <- t(Q.per.longterm[, !grepl('^Month', names(Q.per.longterm))])
   colnames(Q.per.longterm.trans) <-  toupper(month.abb[1:12])
   file.stat.trans.csv<- NA
   if(write.stat.trans.csv){
     file.stat.trans.csv <-file.path(report.dir, paste(Station.Code,"-longterm-percentile-stat-trans.csv", sep=""))
     write.csv(Q.per.longterm.trans, file=file.stat.trans.csv, row.names=TRUE)
   }
   
   return(list(Q.percentile.stat=Q.per.longterm,
               Q.percentile.stat.trans=Q.per.longterm.trans,
               file.stat.csv=file.stat.csv,
               file.stat.trans.csv=file.stat.trans.csv,
               na.rm=na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function
