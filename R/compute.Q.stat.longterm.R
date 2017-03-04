#' @title Compute long term annual (calendar year) summary statistics.
#'
#' @description Computes long term summary statistics on \code{flow$Q} variable
#'    between \code{start.year} and \code{end.year} inclusive.
#'    It (optionally) saves the resutls in *.csv and *.pdf files.
#'
#' @template Station.Code
#' @template Station.Area
#' @template flow
#' @template start.year
#' @param write.stat.csv Should a file be created with the computed percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-summary-stat.csv'))}.
#' @param write.stat.trans.csv Should a file be created with the transposed of the percentile report?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-summary-stat-trabs.csv'))}.
#' @template report.dir
#' @template csv.nddigits
#' @template na.rm
#'
#' @return A list with the following elements:
#'   \item{Q.stat.longterm}{Data frame with the long term statistics of \code{flow$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.stat.longterm.trans}{Data frame with the long term statistics of \code{flow$Q} transposed.}
#'   \item{file.stat.csv}{Object with file name of *.csv file with long term summary statistics.}
#'    \item{file.stat.trans.csv}{Object with file name of *.csv file with transposed long term summary statistics.}
#'    \item{na.rm}{Missing value flags.}
#'    \item{Version}{Version of this function.}
#'    \item{Date}{Date function was run.}
#' @examples
#' \dontrun{
#' stat.longterm <- compute.Q.stat.longterm(
#'                          Station.Code  ='ABCDE',
#'                          Station.Area  =2458,
#'                          flow          =flow,
#'                          start.year    =1960,
#'                          end.year      =2015)
#' }
#'
#'@export
#'@import ggplot2
#'@import scales
#'@import utils
#'


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

compute.Q.stat.longterm <- function(
                          Station.Code='XXXXX',
                          Station.Area=NA,
                          flow,
                          start.year=9999,
                          end.year=0,
                          write.stat.csv=TRUE,         # write out statistics
                          write.stat.trans.csv=TRUE,   # write out statistics in transposed format
                          report.dir='.',
                          csv.nddigits=3,               # decimal digit for csv files.
                          na.rm=list(na.rm.global=TRUE)){
#  Input - see the man-roxygen directory for details.

#  Output: List with elements as desribed above
#
#############################################################
#  Some basic error checking on the input parameters
#
   Version <- packageVersion("BCWaterDischargeAnalysis")

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

   if(!is.numeric(csv.nddigits)){ stop("csv.nddigits must be numeric")}
   csv.nddigits <- round(csv.nddigits)[1]

   if( !is.list(na.rm))              {stop("na.rm is not a list") }
   if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
   my.na.rm <- list(na.rm.global=FALSE)
   if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
   my.na.rm[names(na.rm)]<- na.rm
   na.rm <- my.na.rm  # set the na.rm for the rest of the function.

#  create the year (annual ) and month variables
   flow$Year  <- as.numeric(format(flow$Date, "%Y"))
   flow$Month <- as.numeric(format(flow$Date, '%m'))

   Q.month.longterm <- plyr::ddply(flow[flow$Year >= start.year & flow$Year<=end.year,], "Month", function(x,na.rm){    # all missing values always excluded
      mean  = mean  (x$Q, na.rm=na.rm$na.rm.global)
      median= stats::median(x$Q, na.rm=na.rm$na.rm.global)
      max   = max   (x$Q, na.rm=na.rm$na.rm.global)
      min   = min   (x$Q, na.rm=na.rm$na.rm.global)
      data.frame(mean=mean, median=median, max=max, min=min)
   }, na.rm=na.rm)
   Q.month.longterm$Month <- as.character(Q.month.longterm$Month)

   Q.all.longterm <- plyr::summarize( flow[flow$Year >= start.year & flow$Year<=end.year,],
                          mean  = mean  (Q, na.rm=na.rm$na.rm.global),
                          median= stats::median(Q, na.rm=na.rm$na.rm.global),
                          max   = max   (Q, na.rm=na.rm$na.rm.global),
                          min   = min   (Q, na.rm=na.rm$na.rm.global))
   Q.all.longterm$Month <- "Longterm"

   Q.longterm <- rbind(Q.month.longterm, Q.all.longterm)

#  Write out the summary table for comparison to excel spreadsheet
   file.stat.csv <- NA
   if(write.stat.csv){
      file.stat.csv <-file.path(report.dir, paste(Station.Code,"-longterm-summary-stat.csv", sep=""))
      temp <- Q.longterm
      temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], csv.nddigits)  # round the output
      utils::write.csv(temp, file=file.stat.csv, row.names=FALSE)
   }

#  Write out thesummary table in transposed format
   Month <- Q.longterm[,"Month"]
   Q.longterm.trans <- t(Q.longterm[, !grepl('^Month', names(Q.longterm))])
   colnames(Q.longterm.trans) <- c( toupper(month.abb[1:12]),"Longterm")
   file.stat.trans.csv <- NA
   if(write.stat.trans.csv){
     file.stat.trans.csv <-file.path(report.dir,paste(Station.Code,"-longterm-summary-stat-trans.csv", sep=""))
     temp <- Q.longterm.trans
     temp <- round(temp, csv.nddigits) # round the output
     utils::write.csv(temp, file=file.stat.trans.csv, row.names=TRUE)
   }
   return(list(Q.stat.longterm=Q.longterm,
               Q.stat.longterm.trans=Q.longterm.trans,
               file.stat.csv=file.stat.csv,
               file.stat.trans.csv=file.stat.trans.csv,
               na.rm=na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function
