#' @title Compute long term annual (calendar year) summary statistics.
#'
#' @description Computes long term summary statistics on \code{flow$Q} variable
#'    between \code{start.year} and \code{end.year} inclusive.
#'    It (optionally) saves the resutls in *.csv and *.pdf files.
#'
#' @template Station.Code
#' @template flow
#' @template start.year
#' @param write.CY.stat.csv Should a file be created with the computed percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-summary-stat.csv'))}.
#' @param write.CY.stat.trans.csv Should a file be created with the transposed of the percentile report?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-summary-stat-trans.csv'))}.
#' @param write.WY.stat.csv Should a file be created with the computed percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-WY-summary-stat.csv'))}.
#' @param write.WY.stat.trans.csv Should a file be created with the transposed of the percentile report?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-summary-stat-trabs.csv'))}.
#' @template report.dir
#' @template csv.nddigits
#' @template na.rm
#'
#' @return A list with the following elements:
#'   \item{Q.CY.stat.longterm}{Data frame with the long term statistics of \code{flow$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.CY.stat.longterm.trans}{Data frame with the long term statistics of \code{flow$Q} transposed.}
#'   \item{file.CY.stat.csv}{Object with file name of *.csv file with long term summary statistics.}
#'    \item{file.CY.stat.trans.csv}{Object with file name of *.csv file with transposed long term summary statistics.}
#'    \item{Q.WY.stat.longterm}{Data frame with the long term statistics of \code{flow$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.WY.stat.longterm.trans}{Data frame with the long term statistics of \code{flow$Q} transposed.}
#'   \item{file.WY.stat.csv}{Object with file name of *.csv file with long term summary statistics.}
#'    \item{file.WY.stat.trans.csv}{Object with file name of *.csv file with transposed long term summary statistics.}
#'    \item{na.rm}{Missing value flags.}
#'    \item{Version}{Version of this function.}
#'    \item{Date}{Date function was run.}
#' @examples
#' \dontrun{
#' stat.longterm <- compute.Q.stat.longterm(
#'                          Station.Code  ='ABCDE',
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
                          flow,
                          start.year=9999,
                          end.year=0,
                          write.CY.stat.csv=TRUE,         # write out statistics
                          write.CY.stat.trans.csv=TRUE,   # write out statistics in transposed format
                          write.WY.stat.csv=TRUE,
                          write.WY.stat.trans.csv=TRUE,
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
   if( !is.logical(write.CY.stat.csv))  {stop("write.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.CY.stat.trans.csv)){stop("write.stat.trans.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.WY.stat.csv))  {stop("write.WY.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.WY.stat.trans.csv)){stop("write.WY.stat.trans.csv must be logical (TRUE/FALSE")}
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
   flow$Month <- as.factor(format(flow$Date, '%b'))
   flow$MonthNum <- as.numeric(format(flow$Date, '%m'))
   flow$WaterYear <- as.numeric(ifelse(flow$MonthNum>=10,flow$Year+1,flow$Year))

   #Calendar YEAR

   Q.month.longterm.CY <- plyr::ddply(flow[flow$Year >= start.year & flow$Year<=end.year,], "Month", function(x,na.rm){    # all missing values always excluded
      Mean  = mean  (x$Q, na.rm=na.rm$na.rm.global)
      Median= stats::median(x$Q, na.rm=na.rm$na.rm.global)
      Maximum   = max   (x$Q, na.rm=na.rm$na.rm.global)
      Minimum   = min   (x$Q, na.rm=na.rm$na.rm.global)
      data.frame(Mean=Mean, Median=Median, Maximum=Maximum, Minimum=Minimum)
   }, na.rm=na.rm)
   Q.month.longterm.CY$Month <- as.factor(Q.month.longterm.CY$Month)

   Q.all.longterm.CY <- plyr::summarize( flow[flow$Year >= start.year & flow$Year<=end.year,],
                          Mean  = mean  (Q, na.rm=na.rm$na.rm.global),
                          Median= stats::median(Q, na.rm=na.rm$na.rm.global),
                          Maximum   = max   (Q, na.rm=na.rm$na.rm.global),
                          Minimum   = min   (Q, na.rm=na.rm$na.rm.global))
   Q.all.longterm.CY$Month <- "Long-term"

   Q.longterm.CY <- rbind(Q.month.longterm.CY, Q.all.longterm.CY)
   Q.longterm.CY$Month <- factor(Q.longterm.CY$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))
   Q.longterm.CY <- with(Q.longterm.CY, Q.longterm.CY[order(Month),])

   #WATER YEAR

   Q.month.longterm.WY <- plyr::ddply(flow[flow$WaterYear >= start.year & flow$WaterYear<=end.year,], "Month", function(x,na.rm){    # all missing values always excluded
     Mean  = mean  (x$Q, na.rm=na.rm$na.rm.global)
     Median= stats::median(x$Q, na.rm=na.rm$na.rm.global)
     Maximum   = max   (x$Q, na.rm=na.rm$na.rm.global)
     Minimum   = min   (x$Q, na.rm=na.rm$na.rm.global)
     data.frame(Mean=Mean, Median=Median, Maximum=Maximum, Minimum=Minimum)
   }, na.rm=na.rm)
   Q.month.longterm.WY$Month <- as.factor(Q.month.longterm.WY$Month)


   Q.all.longterm.WY <- plyr::summarize( flow[flow$WaterYear >= start.year & flow$WaterYear<=end.year,],
                                         Mean  = mean  (Q, na.rm=na.rm$na.rm.global),
                                         Median= stats::median(Q, na.rm=na.rm$na.rm.global),
                                         Maximum   = max   (Q, na.rm=na.rm$na.rm.global),
                                         Minimum   = min   (Q, na.rm=na.rm$na.rm.global))
   Q.all.longterm.WY$Month <- "Long-term"

   Q.longterm.WY <- rbind(Q.month.longterm.WY, Q.all.longterm.WY)
   Q.longterm.WY$Month <- factor(Q.longterm.WY$Month, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term"))
   Q.longterm.WY <- with(Q.longterm.WY, Q.longterm.WY[order(Month),])

#  Write out the summary table for comparison to excel spreadsheet
   file.CY.stat.csv <- NA
   if(write.CY.stat.csv){
      file.CY.stat.csv <-file.path(report.dir, paste(Station.Code,"-longterm-CY-summary-stat.csv", sep=""))
      temp <- Q.longterm.CY
      temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], csv.nddigits)  # round the output
      utils::write.csv(temp, file=file.CY.stat.csv, row.names=FALSE)
   }
#  Write out the summary table for comparison to excel spreadsheet WATER YEAR
   file.WY.stat.csv <- NA
   if(write.WY.stat.csv){
     file.WY.stat.csv <-file.path(report.dir, paste(Station.Code,"-longterm-WY-summary-stat.csv", sep=""))
     temp <- Q.longterm.WY
     temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], csv.nddigits)  # round the output
     utils::write.csv(temp, file=file.WY.stat.csv, row.names=FALSE)
   }

#  Write out thesummary table in transposed format
   Month.CY <- Q.longterm.CY[,"Month"]
   Q.longterm.trans.CY <- t(Q.longterm.CY[, !grepl('^Month', names(Q.longterm.CY))])
   colnames(Q.longterm.trans.CY) <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term")
   file.CY.stat.trans.csv <- NA
   if(write.CY.stat.trans.csv){
     file.CY.stat.trans.csv <-file.path(report.dir,paste(Station.Code,"-longterm-CY-summary-stat-trans.csv", sep=""))
     temp <- Q.longterm.trans.CY
     temp <- round(temp, csv.nddigits) # round the output
     utils::write.csv(temp, file=file.CY.stat.trans.csv, row.names=TRUE)
   }

#  Write out thesummary table in transposed format WATER YEAR
   Month.WY <- Q.longterm.WY[,"Month"]
   Q.longterm.trans.WY <- t(Q.longterm.WY[, !grepl('^Month', names(Q.longterm.WY))])
   colnames(Q.longterm.trans.WY) <- c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term")
   file.WY.stat.trans.csv <- NA
   if(write.WY.stat.trans.csv){
     file.WY.stat.trans.csv <-file.path(report.dir,paste(Station.Code,"-longterm-WY-summary-stat-trans.csv", sep=""))
     temp <- Q.longterm.trans.WY
     temp <- round(temp, csv.nddigits) # round the output
     utils::write.csv(temp, file=file.WY.stat.trans.csv, row.names=TRUE)
   }
   return(list(Q.CY.stat.longterm=Q.longterm.CY,
               Q.CY.stat.longterm.trans=Q.longterm.trans.CY,
               file.CY.stat.csv=file.CY.stat.csv,
               file.CY.stat.trans.csv=file.CY.stat.trans.csv,
               Q.WY.stat.longterm=Q.longterm.WY,
               Q.WY.stat.longterm.trans=Q.longterm.trans.WY,
               file.WY.stat.csv=file.WY.stat.csv,
               file.WY.stat.trans.csv=file.WY.stat.trans.csv,
               na.rm=na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function
