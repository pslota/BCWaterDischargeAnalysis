#' @title Compute long-term annual (calendar and water year) summary statistics.
#'
#' @description Computes long-term summary statistics on \code{flow$Q} variable
#'    between \code{start.year} and \code{end.year} inclusive for both calendar and water years.
#'    It (optionally) saves the results in *.csv and *.pdf files.
#'
#' @template Station.Code
#' @template flow
#' @template start.year
#' @param write.cy.stat.csv Should a file be created with the computed statistics?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-cy-summary-stat.csv'))}.
#' @param write.cy.stat.trans.csv Should a file be created with the transposed of the statistics report?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-cy-summary-stat-trans.csv'))}.
#' @param write.wy.stat.csv Should a file be created with the computed statistics?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-wy-summary-stat.csv'))}.
#' @param write.wy.stat.trans.csv Should a file be created with the transposed of the statistics report?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-wy-summary-stat-trans.csv'))}.
#' @template report.dir
#' @template csv.nddigits
#' @template na.rm
#'
#' @return A list with the following elements:
#'   \item{Q.cy.stat.longterm}{Data frame with the long-term statistics of \code{flow$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.cy.stat.longterm.trans}{Data frame with the long-term statistics of \code{flow$Q} transposed.}
#'   \item{file.cy.stat.csv}{Object with file name of *.csv file with long term summary statistics.}
#'    \item{file.cy.stat.trans.csv}{Object with file name of *.csv file with transposed long-term summary statistics.}
#'    \item{Q.wy.stat.longterm}{Data frame with the long-term statistics of \code{flow$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.wy.stat.longterm.trans}{Data frame with the long-term statistics of \code{flow$Q} transposed.}
#'   \item{file.wy.stat.csv}{Object with file name of *.csv file with long-term summary statistics.}
#'    \item{file.wy.stat.trans.csv}{Object with file name of *.csv file with transposed long-term summary statistics.}
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
                          write.cy.stat.csv=TRUE,         # write out calendar year statistics
                          write.cy.stat.trans.csv=TRUE,   # write out statistics in transposed format
                          write.wy.stat.csv=TRUE,         # write out water year statistics
                          write.wy.stat.trans.csv=TRUE,   # write out water year statistics in transposed format
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
   if( !is.logical(write.cy.stat.csv))  {stop("write.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.cy.stat.trans.csv)){stop("write.stat.trans.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.wy.stat.csv))  {stop("write.wy.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.wy.stat.trans.csv)){stop("write.wy.stat.trans.csv must be logical (TRUE/FALSE")}
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

#  Compute calendar year long-term stats

   Q.month.longterm.cy <- plyr::ddply(flow[flow$Year >= start.year & flow$Year<=end.year,], "Month", function(x,na.rm){    # all missing values always excluded
      Mean  = mean  (x$Q, na.rm=na.rm$na.rm.global)
      Median= stats::median(x$Q, na.rm=na.rm$na.rm.global)
      Maximum   = max   (x$Q, na.rm=na.rm$na.rm.global)
      Minimum   = min   (x$Q, na.rm=na.rm$na.rm.global)
      data.frame(Mean=Mean, Median=Median, Maximum=Maximum, Minimum=Minimum)
   }, na.rm=na.rm)
   Q.month.longterm.cy$Month <- as.factor(Q.month.longterm.cy$Month)

   Q.all.longterm.cy <- plyr::summarize( flow[flow$Year >= start.year & flow$Year<=end.year,],
                          Mean  = mean  (Q, na.rm=na.rm$na.rm.global),
                          Median= stats::median(Q, na.rm=na.rm$na.rm.global),
                          Maximum   = max   (Q, na.rm=na.rm$na.rm.global),
                          Minimum   = min   (Q, na.rm=na.rm$na.rm.global))
   Q.all.longterm.cy$Month <- "Long-term"

   Q.longterm.cy <- rbind(Q.month.longterm.cy, Q.all.longterm.cy)
   Q.longterm.cy$Month <- factor(Q.longterm.cy$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))
   Q.longterm.cy <- with(Q.longterm.cy, Q.longterm.cy[order(Month),])

#  Compute water year long-term stats

   Q.month.longterm.wy <- plyr::ddply(flow[flow$WaterYear >= start.year & flow$WaterYear<=end.year,], "Month", function(x,na.rm){    # all missing values always excluded
     Mean  = mean  (x$Q, na.rm=na.rm$na.rm.global)
     Median= stats::median(x$Q, na.rm=na.rm$na.rm.global)
     Maximum   = max   (x$Q, na.rm=na.rm$na.rm.global)
     Minimum   = min   (x$Q, na.rm=na.rm$na.rm.global)
     data.frame(Mean=Mean, Median=Median, Maximum=Maximum, Minimum=Minimum)
   }, na.rm=na.rm)
   Q.month.longterm.wy$Month <- as.factor(Q.month.longterm.wy$Month)


   Q.all.longterm.wy <- plyr::summarize( flow[flow$WaterYear >= start.year & flow$WaterYear<=end.year,],
                                         Mean  = mean  (Q, na.rm=na.rm$na.rm.global),
                                         Median= stats::median(Q, na.rm=na.rm$na.rm.global),
                                         Maximum   = max   (Q, na.rm=na.rm$na.rm.global),
                                         Minimum   = min   (Q, na.rm=na.rm$na.rm.global))
   Q.all.longterm.wy$Month <- "Long-term"

   Q.longterm.wy <- rbind(Q.month.longterm.wy, Q.all.longterm.wy)
   Q.longterm.wy$Month <- factor(Q.longterm.wy$Month, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term"))
   Q.longterm.wy <- with(Q.longterm.wy, Q.longterm.wy[order(Month),])

#  Write out summary tables for calendar years
   #  Write out the summary table for comparison to excel spreadsheet
   file.cy.stat.csv <- NA
   if(write.cy.stat.csv){
      file.cy.stat.csv <-file.path(report.dir, paste(Station.Code,"-longterm-cy-summary-stat.csv", sep=""))
      temp <- Q.longterm.cy
      temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], csv.nddigits)  # round the output
      utils::write.csv(temp, file=file.cy.stat.csv, row.names=FALSE)
   }

   #  Write out thesummary table in transposed format
   Month.cy <- Q.longterm.cy[,"Month"]
   Q.longterm.trans.cy <- t(Q.longterm.cy[, !grepl('^Month', names(Q.longterm.cy))])
   colnames(Q.longterm.trans.cy) <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term")
   file.cy.stat.trans.csv <- NA
   if(write.cy.stat.trans.csv){
     file.cy.stat.trans.csv <-file.path(report.dir,paste(Station.Code,"-longterm-cy-summary-stat-trans.csv", sep=""))
     temp <- Q.longterm.trans.cy
     temp <- round(temp, csv.nddigits) # round the output
     utils::write.csv(temp, file=file.cy.stat.trans.csv, row.names=TRUE)
   }

#  Write out summary tables for water  years
   #  Write out the summary table for comparison to excel spreadsheet WATER YEAR
   file.wy.stat.csv <- NA
   if(write.wy.stat.csv){
     file.wy.stat.csv <-file.path(report.dir, paste(Station.Code,"-longterm-wy-summary-stat.csv", sep=""))
     temp <- Q.longterm.wy
     temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], csv.nddigits)  # round the output
     utils::write.csv(temp, file=file.wy.stat.csv, row.names=FALSE)
   }

   #  Write out thesummary table in transposed format WATER YEAR
   Month.wy <- Q.longterm.wy[,"Month"]
   Q.longterm.trans.wy <- t(Q.longterm.wy[, !grepl('^Month', names(Q.longterm.wy))])
   colnames(Q.longterm.trans.wy) <- c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term")
   file.wy.stat.trans.csv <- NA
   if(write.wy.stat.trans.csv){
     file.wy.stat.trans.csv <-file.path(report.dir,paste(Station.Code,"-longterm-wy-summary-stat-trans.csv", sep=""))
     temp <- Q.longterm.trans.wy
     temp <- round(temp, csv.nddigits) # round the output
     utils::write.csv(temp, file=file.wy.stat.trans.csv, row.names=TRUE)
   }


   return(list(Q.cy.stat.longterm=Q.longterm.cy,
               Q.cy.stat.longterm.trans=Q.longterm.trans.cy,
               file.cy.stat.csv=file.cy.stat.csv,
               file.cy.stat.trans.csv=file.cy.stat.trans.csv,
               Q.wy.stat.longterm=Q.longterm.wy,
               Q.wy.stat.longterm.trans=Q.longterm.trans.wy,
               file.wy.stat.csv=file.wy.stat.csv,
               file.wy.stat.trans.csv=file.wy.stat.trans.csv,
               na.rm=na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function
