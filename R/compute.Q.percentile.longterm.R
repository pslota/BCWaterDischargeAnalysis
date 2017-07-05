#' @title Compute long term annual (calendar and water year) percentiles.
#'
#' @description Computes many percentiles on the \code{flow$Q} variable
#'    between \code{start.year} and \code{end.year} inclusive.
#'    It (optionally) saves the resutls in *.csv and *.pdf files.
#'
#' @template Station.Code
#' @template flow
#' @template start.year
#' @param per.list Percentiles to be computed
#' @param write.cy.stat.csv Should a file be created with the computed calendar year percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-cy-percentile-stat.csv'))}.
#' @param write.cy.stat.trans.csv Should a file be created with the transposed of the calendar year percentile report?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-cy-percentile-stat-trans.csv'))}.
#' @param write.wy.stat.csv Should a file be created with the computed water year percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-wy-percentile-stat.csv'))}.
#' @param write.wy.stat.trans.csv Should a file be created with the transposed of the water year percentile report?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-wy-percentile-stat-trans.csv'))}.

#' @template report.dir
#' @template csv.nddigits
#' @template na.rm
#' @template debug
#' @return A list with the following elements:
#'   \item{Q.cy.percentile.stat}{Data frame with the calendar year percentiles of \code{flow$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.cy.percentile.stat.trans}{Data frame with the calendar year percentiles of \code{flow$Q} transposed.}
#'   \item{file.cy.stat.csv}{Object with file name of *.csv file with calendar year percentile statistics.}
#'    \item{file.cy.stat.trans.csv}{Object with file name of *.csv file with transposed calendar year percentile statistics.}
#'   \item{Q.wy.percentile.stat}{Data frame with the water year percentiles of \code{flow$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.wy.percentile.stat.trans}{Data frame with the water year percentiles of \code{flow$Q} transposed.}
#'   \item{file.wy.stat.csv}{Object with file name of *.csv file with water year percentile statistics.}
#'    \item{file.wy.stat.trans.csv}{Object with file name of *.csv file with transposed water year percentile statistics.}
#'    \item{na.rm}{Missing value flags.}
#'    \item{Version}{Version of this function.}
#'    \item{Date}{Date function was run.}
#'
#' @examples
#' \dontrun{
#' percentile.longterm <- compute.Q.percentile.longterm(
#'                          Station.Code  ='ABCDE',
#'                          flow          =flow,
#'                          start.year    =1960,
#'                          end.year      =2015)
#' }
#'
#' @export
#' @import ggplot2
#' @import scales
#' @import utils

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

compute.Q.percentile.longterm <- function(
                          Station.Code='XXXXX',
                          flow,
                          start.year=9999,
                          end.year=0,
                          per.list=c(1,2,seq(5,95,5),98,99),  # these the standard percentiles
                          write.cy.stat.csv=TRUE,        # write out calendar year statistics
                          write.cy.stat.trans.csv=TRUE,  # write out calendar year statistics in transposed format
                          write.wy.stat.csv=TRUE,        # write out water year statistics
                          write.wy.stat.trans.csv=TRUE,  # write out water year statistics in transposed format
                          report.dir=".",
                          csv.nddigits=3,              # number of decimal digits for csv file
                          na.rm=list(na.rm.global=TRUE),
                          debug=FALSE
                          ){
# Check the man-roxygen for definition of the input parameters
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

   if(!is.numeric(per.list))         {stop("per.list must be numeric")}
   if(!all(per.list <100 & per.list >0)){stop('per.list must be between 0 to 100 (exclusive)')}

   if( !is.logical(write.cy.stat.csv))  {stop("write.cy.stat.csv must be logical (TRUE/FALSE)")}
   if( !is.logical(write.cy.stat.trans.csv)){stop("write.cy.stat.trans.csv must be logical (TRUE/FALSE)")}
   if( !is.logical(write.wy.stat.csv))  {stop("write.wy.stat.csv must be logical (TRUE/FALSE)")}
   if( !is.logical(write.wy.stat.trans.csv)){stop("write.wy.stat.trans.csv must be logical (TRUE/FALSE)")}
   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}

   if( !is.numeric(csv.nddigits)){stop("csv.nddigits must be numeric")}
   csv.nddigits <- round(csv.nddigits)[1]

   if( !is.list(na.rm))              {stop("na.rm is not a list") }
   if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
   my.na.rm <- list(na.rm.global=FALSE)
   if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
   my.na.rm[names(na.rm)]<- na.rm
   na.rm <- my.na.rm  # set the na.rm for the rest of the function.


#  create the year (annual) and month variables
   flow$Year  <- as.numeric(format(flow$Date, "%Y"))
   flow$Month <- as.factor(format(flow$Date, '%b'))
   flow$MonthNum <- as.numeric(format(flow$Date, '%m'))
   flow$WaterYear <- as.numeric(ifelse(flow$MonthNum>=10,flow$Year+1,flow$Year))

   if(debug)browser()

#  Compute water year long-term stats
   # compute the monthly percentiles for the entire period of record
   Q.per.month.cy <- plyr::ddply(flow[flow$Year >= start.year & flow$Year <=end.year,], "Month", function(fy, na.rm){
         per <- try(stats::quantile(fy$Q, prob=per.list/100,na.rm=na.rm$na.rm.global), silent=TRUE)
         if(class(per) == "try-error") per <- rep(NA, length(per.list))
         per <- matrix(per, nrow=1)
         colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
        data.frame(per, stringsAsFactors=FALSE)
   }, na.rm=na.rm)
   Q.per.month.cy$Month <- as.factor(Q.per.month.cy$Month)

   # compute the percentiles for the entire period of record
   flow$Month <- 99
   Q.per.all.cy   <-plyr::ddply(flow[flow$Year >= start.year & flow$Year <=end.year,], "Month",  function(fy, na.rm){
        per <- try(stats::quantile(fy$Q, prob=per.list/100,na.rm=na.rm$na.rm.global), silent=TRUE)
        if(class(per) == "try-error") per <- rep(NA, length(per.list))
        per <- matrix(per, nrow=1)
        colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
        data.frame(per, stringsAsFactors=FALSE)
   }, na.rm=na.rm)
   Q.per.all.cy$Month <- "Long-term"

   Q.per.longterm.cy <- rbind(Q.per.month.cy, Q.per.all.cy)
   Q.per.longterm.cy$Month <- factor(Q.per.longterm.cy$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))
   Q.per.longterm.cy <- with(Q.per.longterm.cy, Q.per.longterm.cy[order(Month),])



#  Compute water year long-term stats
   flow$Month <- as.factor(format(flow$Date, '%b')) #reset the months

   # compute the monthly percentiles for the entire period of record
   Q.per.month.wy <- plyr::ddply(flow[flow$WaterYear >= start.year & flow$WaterYear <=end.year,], "Month", function(fy, na.rm){
     per <- try(stats::quantile(fy$Q, prob=per.list/100,na.rm=na.rm$na.rm.global), silent=TRUE)
     if(class(per) == "try-error") per <- rep(NA, length(per.list))
     per <- matrix(per, nrow=1)
     colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
     data.frame(per, stringsAsFactors=FALSE)
   }, na.rm=na.rm)
   Q.per.month.wy$Month <- as.factor(Q.per.month.wy$Month)


   # compute the percentiles for the entire period of record
   flow$Month <- 99
   Q.per.all.wy   <-plyr::ddply(flow[flow$WaterYear >= start.year & flow$WaterYear <=end.year,], "Month",  function(fy, na.rm){
     per <- try(stats::quantile(fy$Q, prob=per.list/100,na.rm=na.rm$na.rm.global), silent=TRUE)
     if(class(per) == "try-error") per <- rep(NA, length(per.list))
     per <- matrix(per, nrow=1)
     colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
     data.frame(per, stringsAsFactors=FALSE)
   }, na.rm=na.rm)
   Q.per.all.wy$Month <- "Long-term"

   Q.per.longterm.wy <- rbind(Q.per.month.wy, Q.per.all.wy)
   Q.per.longterm.wy$Month <- factor(Q.per.longterm.wy$Month, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term"))
   Q.per.longterm.wy <- with(Q.per.longterm.wy, Q.per.longterm.wy[order(Month),])

#  Write out summary tables for calendar years
   #  Write out the summary table for comparison to excel spreadsheet
   file.cy.stat.csv <- NA
   if(write.cy.stat.csv){
      file.cy.stat.csv <- file.path(report.dir,paste(Station.Code,"-longterm-cy-percentile-stat.csv", sep=""))
      temp <- Q.per.longterm.cy
      temp[, 2:ncol(Q.per.longterm.cy)] <- round(temp[,2:ncol(Q.per.longterm.cy)], csv.nddigits)
      utils::write.csv(temp, file=file.cy.stat.csv, row.names=FALSE)
   }

   #  Write out thesummary table in transposed format
   Month.cy <- Q.per.longterm.cy[,"Month"]
   Q.per.longterm.trans.cy <- t(Q.per.longterm.cy[, !grepl('^Month', names(Q.per.longterm.cy))])
   colnames(Q.per.longterm.trans.cy) <-  c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term")
   file.cy.stat.trans.csv<- NA
   if(write.cy.stat.trans.csv){
     file.cy.stat.trans.csv <-file.path(report.dir, paste(Station.Code,"-longterm-cy-percentile-stat-trans.csv", sep=""))
     temp<- Q.per.longterm.trans.cy
     temp <- round(temp, csv.nddigits)
     utils::write.csv(temp, file=file.cy.stat.trans.csv, row.names=TRUE)
   }

#  Write out summary tables for water years
   #  Write out the summary table for comparison to excel spreadsheet
   file.wy.stat.csv <- NA
   if(write.wy.stat.csv){
     file.wy.stat.csv <- file.path(report.dir,paste(Station.Code,"-longterm-wy-percentile-stat.csv", sep=""))
     temp <- Q.per.longterm.wy
     temp[, 2:ncol(Q.per.longterm.wy)] <- round(temp[,2:ncol(Q.per.longterm.wy)], csv.nddigits)
     utils::write.csv(temp, file=file.wy.stat.csv, row.names=FALSE)
   }

   #  Write out thesummary table in transposed format
   Month.wy <- Q.per.longterm.wy[,"Month"]
   Q.per.longterm.trans.wy <- t(Q.per.longterm.wy[, !grepl('^Month', names(Q.per.longterm.wy))])
   colnames(Q.per.longterm.trans.wy) <-  c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term")
   file.wy.stat.trans.csv<- NA
   if(write.wy.stat.trans.csv){
     file.wy.stat.trans.csv <-file.path(report.dir, paste(Station.Code,"-longterm-wy-percentile-stat-trans.csv", sep=""))
     temp<- Q.per.longterm.trans.wy
     temp <- round(temp, csv.nddigits)
     utils::write.csv(temp, file=file.wy.stat.trans.csv, row.names=TRUE)
   }

   return(list(Q.cy.percentile.stat=Q.per.longterm.cy,
               Q.cy.percentile.stat.trans=Q.per.longterm.trans.cy,
               Q.wy.percentile.stat=Q.per.longterm.wy,
               Q.wy.percentile.stat.trans=Q.per.longterm.trans.wy,
               file.cy.stat.csv=file.cy.stat.csv,
               file.cy.stat.trans.csv=file.cy.stat.trans.csv,
               file.wy.stat.csv=file.wy.stat.csv,
               file.wy.stat.trans.csv=file.wy.stat.trans.csv,
               na.rm=na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function
