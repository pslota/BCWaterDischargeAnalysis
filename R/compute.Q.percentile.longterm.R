#' @title Compute long term annual (calendar and water year) percentiles.
#'
#' @description Computes many percentiles on the \code{flow.data$Q} variable
#'    between \code{start.year} and \code{end.year} inclusive for both calendar and water years.
#'    It (optionally) saves the results in *.csv and *.pdf files.
#'
#' @template station.name
#' @template flow.data
#' @template start.year
#' @param per.list Percentiles to be computed
#' @param write.stat.csv Should a file be created with the computed calendar year percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-longterm-percentile-stat.csv'))}.
#' @param write.stat.trans.csv Should a file be created with the transposed of the calendar year percentile report?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-longterm-percentile-stat-trans.csv'))}.

#' @template report.dir
#' @template csv.nddigits
#' @template na.rm
#' @template debug
#' @return A list with the following elements:
#'   \item{Q.percentile.stat}{Data frame with the calendar year percentiles of \code{flow.data$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.percentile.stat.trans}{Data frame with the calendar year percentiles of \code{flow.data$Q} transposed.}
#'   \item{file.stat.csv}{Object with file name of *.csv file with calendar year percentile statistics.}
#'    \item{file.stat.trans.csv}{Object with file name of *.csv file with transposed calendar year percentile statistics.}
#'    \item{na.rm}{Missing value flags.}
#'    \item{Version}{Version of this function.}
#'    \item{Date}{Date function was run.}
#'
#' @examples
#' \dontrun{
#' percentile.longterm <- compute.Q.percentile.longterm(
#'                          station.name  ='ABCDE',
#'                          flow.data          =flow,
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
                          station.name=NULL,
                          flow.data=NULL,
                          HYDAT=NULL,
                          start.year=NULL,
                          end.year=NULL,
                          water.year=FALSE,
                          per.list=c(1,2,seq(5,95,5),98,99),  # these the standard percentiles
                          write.stat.csv=FALSE,        # write out calendar year statistics
                          write.stat.trans.csv=FALSE,  # write out calendar year statistics in transposed format
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

   if( is.null(flow.data) & is.null(HYDAT)){stop("Flow or HYDAT parameters must be set")}
   if( !is.null(HYDAT) & !is.null(flow.data))  {stop("Must select either flow.data or HYDAT parameters, not both.")}
   if( is.null(HYDAT) & is.null(station.name))  {stop("station.name required with flow.data parameter.")}
   if( length(station.name)>1)        {stop("station.name cannot have length > 1")}
   if( is.null(HYDAT) & !is.data.frame(flow.data))         {stop("flow.data is not a data frame.")}
   if( is.null(HYDAT) & !all(c("Date","Q") %in% names(flow.data))){
                                      stop("flow.data dataframe doesn't contain the variables Date and Q.")}
   if( is.null(HYDAT) & !inherits(flow.data$Date[1], "Date")){
                                      stop("Date column in flow.data data frame is not a date.")}
   if( is.null(HYDAT) & !is.numeric(flow.data$Q))          {stop("Q column in flow.data dataframe is not numeric.")}
   if( is.null(HYDAT) & any(flow.data$Q <0, na.rm=TRUE))   {stop('flow.data cannot have negative values - check your data')}

   if( !is.numeric(per.list))         {stop("per.list must be numeric")}
   if( !all(per.list <100 & per.list >0)){stop('per.list must be between 0 to 100 (exclusive)')}
   if( !is.logical(water.year))  {stop("water.year must be logical (TRUE/FALSE")}
   if( !is.logical(write.stat.csv))  {stop("write.stat.csv must be logical (TRUE/FALSE)")}
   if( !is.logical(write.stat.trans.csv)){stop("write.stat.trans.csv must be logical (TRUE/FALSE)")}
   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}

   if( !is.numeric(csv.nddigits)){stop("csv.nddigits must be numeric")}
   csv.nddigits <- round(csv.nddigits)[1]

   if( !is.list(na.rm))              {stop("na.rm is not a list") }
   if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
   my.na.rm <- list(na.rm.global=FALSE)
   if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
   my.na.rm[names(na.rm)]<- na.rm
   na.rm <- my.na.rm  # set the na.rm for the rest of the function.

   if (!is.null(HYDAT)) {
     if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("HYDAT station does not exist.")}
     if (is.null(station.name)) {station.name <- HYDAT}
     flow.data <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
     flow.data <- dplyr::select(flow.data,Date,Q=Value)
   }

#  create the year (annual) and month variables
   flow.data$Year  <- lubridate::year(flow.data$Date)
   flow.data$MonthNum  <- lubridate::month(flow.data$Date)
   flow.data$Month <- month.abb[flow.data$MonthNum]
   flow.data$WaterYear <- as.numeric(ifelse(flow.data$MonthNum>=10,flow.data$Year+1,flow.data$Year))

   if(debug)browser()

   if (water.year){
     flow.data$AnalysisYear <- flow.data$WaterYear
   } else {
     flow.data$AnalysisYear <- flow.data$Year
   }

   # Set start and end years if not set
   if (!is.numeric(start.year)) {start.year <- min(flow.data$WaterYear)}
   if (!is.numeric(end.year)) {end.year <- max(flow.data$Year)}
   if(! (start.year <= end.year))    {stop("start.year must be less than end.year")}

#  Compute water year long-term stats
   # compute the monthly percentiles for the entire period of record
   Q.per.month <- plyr::ddply(flow.data[flow.data$AnalysisYear >= start.year & flow.data$AnalysisYear <=end.year,], "Month", function(fy, na.rm){
         per <- try(stats::quantile(fy$Q, prob=per.list/100,na.rm=na.rm$na.rm.global), silent=TRUE)
         if(class(per) == "try-error") per <- rep(NA, length(per.list))
         per <- matrix(per, nrow=1)
         colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
        data.frame(per, stringsAsFactors=FALSE)
   }, na.rm=na.rm)
   Q.per.month$Month <- as.factor(Q.per.month$Month)

   # compute the percentiles for the entire period of record
   flow.data$Month <- 99
   Q.per.all   <-plyr::ddply(flow.data[flow.data$AnalysisYear >= start.year & flow.data$AnalysisYear <=end.year,], "Month",  function(fy, na.rm){
        per <- try(stats::quantile(fy$Q, prob=per.list/100,na.rm=na.rm$na.rm.global), silent=TRUE)
        if(class(per) == "try-error") per <- rep(NA, length(per.list))
        per <- matrix(per, nrow=1)
        colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
        data.frame(per, stringsAsFactors=FALSE)
   }, na.rm=na.rm)
   Q.per.all$Month <- "Long-term"

   Q.per.longterm <- rbind(Q.per.month, Q.per.all)
   Q.per.longterm$Month <- factor(Q.per.longterm$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))
   Q.per.longterm <- with(Q.per.longterm, Q.per.longterm[order(Month),])

#  Write out summary tables for calendar years
   #  Write out the summary table for comparison to excel spreadsheet
   file.stat.csv <- NA
   if(write.stat.csv){
      file.stat.csv <- file.path(report.dir,paste(station.name,"-longterm-percentile-stat.csv", sep=""))
      temp <- Q.per.longterm
      temp[, 2:ncol(Q.per.longterm)] <- round(temp[,2:ncol(Q.per.longterm)], csv.nddigits)
      utils::write.csv(temp, file=file.stat.csv, row.names=FALSE)
   }

   #  Write out thesummary table in transposed format
   Month <- Q.per.longterm[,"Month"]
   Q.per.longterm.trans <- t(Q.per.longterm[, !grepl('^Month', names(Q.per.longterm))])
   colnames(Q.per.longterm.trans) <-  c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term")
   file.stat.trans.csv<- NA
   if(write.stat.trans.csv){
     file.stat.trans.csv <-file.path(report.dir, paste(station.name,"-longterm-percentile-stat-trans.csv", sep=""))
     temp<- Q.per.longterm.trans
     temp <- round(temp, csv.nddigits)
     utils::write.csv(temp, file=file.stat.trans.csv, row.names=TRUE)
   }


   return(list("station name"= station.name,
               "year type"=ifelse(!water.year,"Calendar Year (Jan-Dec)","Water Year (Oct-Sep)"),
               "year range"=paste0(start.year," - ",end.year),
               Q.percentile.stat=Q.per.longterm,
               Q.percentile.stat.trans=Q.per.longterm.trans,
               file.stat.csv=file.stat.csv,
               file.stat.trans.csv=file.stat.trans.csv,
               na.rm=na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function
