#' @title Compute long-term daily (calendar or water year) summary statistics.
#'
#' @description Computes long-term daily summary statistics on \code{flow$Q} variable
#'    between \code{start.year} and \code{end.year} inclusive for calendar or water years.
#'    It (optionally) saves the results in *.csv and *.pdf files.
#'
#' @template station.name
#' @template flow.data
#' @template HYDAT
#' @template start.year
#' @template end.year
#' @template water.year
#' @param write.table Should a file be created with the computed statistics?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-longterm-summary-stat.csv'))}.
#' @param write.transposed.table Should a file be created with the transposed of the statistics report?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-longterm-summary-stat-trans.csv'))}.
#' @template report.dir
#' @template csv.nddigits
#' @template na.rm
#'
#' @return A list with the following elements:
#'   \item{Q.cumulative.daily}{Data frame with the long-term daily statistics of \code{flow.data$Q}  between \code{start.year} and \code{end.year}}
#'   \item{Q.cumulative.daily.trans}{Data frame with the long-term daily statistics of \code{flow.data$Q} transposed.}
#'   \item{file.stat.csv}{Object with file name of *.csv file with long term daily summary statistics.}
#'    \item{file.stat.trans.csv}{Object with file name of *.csv file with transposed long-term daily summary statistics.}
#'    \item{na.rm}{Missing value flags.}
#'    \item{Version}{Version of this function.}
#'    \item{Date}{Date function was run.}
#' @examples
#' \dontrun{
#' stat.daily <- compute.Q.cumulative.daily(
#'                          station.name  ='ABCDE',
#'                          flow.data          =flow,
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

compute.Q.cumulative.daily <- function(
  station.name=NULL,
  flow.data=NULL,
  HYDAT=NULL,
  start.year=NULL, #not required
  end.year=NULL, #not required
  water.year= FALSE, #not required
  rolling.mean=1,
  write.table=FALSE,         # write out calendar year statistics
  write.transposed.table=FALSE,   # write out statistics in transposed format
  write.cumulative.table=FALSE,         # write out calendar year statistics
  write.cumulative.transposed.table=FALSE,   # write out statistics in transposed format
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

  if( !is.null(HYDAT) & !is.null(flow.data))  {stop("Must select either flow.data or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & is.null(station.name))  {stop("station.name required with flow.data parameter.")}
  if( is.null(HYDAT) & !is.character(station.name))  {stop("station.name must be a character string.")}
  if( is.null(HYDAT) & length(station.name)>1)        {stop("station.name cannot have length > 1")}
  if( is.null(flow.data) & is.null(HYDAT)){stop("Flow or HYDAT parameters must be set")}
  if( is.null(HYDAT) & !is.data.frame(flow.data))         {stop("Flow is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Q") %in% names(flow.data))){stop("Flow dataframe doesn't contain the variables Date and Q.")}
  if( is.null(HYDAT) & ! inherits(flow.data$Date[1], "Date")){stop("Date column in Flow data frame is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flow.data$Q))          {stop("Q column in flow.data dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flow.data$Q <0, na.rm=TRUE))   {stop('flow.data cannot have negative values - check your data')}
  if( !(is.numeric(start.year) | is.null(start.year)))   {stop("start.year must be numeric.")}
  if( !(is.numeric(end.year) | is.null(end.year)))   {stop("end.year must be numeric.")}
  if( !is.logical(water.year))  {stop("water.year must be logical (TRUE/FALSE")}
  if( !is.logical(write.table))  {stop("write.table must be logical (TRUE/FALSE")}
  if( !is.logical(write.transposed.table)){stop("write.transposed.table must be logical (TRUE/FALSE")}
  if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}

  if(!is.numeric(csv.nddigits)){ stop("csv.nddigits must be numeric")}
  csv.nddigits <- round(csv.nddigits)[1]

  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if( !is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.


  if (!is.null(HYDAT)) {
    if (is.null(station.name)) {station.name <- HYDAT}
    flow.data <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
    flow.data <- dplyr::select(flow.data,Date,Q=Value)
  }


  daily.empty <- data.frame(Date=seq(as.Date((paste((as.numeric(format(min(flow.data$Date),'%Y'))),01,01,sep="-")),"%Y-%m-%d"), as.Date((paste((as.numeric(format(max(flow.data$Date),'%Y'))),12,31,sep="-")),"%Y-%m-%d"), by="days")) # make empty data with the start and end dates
  flow.data <- merge(daily.empty,flow.data,by="Date",all = TRUE)

  #  create the year (annual ) and month variables
  flow.data$Year  <- lubridate::year(flow.data$Date)
  flow.data$MonthNum  <- lubridate::month(flow.data$Date)
  flow.data$Month <- month.abb[flow.data$MonthNum]
  flow.data$WaterYear <- as.numeric(ifelse(flow.data$MonthNum>=10,flow.data$Year+1,flow.data$Year))
  flow.data$DayofYear <- lubridate::yday(flow.data$Date)
  flow.data$WaterDoY <- ifelse(flow.data$MonthNum<10,flow.data$DayofYear+92,
                               ifelse((as.Date(with(flow.data, paste(Year+1,01,01,sep="-")),"%Y-%m-%d")-as.Date(with(flow.data, paste(Year,01,01,sep="-")),"%Y-%m-%d"))==366,
                                      flow.data$DayofYear-274,
                                      flow.data$DayofYear-273))
  # Add cumulative values
  flow.data <- dplyr::mutate(dplyr::group_by(flow.data,Year),CumQ=cumsum(Q))
  flow.data <- dplyr::mutate(dplyr::group_by(flow.data,WaterYear),WYCumQ=cumsum(Q))

  # if no start and or end year, set at the minimum/max years in the dataset
  if (water.year) {
    if (!is.numeric(start.year)) {start.year <- min(flow.data$WaterYear)}
    if (!is.numeric(end.year)) {end.year <- max(flow.data$Year)}
  } else {
    if (!is.numeric(start.year)) {start.year <- min(flow.data$Year)}
    if (!is.numeric(end.year)) {end.year <- max(flow.data$Year)}
  }
  if(! (start.year <= end.year))    {stop("start.year must be less than end.year")}


  if (water.year){
    flow.data$AnalysisYear <- flow.data$WaterYear
    flow.data$AnalysisDoY <- flow.data$WaterDoY
    flow.data$AnalysisDate <- as.Date(flow.data$WaterDoY, origin = "1899-09-30")
    flow.data$AnalysisCumQ <- flow.data$WYCumQ
  } else {
    flow.data$AnalysisYear <- flow.data$Year
    flow.data$AnalysisDoY <- flow.data$DayofYear
    flow.data$AnalysisDate <- as.Date(flow.data$DayofYear, origin = "1899-12-31")
    flow.data$AnalysisCumQ <- flow.data$CumQ
  }

  flow.data <- dplyr::filter(flow.data,AnalysisYear >= start.year & AnalysisYear <= end.year)

  flow.data.365 <- dplyr::filter(flow.data,AnalysisDoY<366)


  ### DAILY CUMULATIVE STATS

  Q.cumulative <- dplyr::summarise(dplyr::group_by(flow.data.365,AnalysisDate,AnalysisDoY),
                              Mean=mean(AnalysisCumQ*86400, na.rm=na.rm$na.rm.global),
                              Minimum=min(AnalysisCumQ*86400, na.rm=na.rm$na.rm.global),
                              "5th Percentile"=quantile(AnalysisCumQ*86400,.05, na.rm=TRUE),
                              "25th Percentile"=quantile(AnalysisCumQ*86400,.25, na.rm=TRUE),
                              Median=median(AnalysisCumQ*86400, na.rm=na.rm$na.rm.global),
                              "75th Percentile"=quantile(AnalysisCumQ*86400,.75, na.rm=TRUE),
                              "95th Percentile"=quantile(AnalysisCumQ*86400,.95, na.rm=TRUE),
                              Maximum=max(AnalysisCumQ*86400, na.rm=na.rm$na.rm.global))
  Q.cumulative <- dplyr::rename(Q.cumulative,Date=AnalysisDate,"Day of Year"=AnalysisDoY)


  # WRITE THE CUMULATIVE FILES
  ##################
  #  Write out summary tables for calendar years
  #  Write out the summary table for comparison to excel spreadsheet
  file.cumulative.csv <- NA
  if(write.table){
    file.cumulative.csv <-file.path(report.dir, paste(station.name,"-longterm-daily-cumulative.csv", sep=""))
    temp <- Q.cumulative
    temp$Date <- format(as.Date(temp$Date),format="%b-%d")
    temp[,3:ncol(temp)] <- round(temp[,3:ncol(temp)], csv.nddigits)  # round the output
    utils::write.csv(temp, file=file.cumulative.csv, row.names=FALSE)
  }

  #  Write out thesummary table in transposed format
  Q.cumulative.trans <- tidyr::gather(Q.cumulative,Statistic,Value,-Date)
  Q.cumulative.trans$Date <- format(as.Date(Q.cumulative.trans$Date),format="%b-%d")
  col.ord <- c("Statistic",unique(Q.cumulative.trans$Date))
  row.ord <- unique(Q.cumulative.trans$Statistic)
  Q.cumulative.trans <- tidyr::spread(Q.cumulative.trans,Date,Value)
  Q.cumulative.trans <- dplyr::arrange(Q.cumulative.trans,match(Statistic,row.ord))
  Q.cumulative.trans <- Q.cumulative.trans[,col.ord]
  file.cumulative.trans.csv <- NA
  if(write.transposed.table){
    file.stat.trans.csv <-file.path(report.dir,paste(station.name,"-longterm-daily-cumulative-trans.csv", sep=""))
    temp <- Q.cumulative.trans
    temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], csv.nddigits)  # round the output
    utils::write.csv(temp, file=file.stat.trans.csv, row.names=FALSE)
  }


  return(list("station name"= station.name,
              "year type"=ifelse(!water.year,"Calendar Year (Jan-Dec)","Water Year (Oct-Sep)"),
              "year range"=paste0(start.year," - ",end.year),
              Q.stat.cumulative=Q.cumulative,
              Q.stat.cumulative.trans=Q.cumulative.trans,
              file.cumulative.csv=file.cumulative.csv,
              file.cumulative.trans.csv=file.cumulative.trans.csv,
              na.rm=na.rm,
              Version=Version,
              Date=Sys.time()))
} # end of function
