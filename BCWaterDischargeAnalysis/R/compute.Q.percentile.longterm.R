#' @title Compute long term annual (calendar year) percentiles.
#'
#' @description Computes many percentiles on the \code{flow$Q} variable
#'    between \code{start.year} and \code{end.year} inclusive.
#'    It (optionally) saves the resutls in *.csv and *.pdf files.
#'
#' @template Station.Code
#' @template Station.Area
#' @template flow
#' @template start.year
#' @param per.list Percentiles to be computed
#' @param write.stat.csv Should a file be created with the computed percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-percentile-stat.csv'))}.
#' @param write.stat.trans.csv Should a file be created with the transposed of the percentile report?
#'    The file name will be  \code{file.path(report.dir,paste(Station.Code,'-longterm-percentile-stat-trabs.csv'))}.

#' @template report.dir
#' @template csv.nddigits
#' @template na.rm
#' @template debug
#' @return A list with the following elements:
#'   \item{Q.percentile.stat}{Data frame with the percentiles of \code{flow$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.percentile.stat.trans}{Data frame with the percentiles of \code{flow$Q} transposed.}
#'   \item{file.stat.csv}{File name of *.csv file with percentile statistics.}
#'    \item{file.stat.trans.csv}{File name of *.csv file with transposed percentile statistics.}
#'    \item{na.rm}{Missing value flags.}
#'    \item{Version}{Version of this function.}
#'    \item{Date}{Date function was run.}
#'
#' @examples
#' \dontrun{
#' percentile.longterm <- compute.Q.percentile.longterm(Station.Code='ABCDE',
#'                          Station.Area=2458,
#'                          flow=flow,
#'                          start.year=1960,
#'                          end.year=2015)
#' }
#'
#' @export

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
                          Station.Area=NA,
                          flow,
                          start.year=9999,
                          end.year=0,
                          per.list=c(1,2,seq(5,95,5),98,99),  # these the standard percentiles
                          write.stat.csv=TRUE,        # write out statistics
                          write.stat.trans.csv=TRUE,  # write out statistics in transposed format
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
   Version <- '2017-02-15'

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

   if(!is.numeric(per.list))         {stop("per.list must be numeric")}
   if(!all(per.list <100 & per.list >0)){stop('per.list must be between 0 to 100 (exclusive)')}

   if( !is.logical(write.stat.csv))  {stop("write.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.stat.trans.csv)){stop("write.stat.trans.csv must be logical (TRUE/FALSE")}
   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}

   if( !is.numeric(csv.nddigits)){stop("csv.nddigits must be numeric")}
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
   if(debug)browser()
   Q.per.month <- plyr::ddply(flow[flow$Year >= start.year & flow$Year <=end.year,], "Month", function(fy, na.rm){
         per <- try(quantile(fy$Q, prob=per.list/100,na.rm=na.rm$na.rm.global), silent=TRUE)
         if(class(per) == "try-error") per <- rep(NA, length(per.list))
         per <- matrix(per, nrow=1)
         colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
        data.frame(per, stringsAsFactors=FALSE)
   }, na.rm=na.rm)
   Q.per.month$Month <- month.abb[Q.per.month$Month]

   # compute the percentiles for the entire period of record
   flow$Month <- 99
   Q.per.all   <-plyr::ddply(flow[flow$Year >= start.year & flow$Year <=end.year,], "Month",  function(fy, na.rm){
        per <- try(quantile(fy$Q, prob=per.list/100,na.rm=na.rm$na.rm.global), silent=TRUE)
        if(class(per) == "try-error") per <- rep(NA, length(per.list))
        per <- matrix(per, nrow=1)
        colnames(per) <- paste("P", formatC(per.list, width=2, format="d", flag="0"), sep="")
        data.frame(per, stringsAsFactors=FALSE)
   }, na.rm=na.rm)
   Q.per.all$Month <- "All years"

   Q.per.longterm <- rbind(Q.per.month, Q.per.all)


#  Write out the summary table for comparison to excel spreadsheet
   file.stat.csv <- NA
   if(write.stat.csv){
      file.stat.csv <- file.path(report.dir,paste(Station.Code,"-longterm-percentile-stat.csv", sep=""))
      temp <- Q.per.longterm
      temp[, 2:ncol(Q.per.longterm)] <- round(temp[,2:ncol(Q.per.longterm)], csv.nddigits)
      write.csv(temp, file=file.stat.csv, row.names=FALSE)
   }

#  Write out thesummary table in transposed format
   Month <- Q.per.longterm[,"Month"]
   Q.per.longterm.trans <- t(Q.per.longterm[, !grepl('^Month', names(Q.per.longterm))])
   colnames(Q.per.longterm.trans) <-  c(toupper(month.abb[1:12]),"All data")
   file.stat.trans.csv<- NA
   if(write.stat.trans.csv){
     file.stat.trans.csv <-file.path(report.dir, paste(Station.Code,"-longterm-percentile-stat-trans.csv", sep=""))
     temp<- Q.per.longterm.trans
     temp <- round(temp, csv.nddigits)
     write.csv(temp, file=file.stat.trans.csv, row.names=TRUE)
   }

   return(list(Q.percentile.stat=Q.per.longterm,
               Q.percentile.stat.trans=Q.per.longterm.trans,
               file.stat.csv=file.stat.csv,
               file.stat.trans.csv=file.stat.trans.csv,
               na.rm=na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function
