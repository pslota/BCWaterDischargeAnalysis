#' @title Perform a volume frequency analysis on annual statistics.
#'
#' @description Performs a volume frequency analysis on annual statistics similar to HEC-SSP.
#'
#' @template station.name
#' @template flow.data
#' @template start.year
#' @param water.year Should results be computed on the water year (starting 1 October)?
#'        The 2013/2014 water year
#'        runs from 2013-10-01 to 2014-09-30.
#' @param roll.avg.days Volume frequency analysis conducted on these rolling averages.
#' @param use.log  Transfrom to log-scale before analysis?
#' @param use.max  Analyze the maximums rather than the minimums.
#' @param prob.plot.position Which plotting positions should be used in the frequency plots. Points are plotted
#'       against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the sample size and
#'       \code{a} and \code{b} are defined as:
#'       (a=0, b=0) for Weibull plotting positions;
#'       (a=.2; b=.3) for Median plotting postions;
#'       (a=.5; b=.5) for Hazen plotting positions.
#' @param prob.scale.points  What points should be plotted along the $X$ axis in the frequency plot.
#' @param fit.distr Which distribution should be fit? PIII = Pearson Log III distribution; weibull=Weibull distribution.
#' @param fit.distr.method Which method used to fit the distribution. MOM=Method of moments; MLE=maximum likelihood estimation.
#' @param fit.quantiles Which quantiles should be estimated from the fitted distribution?
#' @template na.rm
#'
#' @param write.stat.csv Should a file be created with the computed percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,"-annual-vfa-stat.csv", sep=""))}.
#' @param write.stat.trans.csv Should a file be created with the computed percentiles in transposed format?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,"-annual-vfa-stat-trans.csv", sep=""))}.
#' @param write.plotdata.csv Should a file be created with the frequency plot data?
#'    The file name will be  \code{file.path(report.dir, paste(station.name,"-annual-vfa-plotdata.csv", sep=""))}.
#' @param write.quantiles.csv Should a file be created with the fitted quantiles?.
#'    The file name will be  \code{file.path(report.dir, paste(station.name,"-annual-vfa-quantiles.csv", sep=""))}.
#' @param write.quantiles.trans.csv Should a file be created with the (transposed) fitted quantiles?
#'    The file name will be \code{file.path(report.dir, paste(station.name,"-annual-vfa-quantiles-trans.csv", sep=""))}.
#' @param write.frequency.plot Should a file be created with the frequency plot..
#'    The file name will be \code{file.path(report.dir, paste(station.name,"-annual-vfa-frequency-plot.",write.frequency.plot.suffix[1],sep=""))}
#' @param write.frequency.plot.suffix Format of the frequency plot.
#' @template report.dir
#' @template csv.nddigits
#' @template debug
#'
#' @return A list with the following elements:
#'   \item{Q.flow.summary}{Data frame with flow summary.}
#'   \item{start.year}{Start year of the analysis}
#'   \item{end.year}{End year of the analysis}
#'   \item{water.year}{Were computations done on water year?}
#'   \item{use.max}{Were computations done on maximum values.}
#'   \item{roll.avg.days}{Rolling average days on which statistics computed.}
#'   \item{Q.stat}{Data frame with Computed annual summary statistics used in analysis}
#'   \item{Q.stat.trans}{Data frame with Computed annual summary statistics (transposed) used in analysis.}
#'   \item{plotdata}{Data frame with Co-ordinates used in frequency plot.}
#'   \item{prob.plot.position}{Which plotting position was used in frequency plot?}
#'   \item{freqplot}{ggplot2 object with frequency plot}
#'   \item{fit.distr}{Which distribution was fit to the data}
#'   \item{fit}{List of fitted objects from fitdistrplus.}
#'   \item{fitted.quantiles}{Data frame with fitted quantiles.}
#'   \item{fitted.quantiles.trans}{Data frame with fitted quantiles (transposed)}
#'   \item{file.stat.csv}{Object with file name of *.csv file with flow summary}
#'   \item{ file.stat.trans.csv}{Object with file name of *.csv file with flow summary (transposed)}
#'   \item{file.plotdata.csv}{Object with file name of *.csv file with plotting information for frequency plot.}
#'   \item{file.quantile.csv}{Object with file name of fitted quantiles.}
#'   \item{file.quantile.trans.csv}{Object with file name of fitted quantiles (transposed)}
#'   \item{file.frequency.plot}{Object with file name of *.pdf or *.png file with frequency plot}
#'   \item{Version}{Version of this function.}
#'   \item{Date}{Date function was run.}

#' @examples
#' \dontrun{
#' vfa.analysis <- compute.volume.frequency.analysis(
#'                      station.name ='XXX',
#'                      flow.data         =flow,
#'                      start.year   =1960,
#'                      end.year     =2014)
#' }
#' @export
#' @import ggplot2
#' @import scales
#' @import utils
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

# Compute the volume frequency analysis - similar to the HEC-SSP program
# The key difference lies in how the PIII distribuition is fit. I use MLE while
# HEC-SSP appears to use method of moments.

compute.volume.frequency.analysis <- function(station.name, flow.data,
                         start.year=9999, end.year=0000, water.year=FALSE,
                         roll.avg.days=c(1,3,7,15,30,60,90),
                         use.log=FALSE,
                         use.max=FALSE,
                         prob.plot.position=c("weibull","median","hazen"),
                         prob.scale.points=c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                         fit.distr=c("PIII","weibull"),
                         fit.distr.method=ifelse(fit.distr=="PIII","MOM","MLE"),
                         fit.quantiles=c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
                         na.rm=list(na.rm.global=TRUE),
                         write.stat.csv=TRUE, write.stat.trans.csv=TRUE,
                         write.plotdata.csv=FALSE,  # write out the plotting data
                         write.quantiles.csv=TRUE, # write out the fitted quantiles
                         write.quantiles.trans.csv=TRUE,
                         write.frequency.plot=TRUE,  # write out the frequency plot
                         write.frequency.plot.suffix=c("pdf","png"),
                         report.dir='.',
                         csv.nddigits=3, debug=FALSE
                         )
# Input parameter - consult man-roxygen directory
# Output list - see above.

  {
   Version <- packageVersion("BCWaterDischargeAnalysis")
   # replicate the frequency analysis of the HEC-SSP program
   # refer to Chapter 7 of the user manual

   # data checks
   if( !is.character(station.name))  {stop("Station Code must be a character string.")}
   if(length(station.name)>1)        {stop("station.name cannot have length > 1")}
   if( !is.data.frame(flow.data))         {stop("flow.data is not a data frame.")}
   if(! all(c("Date","Q") %in% names(flow.data))){
        stop("flow.data dataframe doesn't contain the variables Date and Q.")}
   if( ! inherits(flow.data$Date[1], "Date")){
        stop("Date column in flow.data data frame is not a date.")}
   if( !is.numeric(flow.data$Q))          {stop("Q column in flow.data dataframe is not numeric.")}
   if( any(flow.data$Q <0, na.rm=TRUE))   {stop('flow.data cannot have negative values - check your data')}
   if(! (is.numeric(start.year) & is.numeric(end.year))){
        stop("start.year and end.year not numberic.")}
   if(! (start.year <= end.year))    {stop("start.year > end.year")}
   if( !is.logical(water.year))  {stop("water.year must be logical (TRUE/FALSE")}
   if( !is.numeric(roll.avg.days))   {stop("roll.avg.days must be numeric")}
   if( !all(roll.avg.days>=0 & roll.avg.days<=180))
       {stop("roll.avg.days must be >0 and <=180)")}
   if( !all(roll.avg.days==floor(roll.avg.days)))
                                     {stop("roll.avg.days must be integers")}
   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exits")}

   if( !is.list(na.rm))              {stop("na.rm is not a list") }
   if( !is.logical(write.stat.csv))  {stop("write.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.stat.trans.csv)){stop("write.stat.trans.csv must be logical (TRUE/FALSE")}
   if(! is.logical(unlist(na.rm))){  {stop("na.rm is list of logical (TRUE/FALSE) values only.")}
   if(! is.logical(use.log))         {stop("use.log must be logical (TRUE/FALSE)")}
   if(! is.logical(use.max))         {stop("use.max must be logical (TRUE/FALSE)")}
   if(! all(prob.plot.position %in% c("weibull","median","hazen"))){}
       stop("prob.plot.position must be one of weibull, median, or hazen")}
   if( !is.numeric(prob.scale.points)){stop("prob.scale.points must be numeric and between 0 and 1 (not inclusive)")}
   if( !all(prob.scale.points>0 & prob.scale.points<1)){
       stop("prob.scale.points must be numeric and between 0 and 1 (not inclusive)")}
   if( !all(fit.distr %in% c("weibull","PIII"))){
       stop("fit.distr must be one of weibull or PIII")}
   if(! is.numeric(fit.quantiles))  {stop("fit.quantiles must be numeric and between 0 and 1 (not inclusive)")}
   if(! all(fit.quantiles >0 & fit.quantiles < 1)){
       stop("fit.quantiles must be numeric and between 0 and 1 (not inclusive)")}
   if(  fit.distr[1]=='weibull' & use.log){stop("Cannot fit Weibull distribution on log-scale")}
   if(  fit.distr[1]=='weibull' & any(flow.data$Q<0, na.rm=TRUE)){stop("cannot fit weibull distribution with negative flow values")}
   if(  fit.distr[1]!="PIII" & fit.distr.method[1]=="MOM"){stop('MOM only can be used with PIII distribution')}

   if( !is.logical(write.stat.csv))      {stop("write.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.stat.trans.csv)){stop("write.stat.trans.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.plotdata.csv))  {stop("write.plotdata.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.quantiles.csv)) {stop("write.quantiles.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.quantiles.trans.csv)){stop("write.quantiles.trans.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.frequency.plot)) {stop("write.frequency.plot must be logical (TRUE/FALSE)")}
   if( !write.frequency.plot.suffix[1] %in% c("pdf","png")){stop("write.frequency.plot.suffix must be pdf or png")}

   if( !is.numeric(csv.nddigits)){      stop("csv.nddigits must be numeric")}
   csv.nddigits = round(csv.nddigits)[1]

   # merge the specified na.rm options with my options
   my.na.rm <- list(na.rm.global=TRUE)
   if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
   my.na.rm[names(na.rm)]<- na.rm
   na.rm <- my.na.rm  # set the na.rm for the rest of the function.

   # Define the log=Pearson III function needed for fitting at the GLOBAL environment level
   dPIII <<-function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)
   pPIII <<-function(q, shape, location, scale) PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
   qPIII <<-function(p, shape, location, scale) PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)

   mPIII <<-function(order, shape, location, scale){
      # compute the empirical first 3 moments of the PIII distribution
      if(order==1) return( location + shape*scale)
      if(order==2) return(scale*scale*shape)
      if(order==3) return(2/sqrt(shape)*sign(scale))
   }

   # Expand the data from the min(date) to max(date) in the dataframe to
   # insert missing values if date was omitted
   min.date <- as.Date(paste(min(start.year,as.numeric(format(min(flow.data$Date,na.rm=TRUE),"%Y"))-water.year),'-',
                             ifelse(water.year,10,1),'-01',sep=""))
   max.date <- as.Date(paste(max(end.year,as.numeric(format(max(flow.data$Date,na.rm=TRUE),"%Y"))),'-',
                             ifelse(water.year,9,12),'-',
                             ifelse(water.year,30,31),sep=""))

   temp <- data.frame(Date=seq(min.date,max.date, 1))
   flow.data <- merge(flow.data, temp, all.y=TRUE)

   # Compute the year and adjust if want to use the water year
   flow.data$Year <- as.numeric(format(flow.data$Date, "%Y")) + water.year*(as.numeric(format(flow.data$Date,"%m"))>=10)

   # Compute the rolling averagw of interest
   flow.data <- flow.data[ order(flow.data$Date),]
   flow.data <- plyr::ldply(roll.avg.days, function (x, flow.data){
        # compute the rolling average of x days
        # create a variable to be attached to the statistic
        flow.data$Measure <- paste("Q", formatC(x, width=3, format="d", flag="0"),"-avg",sep="")
        flow.data$Q       <- zoo::rollapply( flow.data$Q,  x, mean, fill=NA, align="right")
        flow.data
   }, flow.data=flow.data)

   # Truncate the data between the start and end year
   flow.data <- flow.data[ flow.data$Year >=start.year & flow.data$Year <= end.year,]

   # Compute the yearly min (unless max flag is set)
   Q.stat <- plyr::ddply(flow.data, c("Year","Measure"), function(x,use.max=FALSE, na.rm){
       # compute min or max of Q for the year-measure combination
       value <- ifelse(use.max, max(x$Q,na.rm=na.rm$na.rm.global), min(x$Q,na.rm=na.rm$na.rm.global) )
       data.frame(value=value)
   },use.max=use.max, na.rm=na.rm)
   if(nrow(Q.stat)==0){stop("Start.year and end.year eliminated ALL data values")}

   # Compute the summary table for output
   Q.stat.trans <- reshape2::dcast( Q.stat, Year~Measure, value.var="value")

   # See if a (natural) log-transform is to be used in the frequency analysis?
   # This flag also controls how the data is shown in the frequency plot
   if(use.log)Q.stat$value <- log(Q.stat$value)

   # make the plot. Remove any missing or infinite or NaN values
   Q.stat <- Q.stat[ is.finite(Q.stat$value),]  # remove missing/ Inf/ NaN values

   # get the plotting positions
   # From the HEC-SSP package, the  plotting positions are (m-a)/(n+1-a-b)
   a <- 0; b <- 0
   if(prob.plot.position[1]=='weibull'){a <- 0; b <- 0}
   if(prob.plot.position[1]=='median' ){a <-.3; b <-.3}
   if(prob.plot.position[1]=='hazen'  ){a <-.5; b <-.5}
   plotdata <- plyr::ddply(Q.stat, "Measure", function(x,a,b,use.max){
       # sort the data
       x <- x[ order(x$value),]
       x$prob <- ((1:length(x$value))-a)/((length(x$value)+1-a-b))
       if(use.max)x$prob <- 1- x$prob   # they like to use p(exceedance) if using a minimum
       x$dist.prob <- stats::qnorm(1-x$prob)
       x
   }, a=a, b=b, use.max=use.max)
   if(debug)browser()
   # change the measure labels in the plot
   plotdata2<- plotdata
   plotdata2$Measure <- paste(formatC(as.numeric(substr(plotdata2$Measure,2,4)),width=3),"-day Avg",sep="")
   freqplot <- ggplot2::ggplot(data=plotdata2, aes(x=prob, y=value, group=Measure, color=Measure),environment=environment())+
      ggtitle(paste(station.name, " Volume Frequency Analysis"))+
      geom_point()+
      xlab("Probability")+
      scale_x_continuous(trans=scales::probability_trans("norm", lower.tail=FALSE),
                         breaks=prob.scale.points,
                         sec.axis=sec_axis(trans=~1/.,
                                           name='Return Period',
                                           breaks=c(1.01,1.1,2,5,10,20,100,1000),
                                           labels=function(x){ifelse(x<2,x,round(x,0))}))+
      theme(axis.title.x.top = element_text(size=8),
            legend.title=element_blank(), legend.key.size=unit(.1,"in"))

   if(!use.max){ freqplot <- freqplot+theme(legend.justification=c(1,1), legend.position=c(1,1))}
   if( use.max){ freqplot <- freqplot+theme(legend.justification=c(1,0), legend.position=c(1,0))}
   if(!use.log){ freqplot <- freqplot + scale_y_log10(breaks=pretty_breaks(n=20))}
   if( use.log){ freqplot <- freqplot + scale_y_continuous(breaks=pretty_breaks(n=20))}
   if( use.log &  use.max ){freqplot <- freqplot + ylab("ln(Max Flow (cms))")}  # adjust the Y axis label
   if( use.log & !use.max){freqplot <- freqplot + ylab("ln(Min Flow (cms))")}
   if(!use.log &  use.max ){freqplot <- freqplot + ylab("Max Flow (cms)")}
   if(!use.log & !use.max){freqplot <- freqplot + ylab("Min Flow (cms)")}


   # fit the distribution to each measure
   # log-Pearson III implies that the log(x) has a 3-parameter gamma distribution
   ePIII <- function(x,order){
      # compute (centered) empirical centered moments of the data
      if(order==1) return(mean(x))
      if(order==2) return(stats::var(x))
      if(order==3) return(e1071::skewness(x, type=2))
   }

   fit <- plyr::dlply(Q.stat, "Measure", function(x, distr, fit.method){
      start=NULL
      # PIII is fit to log-of values unless use.log has been set, in which case data has previous been logged
      if(distr=='PIII' & !use.log){x$value <- log10(x$value)}
      # get starting values
      if(distr=='PIII'){
         # Note that the above forgot to mulitply the scale by the sign of skewness .
         # Refer to Page 24 of the Bulletin 17c
         m <- mean(x$value)
         v <- stats::var (x$value)
         s <- stats::sd  (x$value)
         g <- e1071::skewness(x$value, type=2)

         # This can be corrected, but HEC Bulletin 17b does not do these corrections
         # Correct the sample skew for bias using the recommendation of
         # Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and Log Pearson Type 3 distributions revisited."
         # Water Resources Reseach 13(2): 427-443, as used by Kite
         #n <- length(x$value)
         #g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)
         # We will use method of moment estimates as starting values for the MLE search

         my.shape <- (2/g)^2
         my.scale <- sqrt(v)/sqrt(my.shape)*sign(g)
         my.location <- m-my.scale*my.shape

         start=list(shape=my.shape, location=my.location, scale=my.scale)
      }
      if(debug)browser()
      if(fit.method=="MLE") {fit <- fitdistrplus::fitdist(x$value, distr, start=start, control=list(maxit=1000)) }# , trace=1, REPORT=1))
      if(fit.method=="MOM") {fit <- fitdistrplus::fitdist(x$value, distr, start=start,
                                    method="mme", order=1:3, memp=ePIII, control=list(maxit=1000))
      } # fixed at MOM estimates
      fit
   }, distr=fit.distr[1], fit.method=fit.distr.method[1])

   if(debug)browser()
   # extracted the fitted quantiles from the fitted distribution
   fitted.quantiles <- plyr::ldply(names(fit), function (measure, prob,fit, use.max, use.log){
      # get the quantiles for each model
      x <- fit[[measure]]
      # if fitting minimums then you want EXCEEDANCE probabilities
      if( use.max) prob <- 1-prob
      quant <- stats::quantile(x, prob=prob)
      quant <- unlist(quant$quantiles)
      if(x$distname=='PIII' & !use.log)quant <- 10^quant # PIII was fit to the log-values
      if(  use.max) prob <- 1-prob  # reset for adding to data frame
      if(  use.log) quant <- exp(quant) # transforma back to original scale
      res <- data.frame(Measure=measure, distr=x$distname, prob=prob, quantile=quant ,stringsAsFactors=FALSE)
      rownames(res) <- NULL
      res
    }, prob=fit.quantiles, fit=fit, use.max=use.max, use.log=use.log)
   if(debug)browser()
   # get the transposed version
   fitted.quantiles$Return <- 1/fitted.quantiles$prob
   fitted.quantiles.trans <- reshape2::dcast(fitted.quantiles, distr+prob+Return~Measure , value.var="quantile")

   file.stat.csv <- NA
   if(write.stat.csv){
     # Write out the summary table for comparison to HEC spreadsheet
     file.stat.csv <- file.path(report.dir,paste(station.name,"-annual-vfa-stat.csv", sep=""))
     temp <- Q.stat
     temp$value <- round(temp$value, csv.nddigits)
     utils::write.csv(temp,file=file.stat.csv, row.names=FALSE)
   }

   file.stat.trans.csv <- NA
   if(write.stat.trans.csv){
     # Write out the  transposed summary table for comparison to HEC spreadsheet
     file.stat.trans.csv <- file.path(report.dir, paste(station.name,"-annual-vfa-stat-trans.csv", sep=""))
     temp <- Q.stat.trans
     temp <- round(temp, csv.nddigits)
     utils::write.csv(temp,file=file.stat.trans.csv, row.names=FALSE)
   }

   file.plotdata.csv <- NA
   if(write.plotdata.csv){
     # Write out the plotdata for comparison with HEC output
     file.plotdata.csv <- file.path(report.dir, paste(station.name,"-annual-vfa-plotdata.csv", sep=""))
     utils::write.csv(plotdata,file=file.plotdata.csv, row.names=FALSE)
   }

   file.quantile.csv <- NA
   if(write.quantiles.csv){
     # Write out the summary table for comparison to HEC spreadsheet
     file.quantile.csv<- file.path(report.dir, paste(station.name,"-annual-vfa-quantiles.csv", sep=""))
     temp <- fitted.quantiles
     temp$quantile <- round(temp$quantile, csv.nddigits)
     utils::write.csv(temp,file=file.quantile.csv, row.names=FALSE)
   }

   file.quantile.trans.csv <- NA
   if(write.quantiles.trans.csv){
     # Write out the  transposed summary table for comparison to HEC spreadsheet
     file.quantile.trans.csv <- file.path(report.dir, paste(station.name,"-annual-vfa-quantiles-trans.csv", sep=""))
     temp <- fitted.quantiles.trans
     temp[,3:ncol(temp)]<- round(temp[,3:ncol(temp)], csv.nddigits)
     utils::write.csv(temp,file=file.quantile.trans.csv, row.names=FALSE)
   }

   file.frequency.plot <- NA
   if(write.frequency.plot){
      file.frequency.plot <- file.path(report.dir, paste(station.name,"-annual-vfa-frequency-plot.",write.frequency.plot.suffix[1],sep=""))
      ggplot2::ggsave(plot=freqplot, file=file.frequency.plot, h=4, w=6, units="in", dpi=300)
   }

   list(start.year=start.year,
        end.year  =end.year,
        water.year=water.year,
        use.max=use.max,
        roll.avg.days=roll.avg.days,
        Q.stat=Q.stat,
        Q.stat.trans=Q.stat.trans,
        plotdata=plotdata,  # has the plotting positions for each point in frequency analysis
        prob.plot.position=prob.plot.position,
        freqplot = freqplot,
        fit.distr=fit.distr[1],  # distributions fit to the data
        fit = fit,               # list of fits of freq.distr to each measure
        fitted.quantiles=fitted.quantiles,             # fitted quantiles and their transposition
        fitted.quantiles.trans=fitted.quantiles.trans,
        file.stat.csv = file.stat.csv,   # file with rolling average statistics
        file.stat.trans.csv=file.stat.trans.csv,
        file.plotdata.csv=file.plotdata.csv, # file with plotting information
        file.quantile.csv=file.quantile.csv,
        file.quantile.trans.csv=file.quantile.trans.csv,
        file.frequency.plot=file.frequency.plot,
        Version = Version,
        Date=Sys.time())
}
