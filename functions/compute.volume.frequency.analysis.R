# Compute the volume frequency analysis - similar to the HEC-SSP program
# The key difference lies in how the PIII distribuition is fit. I use MLE while
# HEC-SSP appears to use method of moments.

compute.volume.frequency.analysis <- function(Station.Code, flow, 
                         start.year=9999, end.year=0000, use.water.year=FALSE, 
                         roll.avg.days=c(1,3,7,30,60,90),
                         use.log=FALSE,
                         use.max=FALSE,
                         prob.plot.position=c("weibull","median","hazen"),
                         prob.scale.points=c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                         fit.distr=c("PIII","weibull"),
                         fit.distr.method=ifelse(fit.distr=="PIII","MOM","MLE"),
                         fit.quantiles=c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
                         na.rm=list(na.rm.global=TRUE),
                         write.stat.csv=FALSE, write.stat.trans.csv=FALSE,
                         write.plotdata.csv=FALSE,  # write out the plotting data
                         write.quantiles.csv=FALSE, # write out the fitted quantiles
                         write.quantiles.trans.csv=FALSE,
                         write.frequency.plot=TRUE,  # write out the frequency plot
                         write.frequency.plot.suffix=c("pdf","png"),
                         report.dir='.', debug=FALSE
                         )
  {
   Version <- '2017-02-01'
   library(ggplot2); library(plyr); 
   library(reshape2); library(zoo); 
   library(PearsonDS); library(e1071)
   # replicate the frequency analysis of the HEC-SSP program
   # refer to Chapter 7 of the user manual

   # Input 
   #   Station.Code - code for station
   #   flow - input data frame with variables
   #      Date  - in R format
   #      Q     - the daily average flow on that date
   #   Missing values are indicated by either using NA in the flow, or omitting the
   #   date entirely from the data frame.
   #
   #   start.year, end.year - range of calendar (or water) years to include in the analysis
   #   use.water.year - are computations to be done on the water year (Oct of prev year to Sept) or 
   #                    calendar year
   #   roll.average.days - number of days in rolling average. If want the daily minimum, use the value of 1
   #   use.log  - should the data be analyzed on the (natural) log scale after the rolling averages are computed
   #   use.max     - compute the yearly maximum rather than yearly minimum
   #   prob.plot.position - type of plotting positions to be used. See HEC-SSP manual for details. The first one specified is used.
   #   prob.scale.points  - points on bottom scale to plot
   #   fit.distr - which distribution to fit to the data
   #   fit.distr.method - method to use when fitting distr (MOM only supported for PIII)
   #   fit.quantiles  - what quantiles are extracted from fitted distribution to the data 
  
  
   #   vars - variables in stat to plot on the same graph using different colors
   #   distr - distribution to plot
   #
   #   Missing values will automatically be deleted
   #
   # Output list with
   #   plot - ggplot object
  
   # data checks
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
   if( !is.logical(use.water.year))  {stop("use.water.year must be logical (TRUE/FALSE")}
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
   if(  fit.distr[1]=='weibull' & any(flow$Q<0)){stop("cannot fit weibull distribution with negative flow values")}
   if(  fit.distr[1]!="PIII" & fit.distr.method[1]=="MOM"){stop('MOM only can be used with PIII distribution')}
   
   if( !is.logical(write.stat.csv))      {stop("write.stat.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.stat.trans.csv)){stop("write.stat.trans.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.plotdata.csv))  {stop("write.plotdata.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.quantiles.csv)) {stop("write.quantiles.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.quantiles.trans.csv)){stop("write.quantiles.trans.csv must be logical (TRUE/FALSE")}
   if( !is.logical(write.frequency.plot)) {stop("write.frequency.plot must be logical (TRUE/FALSE)")}
   if( !write.frequency.plot.suffix[1] %in% c("pdf","png")){stop("write.frequency.plot.suffix must be pdf or png")}
   
   # merge the specified na.rm options with my options
   my.na.rm <- list(na.rm.global=TRUE)
   if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
   my.na.rm[names(na.rm)]<- na.rm
   na.rm <- my.na.rm  # set the na.rm for the rest of the function.

   # Expand the data from the min(date) to max(date) in the dataframe to
   # insert missing values if date was omitted
   min.date <- as.Date(paste(min(start.year,as.numeric(format(min(flow$Date,na.rm=TRUE),"%Y"))-use.water.year),'-',
                             ifelse(use.water.year,10,1),'-01',sep=""))
   max.date <- as.Date(paste(max(end.year,as.numeric(format(max(flow$Date,na.rm=TRUE),"%Y"))),'-',
                             ifelse(use.water.year,9,12),'-',
                             ifelse(use.water.year,30,31),sep=""))
   
   temp <- data.frame(Date=seq(min.date,max.date, 1))
   flow <- merge(flow, temp, all.y=TRUE)
   
   # Compute the year and adjust if want to use the water year
   flow$Year <- as.numeric(format(flow$Date, "%Y")) + use.water.year*(as.numeric(format(flow$Date,"%m"))>=10)
   
   # Compute the rolling averagw of interest
   flow <- flow[ order(flow$Date),]
   flow <- ldply(roll.avg.days, function (x, flow){
        # compute the rolling average of x days
        # create a variable to be attached to the statistic
        flow$Measure <- paste("Q", formatC(x, width=3, format="d", flag="0"),"-avg",sep="")
        flow$Q       <- zoo::rollapply( flow$Q,  x, mean, fill=NA, align="right")
        flow
   }, flow=flow)
   
   # Truncate the data between the start and end year
   flow <- flow[ flow$Year >=start.year & flow$Year <= end.year,]
   
   # Compute the yearly min (unless max flag is set)
   Q.stat <- ddply(flow, c("Year","Measure"), function(x,use.max=FALSE, na.rm){
       # compute min or max of Q for the year-measure combination
       value <- ifelse(use.max, max(x$Q,na.rm=na.rm$na.rm.global), min(x$Q,na.rm=TRUE) )
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
       x$dist.prob <- qnorm(1-x$prob)
       x
   }, a=a, b=b, use.max=use.max)
   if(debug)browser()
   freqplot <- ggplot2::ggplot(data=plotdata, aes(x=prob, y=value, group=Measure, color=Measure),environment=environment())+
      geom_point()+
      xlab("Probability")+
      scale_x_continuous(trans=probability_trans("norm", lower.tail=FALSE), 
                         breaks=prob.scale.points,
                         sec.axis=sec_axis(trans=~1/.,
                                           breaks=c(1.01,1.1,2,5,10,20,100,1000),
                                           labels=function(x){ifelse(x<2,x,round(x,0))}))
      
   if(!use.max){ freqplot <- freqplot+theme(legend.justification=c(1,1), legend.position=c(1,1))}
   if( use.max){ freqplot <- freqplot+theme(legend.justification=c(1,0), legend.position=c(1,0))}
   if(!use.log){ freqplot <- freqplot + scale_y_log10(breaks=function(x){pretty(x)})}
   if( use.log &  use.max ){freqplot <- freqplot + ylab("ln(Max Value)")}  # adjust the Y axis label
   if( use.log & !use.max){freqplot <- freqplot + ylab("ln(Min Value)")}
   if(!use.log &  use.max ){freqplot <- freqplot + ylab("Max Value")}  
   if(!use.log & !use.max){freqplot <- freqplot + ylab("Min Value")}

   
   # fit the distribution to each measure
   # log-Pearson III implies that the log(x) has a 3-parameter gamma distribution 
   ePIII <- function(x,order){
      # compute (centered) empirical centered moments of the data
      if(order==1) return(mean(x))
      if(order==2) return(var(x))
      if(order==3) return(e1071::skewness(x, type=2))
   }
   
   fit <- dlply(Q.stat, "Measure", function(x, distr, fit.method){
      start=NULL
      # PIII is fit to log-of values unless use.log has been set, in which case data has previous been logged
      if(distr=='PIII' & !use.log){x$value <- log10(x$value)} 
      # get starting values
      if(distr=='PIII'){
         # Note that the above forgot to mulitply the scale by the sign of skewness .
         # Refer to Page 24 of the Bulletin 17c
         m <- mean(x$value)
         v <- var (x$value)
         s <- sd  (x$value)
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
   }, distr=fit.distr[1], fit.method=fit.distr.method[1])
  
   # extracted the fitted quantiles from the fitted distribution
   fitted.quantiles <- ldply(names(fit), function (measure, prob,fit, use.max, use.log){
      # get the quantiles for each model
      x <- fit[[measure]]
      # if fitting minimums then you want EXCEEDANCE probabilities
      if( use.max) prob <- 1-prob
      quant <- quantile(x, prob=prob)
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
   fitted.quantiles.trans <- reshape2::dcast(fitted.quantiles, distr+prob~Measure , value.var="quantile")
   
   file.stat.csv <- NA
   if(write.stat.csv){
     # Write out the summary table for comparison to HEC spreadsheet
     file.stat.csv <- file.path(report.dir,paste(Station.Code,"-annual-vfa-stat.csv", sep=""))
     write.csv(Q.stat,file=file.stat.csv, row.names=FALSE)
   }
   
   file.stat.trans.csv <- NA
   if(write.stat.trans.csv){
     # Write out the  transposed summary table for comparison to HEC spreadsheet
     file.stat.trans.csv <- file.path(report.dir, paste(Station.Code,"-annual-vfa-stat-trans.csv", sep=""))
     write.csv(Q.stat.trans,file=file.stat.trans.csv, row.names=FALSE)
   }
   
   file.plotdata.csv <- NA
   if(write.plotdata.csv){
     # Write out the plotdata for comparison with HEC output
     file.plotdata.csv <- file.path(report.dir, paste(Station.Code,"-annual-vfa-plotdata.csv", sep=""))
     write.csv(plotdata,file=file.plotdata.csv, row.names=FALSE)
   }

   file.quantile.csv <- NA
   if(write.quantiles.csv){
     # Write out the summary table for comparison to HEC spreadsheet
     file.quantile.csv<- file.path(report.dir, paste(Station.Code,"-annual-vfa-quantiles.csv", sep=""))
     write.csv(fitted.quantiles,file=file.quantile.csv, row.names=FALSE)
   }
   
   file.quantile.trans.csv <- NA
   if(write.quantiles.trans.csv){
     # Write out the  transposed summary table for comparison to HEC spreadsheet
     file.quantile.trans.csv <- file.path(report.dir, paste(Station.Code,"-annual-vfa-quantiles-trans.csv", sep=""))
     write.csv(fitted.quantiles.trans,file=file.quantile.trans.csv, row.names=FALSE)
   }
   
   file.frequency.plot <- NA
   if(write.frequency.plot){
      file.frequency.plot <- file.path(report.dir, paste(Station.Code,"-frequency-plot.",write.frequency.plot.suffix[1],sep=""))
      ggsave(plot=freqplot, file=file.frequency.plot, h=4, w=6, units="in", dpi=300)
   }
   
   list(start.year=start.year,
        end.year  =end.year,
        use.water.year=use.water.year,
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
        date=Sys.time())
} 
   