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

#--------------------------------------------------------------
#  compare the R computed quantitles with those from HEC as reported in a *.rpt file
#  Two functions here - one to read the HEC-VFA report file and the second to make the comparison

# Compare the computed frequencey statistics with those from the HEC
# Change Log
#     2017-01-30 CJS First Edition

compare.frequency.with.hec <- function(Station.Code,Q.file.stat,
                                       Q.file.plotdata,
                                       Q.file.quantile,
                                       HEC.filename,
                                       write.comparison.csv=FALSE,
                                       write.plots.pdf=FALSE,
                                       report.dir='.', debug=FALSE){
#  Input
#    Station.Code - prefix for file names
#    Q.file.stat   - file name of csv file containing the frequency statistics
#    Q.file.plotdata     file name of csv file containing the plotting information
#    HEC.filename - HEC VFA rpt file
#
#  Output: List with the following objects
#    stats.in.Q.not.in.E  - statistics in Q but not in E
#    stats.in.E.not.in.Q  - statistics in E but not in Q
#    diff.stat - data frame showing for each statistics the value in Q, the value in E and the
#                proportional difference
#    plot.list - list of plots visualizing the diff.stat
#    stat.not.plotted - list of variables not in any of the plots (should be empty)
#############################################################
#  Some basic error checking on the input parameters
#
   Version <- '2017-01-01'
   if( !is.character(Station.Code))  {stop("Station Code must be a character string.")}
   if(length(Station.Code)>1)        {stop("Station.Code cannot have length > 1")}
   if( !is.character(Q.file.stat))    {stop("Q.file.stat must be a character string.")}
   if( !file.exists(Q.file.stat))     {stop('Q.file.stat does not exist')}
   if(length(Q.file.stat)>1)          {stop("Q.file.stat cannot have length > 1")}
   if( !is.character(Q.file.plotdata))    {stop("Q.file.plotdata must be a character string.")}
   if( !file.exists(Q.file.plotdata))     {stop('Q.file.plotdata does not exist')}
   if(length(Q.file.plotdata)>1)          {stop("Q.file.plotdata cannot have length > 1")}
   if( !is.character(Q.file.quantile))    {stop("Q.file.quantile must be a character string.")}
   if( !file.exists(Q.file.quantile))     {stop('Q.file.quantile does not exist')}
   if(length(Q.file.quantile)>1)          {stop("Q.file.quantile cannot have length > 1")}
   if(length(HEC.filename)>1)        {stop("HEC.filename cannot have length > 1")}
   if( !is.character(HEC.filename))  {stop("HEC.filename must be a character string.")}
   if( !file.exists(HEC.filename))   {stop('HEC.filename does not exist')}

   if(! is.logical(write.comparison.csv)) {stop("write.comparison.csv should be logical")}
   if(! is.logical(write.plots.pdf))      {stop("write.plots.pdf should be logical")}
   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exits")}

   #  Load the packages used
   library(ggplot2)
   library(plyr)

   # Get the computed summary statistics created in another file
   Q.stat <- read.csv(file=Q.file.stat, header=TRUE, as.is=TRUE, strip.white=TRUE)

   # Get the plotting positions and rename to match those from HEC file
   Q.pp   <- read.csv(file=Q.file.plotdata, header=TRUE, as.is=TRUE, strip.white=TRUE)
   Q.pp$Measure <- sub("avg",'pp',Q.pp$Measure)
   Q.pp$value <- Q.pp$prob

   # Get the fitted quantiles. Change the "Year" to the quantile value
   Q.quantile <- read.csv(file=Q.file.quantile,as.is=TRUE, strip.white=TRUE, header=TRUE)
   Q.quantile$Measure <- paste(Q.quantile$Measure,'-q',sep="")
   Q.quantile$Year <- Q.quantile$prob
   Q.quantile$prob <- NULL
   Q.quantile$value <- Q.quantile$quantile
   Q.quantile$quantile <- NULL
   Q.quantile$distr <- NULL

   if(debug)browser()
   Q.stat <- rbind(Q.stat,
                   Q.pp[,c("Year","Measure","value")],
                   Q.quantile[,c("Year","Measure","value")])  # put both together to merge later

   # Get the data from the Excel spreadsheet
   HEC.stat <- read.hec.vfa.rpt(HEC.filename)

   # check the statistcs in the two data frames
   unique(Q.stat$Measure)
   unique(HEC.stat$Measure)

   # which statistics are in Q.stat, but not in E.stat
   stats.in.Q.not.in.HEC <- unique(Q.stat$Measure)[ !unique(Q.stat$Measure) %in% unique(HEC.stat$Measure)]

   # which statistics are in E.stat but not in Q.stat
   stats.in.HEC.not.in.Q <- unique(HEC.stat$Measure)[ !unique(HEC.stat$Measure) %in% unique(Q.stat$Measure)]
   if(debug)browser()
   # Now to compare the results from Q.stat to those in E.stat
   diff.stat <- merge(Q.stat, HEC.stat, by=c("Year","Measure"), suffixes=c(".Q",".HEC"))
   diff.stat$diff <-  diff.stat[,"value.Q"] - diff.stat[,"value.HEC"]
   diff.stat$mean <- (diff.stat[,"value.Q"] + diff.stat[,"value.HEC"])/2
   diff.stat$pdiff  <- abs(diff.stat$diff)/ diff.stat$mean

   # Visualize where any difference lie
   diff.stat[ is.na(diff.stat$pdiff),]
   max(diff.stat$pdiff, na.rm=TRUE)
   min(diff.stat$pdiff, na.rm=TRUE)

   makediffplot <- function (plotdata){
     myplot <- ggplot(data=plotdata, aes(x=Year, y=Measure, size=pdiff))+
       ggtitle(paste(Station.Code, " - Standardized differences between Q.stat and E.Stat",sep=""))+
       theme(plot.title = element_text(hjust = 0.5))+
       geom_point()+
       scale_size_area(limits=c(0,.01), name="Proportional\ndifference")+
       ylab("Variables showing \nProportion of abs(diff) to mean")
     # indicate missing values using X
     if(sum(is.na(plotdata$pdiff))){
        myplot <- myplot + geom_label(data=plotdata[is.na(plotdata$pdiff),], aes(label="X", size=NULL),size=4, fill='red',alpha=0.2)
     }
     myplot
  }

   plotdata <- diff.stat
   plotdata$pdiff <- pmin(.01, plotdata$pdiff)

   #plot.allstat <- makediffplot(plotdata)  # all variables

   # are there any variables not plotted?
   stat.not.plotted <- NA

   set.avg  <- grepl("-avg$", plotdata$Measure)
   plot.avg <- makediffplot(plotdata[ set.avg,])   # only for rolling averages

   set.pp  <- grepl("-pp", plotdata$Measure)
   plot.pp <- makediffplot(plotdata[ set.pp,])   # only for plotting positions

   set.quant  <- grepl("-q$", plotdata$Measure)
   plot.quant <- makediffplot(plotdata[ set.quant,])   # only for quantiles
   # need to adjust the axis labels
   plot.quant <- plot.quant + xlab("Quantile")

   # are there any variables not plotted?
   stat.not.plotted <- unique( plotdata$Measure[ !(set.avg | set.pp | set.quant)])

   plot.list <- list(plot.avg=plot.avg,
                     plot.pp=plot.pp,
                     plot.quant=plot.quant)

   file.comparison.csv <- NA
   if(write.comparison.csv){
      file.comparison.csv <- file.path(report.dir, paste(Station.Code,"-comparison-vfa-R-vs-HEC.csv",sep=""))
      write.csv(diff.stat, file.comparison.csv, row.names=FALSE)
   }

   file.plots.pdf <- NA
   if(write.plots.pdf){
      file.plots.pdf <- file.path(report.dir, paste(Station.Code, "-comparison-vfa-R-vs-HEC.pdf",sep=""))
      pdf(file=file.plots.pdf)
      l_ply(plot.list, function(x){plot(x)})
      dev.off()
   }



   list(stats.in.Q.not.in.HEC=stats.in.Q.not.in.HEC,
        stats.in.HEC.not.in.Q=stats.in.HEC.not.in.Q,
        diff.stat=diff.stat,
        plot.list=plot.list,
        stat.not.plotted=stat.not.plotted,
        file.comparison.csv=file.comparison.csv,
        file.plots.pdf=file.plots.pdf,
        Version=Version,
        Date=Sys.time())
}



# read a HEC-SSP VFA report - groan - in such a bad format
# Note that there may be one or two sections of output for each statistic
#    Preliminary output if outliers are detected
#      followed by final report excluding outliers  - we want the prelimnary report
#    Just a final report if there are no outliers.
#    So the rule of thumb is choose only the first statisic if there are two.
read.hec.vfa.rpt <- function(file){
  # read the hec vfa report file to extract the statistics computed
  hec <- readLines(file)

  # find all of the positions of the << Plotting Positions >>
  where.pp <- grep("<< Plotting Positions >>", hec, fixed=TRUE)

  extract.stat <- ldply(where.pp, function(x, hec){
   # extract the statistics for each group of statistics starting a line x in the hec report file
   #
   stat <- substr(hec[x+1], regexpr("(",hec[x+1],fixed=TRUE)+1, regexpr(")",hec[x+1],fixed=TRUE)-1)
   roll.avg <- as.numeric(substr(stat,1,regexpr("-",stat,fixed=TRUE)-1))
   measure <- paste("Q", formatC(roll.avg, width=3, format="d", flag="0"),"-avg",sep="")
   # which is the start/end row for the actual statistics
   start.row <- x+7
   end.row   <- grep("|----------",hec[-(1:start.row)],fixed=TRUE)[1]+start.row-1

   words <- strsplit(hec[start.row:end.row],' +')
   extract.text <- ldply(words, function (x){
       # date is in positions 2, 3, 4 in d month yyyy format
       Date <- as.Date(paste(x[2:4],collapse="-"),"%d-%b-%Y")
       Year <- as.numeric(format(Date, "%Y"))
       Q    <- as.numeric(x[5])
       data.frame(Year=Year, value=Q, stringsAsFactors=FALSE)
   })
   data.frame(Measure=measure, extract.text, stringsAsFactors=FALSE)
  },hec=hec)
  extract.stat <- extract.stat[ ! duplicated(extract.stat[,c("Year","Measure")]),]

  extract.pp <- ldply(where.pp, function(x, hec){
   # extract the plotting positions for each group of statistics starting a line x in the hec report file
   #
   stat <- substr(hec[x+1], regexpr("(",hec[x+1],fixed=TRUE)+1, regexpr(")",hec[x+1],fixed=TRUE)-1)
   roll.avg <- as.numeric(substr(stat,1,regexpr("-",stat,fixed=TRUE)-1))
   measure <- paste("Q", formatC(roll.avg, width=3, format="d", flag="0"),"-pp",sep="")
   # which is the start/end row for the actual statistics
   start.row <- x+7
   end.row   <- grep("|----------",hec[-(1:start.row)],fixed=TRUE)[1]+start.row-1

   words <- strsplit(hec[start.row:end.row],' +')
   extract.text <- ldply(words, function (x){
       # date is in positions 2, 3, 4 in d month yyyy format
       Year <- as.numeric(x[8])
       pp  <- as.numeric(x[10])/100
       data.frame(Year=Year, value=pp, stringsAsFactors=FALSE)
   })
   data.frame(Measure=measure, extract.text, stringsAsFactors=FALSE)
  },hec=hec)
  extract.pp <- extract.pp[ ! duplicated(extract.pp[,c("Year","Measure")]),] # get first record

  where.quant <- grep("<< Frequency Curve >>", hec, fixed=TRUE)
  extract.quant <- ldply(where.quant, function(x, hec){
   # extract the quantiles for each group of statistics
   stat <- substr(hec[x+1], regexpr("(",hec[x+1],fixed=TRUE)+1, regexpr(")",hec[x+1],fixed=TRUE)-1)
   roll.avg <- as.numeric(substr(stat,1,regexpr("-",stat,fixed=TRUE)-1))
   measure <- paste("Q", formatC(roll.avg, width=3, format="d", flag="0"),"-avg-q",sep="")
   # which is the start/end row for the actual statistics
   start.row <- x+7
   end.row   <- grep("|----------",hec[-(1:start.row)],fixed=TRUE)[1]+start.row-1

   words <- strsplit(hec[start.row:end.row],' +')
   extract.text <- ldply(words, function (x,Measure){
       # date is in positions 2, 3, 4 in d month yyyy format
       quant  <- as.numeric(x[2])
       Year   <- as.numeric(x[5])/100  # use the Year variable to store the probability
       data.frame(Measure=Measure, Year=Year, value=quant, stringsAsFactors=FALSE)
   }, Measure=measure)
   extract.text
  },hec=hec)
  extract.quant <- extract.quant[ ! duplicated(extract.quant[,c("Year","Measure")]),] # get first record

  rbind(extract.stat, extract.pp, extract.quant)
}

# hec.stat <- read.hec.vfa.rpt(HEC.filename)
# head(hec.stat)
# tail(hec.stat)




