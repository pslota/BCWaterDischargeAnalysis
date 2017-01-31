
# Compare the computed summary statistics with those from the Excel spreadsheet
# Change Log
#     2017-01-30 CJS First Edition

compare.annual.stat <- function(Q.filename, E.filename, write.comparison.csv=FALSE, write.plots.pdf=FALSE, report.dir){
#  Input
#    Q.filename - file name of csv file containing the annual statistics
#    E.filename - Excel workbook with the statistics
#    write.comparison.csv - save the comparsion file in csv format?
#    write.plots.pdf - should plots comparing the two statistics be saved?
#    report.dir - directory where reports and statistics should be saved
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
   Version <- '2017-02-01'
   if( !is.character(Q.filename))    {stop("Q.filename  muste be a character string.")}
   if( !is.character(E.filename))    {stop("E.filename  muste be a character string.")}
   if( !file.exists(Q.filename))     {stop('Q.filename does not exist')}
   if( !file.exists(E.filename))     {stop('E.filename does not exist')}
   if(length(Q.filename)>1)          {stop("Q.filename cannot have length > 1")}
   if(length(E.filename)>1)          {stop("E.filename cannot have length > 1")}

   if(! is.logical(write.comparison.csv)) {stop("write.comparison.csv should be logical")}
   if(! is.logical(write.plots.pdf))      {stop("write.plots.pdf should be logical")}
   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exits")}

   #  Load the packages used 
   library(ggplot2)
   library(openxlsx)
   library(plyr)

   # Get the computed summary statistics created in another file
   Q.stat <- read.csv(file=Q.filename, header=TRUE, as.is=TRUE, strip.white=TRUE)

   # Get the data from the Excel spreadsheet
   E.stat <- openxlsx::readWorkbook(E.filename, sheet='HydroTrends_Input')

   # check the names in the two data frames
   names(Q.stat)
   names(E.stat)

   # which statistics are in Q.stat, but not in E.stat
   names(Q.stat)[ !names(Q.stat) %in% names(E.stat)]
   # convert SEP to SEPT for daily min and daily max only
   names(Q.stat)[ names(Q.stat) %in% c("SEP_MIN_DAILY_SW", "SEP_MAX_DAILY_SW")]<- c("SEPT_MIN_DAILY_SW", "SEPT_MAX_DAILY_SW")
   # convert TotalQ to TOTALQ
   names(Q.stat)[ names(Q.stat) %in% c('SW_AMJJAS_TOTALQ')]<- c("SW_AMJJAS_TotalQ")
   stats.in.Q.not.in.E <- names(Q.stat)[ !names(Q.stat) %in% names(E.stat)]

   # which statistics are in E.stat but not in Q.stat
   stats.in.E.not.in.Q <- names(E.stat)[ !names(E.stat) %in% names(Q.stat)]

   # Now to compare the results from Q.stat to those in E.stat
   diff.stat <- plyr::ldply( names(Q.stat)[ names(Q.stat) != "Year"], function (stat, Q.stat, E.stat){
     require(plyr) # for rename
     # stat has the name of the column to compare
     Q.values <- Q.stat[, c("Year",stat)]
     E.values <- E.stat[, c("Year",stat)]
     both.values <- merge(Q.values, E.values, by="Year", suffixes=c(".Q",".E"))
     both.values$diff <-  both.values[,paste(stat,".Q",sep="")] - both.values[,paste(stat,".E",sep="")]  
     both.values$mean <- (both.values[,paste(stat,".Q",sep="")] + both.values[,paste(stat,".E",sep="")])/2
     both.values$pdiff  <- abs(both.values$diff)/ both.values$mean 
     both.values$stat <- stat
     names(both.values)[names(both.values) == paste(stat,".Q",sep="")] <- "Value.Q"
     names(both.values)[names(both.values) == paste(stat,".E",sep="")] <- "Value.E"
     both.values[ !(is.na(both.values$Value.Q) & is.na(both.values$Value.E)),]
   }, Q.stat=Q.stat, E.stat=E.stat)

   # Visualize where any difference lie
   diff.stat[ is.na(diff.stat$pdiff),]
   max(diff.stat$pdiff, na.rm=TRUE)
   min(diff.stat$pdiff, na.rm=TRUE)

   makediffplot <- function (plotdata){
     myplot <- ggplot2::ggplot(data=plotdata, aes(x=Year, y=stat, size=pdiff))+
       ggtitle("Standardized differences between Q.stat and E.Stat")+
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

  set.daily  <- grepl("DAILY", plotdata$stat) | grepl("day", plotdata$stat) | grepl("ANNUAL", plotdata$stat)
  plot.daily <- makediffplot(plotdata[ set.daily,])   # only for daily  statistics

  set.totalq   <- grepl("TOTALQ", plotdata$stat)
  set.yieldmm  <- grepl("YIELDMM", plotdata$stat)
  plot.yieldmm <- makediffplot(plotdata[ set.totalq | set.yieldmm,]) 

  set.mean   <- grepl("MEAN", plotdata$stat)
  plot.mean  <- makediffplot(plotdata[ set.mean,]) 

  set.per    <- grepl("P50", plotdata$stat) | grepl("P80", plotdata$stat)| grepl("P90", plotdata$stat) 
  plot.per   <- makediffplot(plotdata[ set.per,]) 

  set.wyear  <- grepl("Oct_to_Sept", plotdata$stat) | grepl("ONDJFM", plotdata$stat) | grepl("AMJJAS", plotdata$stat)
  plot.wyear <- makediffplot(plotdata[ set.wyear,] ) 

  # are there any variables not plotted?
  stat.not.plotted <- unique( plotdata$stat[ !(set.daily | set.totalq | set.yieldmm | set.mean | set.per | set.wyear)])

  plot.list <- list(#plot.allstat=plot.allstat,
                     plot.daily  =plot.daily,
                     plot.yieldmm=plot.yieldmm,
                     plot.mean   =plot.mean,
                     plot.per    =plot.per,
                     plot.wyear  =plot.wyear)
   
   file.comparison <- NA
   if(write.comparison.csv){
      file.comparison.csv <- file.path(report.dir, "comparison-annual-R-vs-Excel.csv")
      write.csv(diff.stat, file.comparison.csv, row.names=FALSE)
   }
   
   file.plots.pdf <- NA
   if(write.plots.pdf){
      file.plots <- file.path(report.dir, "comparison-annual-R-vs-Excel.pdf")
      pdf(file=file.plots.pdf)
      l_ply(plot.list, function(x){plot(x)})
      dev.off()
   }
  

   list(stats.in.Q.not.in.E=stats.in.Q.not.in.E,
        stats.in.E.not.in.Q=stats.in.E.not.in.Q,
        diff.stat=diff.stat,
        plot.list=plot.list,
        stat.not.plotted=stat.not.plotted,
        file.comparison.csv=file.comparison.csv,
        file.plots.pdf=file.plots.pdf,
        Version=Version,
        Date=Sys.time())
}
