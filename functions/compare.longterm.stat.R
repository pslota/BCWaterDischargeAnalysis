
# Compare the computed longterm statistics with those from the Excel spreadsheet
# Change Log
#     2017-01-30 CJS First Edition

compare.longterm.stat <- function(Q.filename, E.filename,
                                  report.dir='.',
                                  save.comparison=FALSE,
                                  save.plots=FALSE){
#  Input
#    Q.filename - file name of csv file containing the annual statistics
#    E.filename - Excel workbook with the statistics
#    report.dir - where the csv and any plots are saved
#    save.comparsion - save the comparsion between R and Excel
#    save.plots   save the plots of the comparisons?
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
   if( !is.character(Q.filename))    {stop("Q.filename  muste be a character string.")}
   if( !is.character(E.filename))    {stop("E.filename  muste be a character string.")}
   if( !file.exists(Q.filename))     {stop('Q.filename does not exist')}
   if( !file.exists(E.filename))     {stop('E.filename does not exist')}
   if(length(Q.filename)>1)          {stop("Q.filename cannot have length > 1")}
   if(length(E.filename)>1)          {stop("E.filename cannot have length > 1")}

   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exits")}
   if( !is.logical(save.comparison)) {stop('save.comparison must be logical')}
   if( !is.logical(save.plots))      {stop("save.plots must be logial")}
     
   #  Load the packages used 
   library(ggplot2)
   library(openxlsx)
   library(plyr)

   # Get the computed summary statistics created in another file
   Q.stat <- read.csv(file=Q.filename, header=TRUE, as.is=TRUE, strip.white=TRUE)

   # Get the data from the Excel spreadsheet
   E.stat.in <- readWorkbook(E.filename, sheet='HydroDataSummary',  rows=1:5)
   E.stat.in <- E.stat.in[, 25:38]  # CG1: CT5 but readWorkBook skips blank columns in the first 5 rows

   # Transpose the Excel sheet and convert month names to numbers
   E.stat <-as.data.frame(t(E.stat.in), stringsAsFactors=FALSE)
   E.stat$Month <- row.names(E.stat)
   names(E.stat) <- c("mean","median",'max','min',"Month")
   E.stat <- E.stat[ -1,]
   E.stat$Month <- match( substr(E.stat$Month,1,3), c(month.abb,"Lon"))

   # check the names in the two data frames
   names(Q.stat)
   names(E.stat)

   # which statistics are in Q.stat, but not in E.stat
   stats.in.Q.not.in.E <- names(Q.stat)[ !names(Q.stat) %in% names(E.stat)]

   # which statistics are in E.stat but not in Q.stat
   stats.in.E.not.in.Q <- names(E.stat)[ !names(E.stat) %in% names(Q.stat)]

   # Now to compare the results from Q.stat to those in E.stat
   diff.stat <- ldply( names(Q.stat)[ names(Q.stat) != "Month"], function (stat, Q.stat, E.stat){
      # stat has the name of the column to compare
      Q.values <- Q.stat[, c("Month",stat)]
      E.values <- data.frame(Month=E.stat$Month, stat=as.numeric(as.vector(E.stat[, stat])),stringsAsFactors=FALSE)
      names(E.values)[2] <- stat
      both.values <- merge(Q.values, E.values, by="Month", suffixes=c(".Q",".E"))
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
     myplot <- ggplot(data=plotdata, aes(x=Month, y=stat, size=pdiff))+
       ggtitle("Standardized differences between Q.stat and E.stat")+
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

   plot.allstat <- makediffplot(plotdata)  # all variables

   # are there any variables not plotted?
   stat.not.plotted <- NA

   plot.list <- list(plot.allstat=plot.allstat)
 
   file.comparison <- NA
   if(save.comparison){
      file.comparison <- file.path(report.dir, "comparison-longterm-R-vs-Excel.csv")
      write.csv(diff.stat, file.comparison, row.names=FALSE)
   }
   
   file.plots <- NA
   if(save.plots){
      file.plots <- file.path(report.dir, "comparison-longterm-R-vs-Excel.pdf")
      pdf(file=file.plots)
      l_ply(plot.list, function(x){plot(x)})
      dev.off()
   }
    
   list(stats.in.Q.not.in.E=stats.in.Q.not.in.E,
        stats.in.E.not.in.Q=stats.in.E.not.in.Q,
        diff.stat=diff.stat,
        plot.list=plot.list,
        stat.not.plotted=stat.not.plotted,
        file.comparison=file.comparison,
        file.plots=file.plots,
        Date=Sys.time())
}