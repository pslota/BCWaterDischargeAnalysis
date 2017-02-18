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

# Compare the computed summary statistics with those from the Excel spreadsheet
# Change Log
#     2017-01-30 CJS First Edition

compare.annual.stat <- function(Station.Code, Q.filename, E.filename, SW_translate=NULL,
                                write.comparison.csv=FALSE, write.plots.pdf=FALSE, report.dir, debug=FALSE){
#  Input
#    Station.Code - prefix for output files
#    Q.filename - file name of csv file containing the annual statistics
#    E.filename - Excel workbook with the statistics
#    SW_translate - dataframe with equivalents between Q.filename variables and E.filename varibles
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
   Version <- '2017-02-15'
   if( !is.character(Station.Code))  {stop("Station Code must be a character string.")}
   if(length(Station.Code)>1)        {stop("Station.Code cannot have length > 1")}
   if( !is.character(Q.filename))    {stop("Q.filename  muste be a character string.")}
   if( !is.character(E.filename))    {stop("E.filename  muste be a character string.")}
   if( !file.exists(Q.filename))     {stop('Q.filename does not exist')}
   if( !file.exists(E.filename))     {stop('E.filename does not exist')}
   if(length(Q.filename)>1)          {stop("Q.filename cannot have length > 1")}
   if(length(E.filename)>1)          {stop("E.filename cannot have length > 1")}

   if( !is.data.frame(SW_translate)) {stop("SW_translate is not a data frame.")}
   if(! all(c("V.Program","V.Excel") %in% names(SW_translate))){
                                      stop("SW_translate dataframe doesn't contain the variables V.Program and V.Excel.")}

   if(! is.logical(write.comparison.csv)) {stop("write.comparison.csv should be logical")}
   if(! is.logical(write.plots.pdf))      {stop("write.plots.pdf should be logical")}
   if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exits")}

   #  Load the packages used
   library(ggplot2)
   library(openxlsx)
   library(plyr)

   if(debug)browser()
   # Get the computed summary statistics created in another file
   Q.stat <- read.csv(file=Q.filename, header=TRUE, as.is=TRUE, strip.white=TRUE)

   # Get the data from the Excel spreadsheet
   E.stat <- openxlsx::readWorkbook(E.filename, sheet='HydroTrends_Input')

   # convert all variable names to lower case
   names(Q.stat) <- tolower(names(Q.stat))
   names(E.stat) <- tolower(names(E.stat))

   # Rename any column in E.stat according to the SW_translate table
   # If E.stat name is not in the translate table, it is left unchanged
   # We first add the current names in E.stat to the translation table to account
   # for names that don't exist
   SW_translate <- rbind(SW_translate, data.frame(V.Program=names(E.stat), V.Excel=names(E.stat), Comment="",stringsAsFactors=FALSE))
   names(E.stat) <- tolower(SW_translate$V.Program[match( names(E.stat), tolower(SW_translate$V.Excel)) ])

   # check the names in the two data frames
   names(Q.stat)
   names(E.stat)

   # which statistics are in Q.stat, but not in E.stat
   stats.in.Q.not.in.E <- names(Q.stat)[ !names(Q.stat) %in% names(E.stat)]

   # which statistics are in E.stat but not in Q.stat
   stats.in.E.not.in.Q <- names(E.stat)[ !names(E.stat) %in% names(Q.stat)]

   # Now to compare the results from Q.stat to those in E.stat
   # don't forget that all variable names are now lower case
   diff.stat <- plyr::ldply( names(Q.stat)[ names(Q.stat) != "year" & !(names(Q.stat) %in% stats.in.Q.not.in.E)],
                             function (stat, Q.stat, E.stat){
     require(plyr) # for rename
     # stat has the name of the column to compare
     Q.values <- Q.stat[, c("year",stat)]
     E.values <- E.stat[, c("year",stat)]
     both.values <- merge(Q.values, E.values, by="year", suffixes=c(".Q",".E"))
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
     myplot <- ggplot2::ggplot(data=plotdata, aes(x=year, y=stat, size=pdiff))+
       ggtitle(paste(Station.Code, " - Standardized differences between Q.stat and E.Stat",sep=""))+
       theme(plot.title = element_text(hjust = 0.5))+
       geom_point()+
       scale_size_area(limits=c(0,.01), name="Proportional\ndifference")+
       ylab("Variables showing \nProportion of abs(diff) to mean")+xlab("Year")
       # indicate missing values using X
     if(sum(is.na(plotdata$pdiff))){
      myplot <- myplot + geom_label(data=plotdata[is.na(plotdata$pdiff),], aes(label="X", size=NULL),size=4, fill='red',alpha=0.2)
     }
     myplot
  }

  plotdata <- diff.stat
  plotdata$pdiff <- pmin(.01, plotdata$pdiff)

  #plot.allstat <- makediffplot(plotdata)  # all variables

  set.daily  <- grepl("day",    plotdata$stat,   ignore.case=TRUE) |
                grepl("ANNUAL", plotdata$stat,   ignore.case=TRUE)
  plot.daily <- makediffplot(plotdata[ set.daily,])   # only for daily  statistics

  set.totalq   <- grepl("TOTALQ", plotdata$stat,  ignore.case=TRUE)
  set.yieldmm  <- grepl("YIELDMM", plotdata$stat, ignore.case=TRUE)
  plot.yieldmm <- makediffplot(plotdata[ set.totalq | set.yieldmm,])

  set.mean   <- grepl("MEAN", plotdata$stat, ignore.case=TRUE)
  plot.mean  <- makediffplot(plotdata[ set.mean,])

  set.per    <- grepl("P50", plotdata$stat, ignore.case=TRUE) |
                grepl("P20", plotdata$stat, ignore.case=TRUE) |
                grepl("P10", plotdata$stat, ignore.case=TRUE)
  plot.per   <- makediffplot(plotdata[ set.per,])

  set.wyear  <- grepl("Oct_to_Sept", plotdata$stat, ignore.case=TRUE) |
                grepl("ONDJFM",      plotdata$stat, ignore.case=TRUE) |
                grepl("AMJJAS",      plotdata$stat, ignore.case=TRUE)
  plot.wyear <- makediffplot(plotdata[ set.wyear,] )

  set.cumq  <- grepl("CUMQ", plotdata$stat, ignore.case=TRUE)
  plot.cumq <- makediffplot(plotdata[ set.cumq,] )

  # are there any variables not plotted?
  stat.not.plotted <- unique( plotdata$stat[ !(set.daily | set.totalq | set.yieldmm |
                                              set.mean   | set.per    | set.wyear   | set.cumq)])

  plot.list <- list(#plot.allstat=plot.allstat,
                     plot.daily  =plot.daily,
                     plot.yieldmm=plot.yieldmm,
                     plot.mean   =plot.mean,
                     plot.per    =plot.per,
                     plot.wyear  =plot.wyear,
                     plot_cumq   =plot.cumq)

   file.comparison <- NA
   if(write.comparison.csv){
      file.comparison.csv <- file.path(report.dir, paste(Station.Code,"-comparison-annual-R-vs-Excel.csv",sep=""))
      write.csv(diff.stat, file.comparison.csv, row.names=FALSE)
   }

   file.plots.pdf <- NA
   if(write.plots.pdf){
      file.plots.pdf <- file.path(report.dir, paste(Station.Code,"-comparison-annual-R-vs-Excel.pdf",sep=""))
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
