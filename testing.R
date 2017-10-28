flow <- read.csv("08NM116 - daily discharge.csv", stringsAsFactors = FALSE)
flow <- dplyr::filter(flow,Parameter=="FLOW")
flow <- dplyr::select(flow,Date,Q=Value)
flow$Date <- as.Date(flow$Date)

#flow$Year  <- lubridate::year(flow$Date)
#flow$MonthNum  <- lubridate::month(flow$Date)
#flow$Month <- month.abb[flow$MonthNum]
#flow$WaterYear <- as.numeric(ifelse(flow$MonthNum>=10,flow$Year+1,flow$Year))

station.name <- "CARN TEST"
flow.data <- flow
start.year <- 1975
end.year <- 2000
water.year <- FALSE
na.rm <- list(na.rm.global=FALSE)
basin.area <- 10
csv.nddigits <- 3
report.dir <- "testing"

#  Compute calendar year long-term stats
Q.month.longterm <-   dplyr::summarize(dplyr::group_by(flow,Month),
                                       Mean = mean(Q,na.rm=TRUE),
                                       Median = median(Q,na.rm=TRUE),
                                       Maximum = max(Q,na.rm=TRUE),
                                       Minimum = min(Q,na.rm=TRUE))
Q.all.longterm <-   dplyr::summarize(flow,
                                     Mean = mean(Q,na.rm=TRUE),
                                     Median = median(Q,na.rm=TRUE),
                                     Maximum = max(Q,na.rm=TRUE),
                                     Minimum = min(Q,na.rm=TRUE))
Q.all.longterm <- dplyr::mutate(Q.all.longterm,Month="Long-term")

Q.longterm <- rbind(Q.month.longterm, Q.all.longterm)
Q.longterm$Month <- factor(Q.longterm$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))
Q.longterm <- with(Q.longterm, Q.longterm[order(Month),])


Q.longterm.trans <- tidyr::gather(Q.longterm,Statistic,Value,-Month)
Q.longterm.trans <- tidyr::spread(Q.longterm.trans,Month,Value)
#gather and spreadf!

file.stat.trans.csv <- NA

long.term <- compute.Q.stat.longterm(
  station.name='Carnation',
  #flow.data=flow,
  HYDAT="08NM116",#,
  #start.year = 1989,
  end.year = 2010,
  water.year = TRUE,
  #write.table=TRUE,         # write out calendar year statistics
  #write.transposed.table=TRUE,   # write out statistics in transposed format
  report.dir='testing',
  #csv.nddigits=3,               # decimal digit for csv files.
  na.rm=list(na.rm.global=TRUE)
  )

  cy <- long.term$Q.stat.longterm



  annual
  annual <- compute.Q.stat.annual(station.name='Carnation-CY',
                                  basin.area=100,
                                  #flow.data=flow,
                                  HYDAT = "08HB048",
                                  water.year = FALSE,
                                  start.year=1975,
                                  end.year=2000,
                                  write.table=TRUE,        # write out statistics on calendar year
                                  write.transposed.table=TRUE,  # write out statistics in transposed format (cy & wy)
                                  write.summary.table=TRUE, # write out a summary of period of record
                                  write.lowflow.table=TRUE,      # write out a summary of low flows
                                  plot.stat.trend=FALSE,        # should you plot all of stat trends?
                                  plot.cumdepart=FALSE,         # plot cumulative departure curves
                                  report.dir="testing",
                                  na.rm=list(na.rm.global=FALSE),
                                  csv.nddigits=3,              # decimal digits for csv files for statistics
                                  debug=FALSE
  )
annual.test <- annual$Q.stat.annual
####### double check the date of half flow dates




daily <- compute.Q.stat.daily(station.name="YAY2",
                              #flow.data=flow,
                              HYDAT="08HB048",
                              #start.year=1975, #not required
                              #end.year=2000, #not required
                              rolling.mean=2,
                              water.year= TRUE, #not required
                              write.table=TRUE,         # write out calendar year statistics
                              #write.transposed.table=FALSE,   # write out statistics in transposed format
                              #write.cumulative.table=FALSE,         # write out calendar year statistics
                              #write.cumulative.transposed.table=FALSE,   # write out statistics in transposed format                              report.dir='testing'#,
                              #csv.nddigits=3,               # decimal digit for csv files.
                              #na.rm=list(na.rm.global=TRUE)
                              report.dir="testing"
                              )
daily.data <- daily$Q.stat.daily

