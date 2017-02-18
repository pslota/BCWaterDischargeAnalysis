#'@param na.rm List of logical values indicating how to deal with missing values.
#'        \code{na.rm$na.rm.global} control if missing values are to be globally excluded (TRUE) or propogated (FALSE).
#'        If missing values are to be propogated, then any statistic computed over a missing value is also set
#'        to missing. For example, if a flow value was missing on 2014-01-03, then any statistics for January of
#'        2014, JFM of 2014, calendar year 2014, water year 2014, 3 day rolling average of 1, 2, 3 of January 2014
#'        and all statistics that depend on those rolling averages, will also be set to missing.
