#' @param flow Dataframe with two variables (1) \code{Date} with the date as a R date class variable and
#'          (2) \code{Q} a numeric variable with the flow for that day.
#'
#'          Missing values for \code{Q} can be
#'          explicitly represented using \code{NA} with the associated date, or implicitly represented
#'          by excluding the date from the data.frame.
#'
#'          The date range of \code{flow} can be wider than
#'          the \code{start.year} and \code{end.year}. This is useful for statistics on the water year
#'          (starting 1 October of the previous year) when you may wish to include flow data for the latter
#'          part of the year. Similarly, it may be useful for statistics that use a 3, 7, or 30 day
#'          rolling average as this will be missing for the first few days of each year.
#'
#'          A water year runs from 1 October of the previous year to 30 Sept of the current year
#           So the 2001 water year runs from 2000-10-01 to 2001-09-30.
#'
#'          All other variables in the data frame will be ignored. Data does NOT have to be sorted by Date order.
