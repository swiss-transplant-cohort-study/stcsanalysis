#' Impute to the last day of the month/year
#'
#' @param date Date. A vector of date.
#' @param dateaccuracy chr. A vector of date accuracy ("Exact date", "Day uncertain", "Day/Month uncertain", "Estimated date")
#'
#' @return a date impute at the last day of the month/year depending on the accuracy.
#' @export
#'
#' @examples
#' impute_lastday(as.Date("2023-1-12"),"Day uncertain")
#'
#' @importFrom dplyr case_when
#' @importFrom lubridate years days make_date
#' @importFrom lubridate year month
impute_lastday <- function(date, dateaccuracy){
  case_when(
    dateaccuracy=="Exact date"~date,
    is.na(dateaccuracy)~date,
    is_truemissing(dateaccuracy)~date,
    dateaccuracy=="Day uncertain"~make_date(year(date),month(date),1L)+months(1L)-days(1L),
    dateaccuracy=="Day/Month uncertain"~make_date(year(date),1L,1L)+years(1L)-days(1L),
    dateaccuracy=="Estimated date"~date)
}
