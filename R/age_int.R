#' Calculate the age in years between 2 dates
#'
#' @param from Date. Birthday.
#' @param to Date. Day when age is computed.
#'
#' @return The number of full years between the 2 dates.
#' @export
#'
#' @importFrom lubridate as.period interval years
#' @importFrom dplyr case_when
#' @importFrom tidyr replace_na
#'@name age


#'@rdname age
#' @export
age_int <- function(from, to){

  as.integer(as.period(interval(from, to))%/%years(1L))

}


#'@rdname age
#' @export
age_months <- function(from, to){

  as.integer(as.period(interval(from, to))%/%months(1L))

}




