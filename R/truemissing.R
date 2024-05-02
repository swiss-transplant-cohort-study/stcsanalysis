#' Confirmed missing values
#'
#' Handles confirmed missing values
#'
#'@param x a vector. Logical are not implemented.
#'
#'@details
#'\code{is_truemissing()}: return a logical
#'\code{truemissing_to_na()}: replace confirmed missing by NA's
#'
#'@return a vector of logical
#'@name truemissing
#'@family truemissing


#'@export
#'@rdname truemissing
is_truemissing <- function(x){
  UseMethod("is_truemissing", x)
}

#'@export
#'@rdname truemissing
#'@importFrom tidyr replace_na
is_truemissing.character <- function(x){
  replace_na(x=="Missing", FALSE)
}

#'@export
#'@rdname truemissing
#'@importFrom lubridate make_date
is_truemissing.Date <- function(x){
  replace_na(x==make_date(1700L, 1L, 1L), FALSE)
}

#'@export
#'@rdname truemissing
is_truemissing.factor <- function(x){
  replace_na(x=="Missing", FALSE)
}

#'@export
#'@rdname truemissing
is_truemissing.integer <- function(x){
  replace_na(x==-9999L, FALSE)
}

#'@export
#'@rdname truemissing
is_truemissing.logical <- function(x){
  replace_na(rep(FALSE, length(x)), FALSE)
}

#'@export
#'@rdname truemissing
is_truemissing.numeric <- function(x){
  replace_na(x==-9999, FALSE)
}

#'@export
#'@rdname truemissing
truemissing_to_na <-function(x){
  x[is_truemissing(x)]<-NA
  x
}

#'@export
#'@rdname truemissing
is_truemissing.POSIXct <- function(x){
  replace_na(x==as.POSIXct("1700-01-01 00:00:00", tz = "Europe/Zurich"), FALSE)
}
