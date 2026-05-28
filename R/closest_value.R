#' Closest values
#'
#' Compute the closest vlues from target values.
#'
#' @param x num. Input to derived closest values.
#' @param target num. Target values.
#'
#' @export
#'
#' @examples
#'
#' closest_value(1:10, c(0, 5, 10))
#'
closest_value <- function(x, target = round(c(0, 186, 365.25*c(1:25)))){
  target <- sort(target)

  ub <- min(max(which(target <= max(x, na.rm = TRUE)))+1L, length(target))
  lb <- max(min(which(target >= min(x, na.rm = TRUE)))-1L, 1L)

  target <- target[seq(from = lb, to = ub, by = 1L)]

  apply(abs(outer(x, target  , "-")),1,
        \(xi){
          if(all(is.na(xi))){
            NA
          }else{
            c(target)[which.min(xi)]
          }})
}


