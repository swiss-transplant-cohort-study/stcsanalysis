#' Indicate the index of the row-wise minimal value
#'
#' @param ... Vectors of the same type where which.min is applied.
#' @param silent a logical indicating if the function should return a warning in case of duplicated minimal.
#'
#' @return an integer vector indicating the row-wise index of the (first) minimal value
#'
#' @details See original idea https://github.com/radiant-rstats/radiant.data/blob/master/R/radiant.R
#' @name whichp



#' @export
#' @rdname whichp
which.pmin <- function(...,silent = F){
  if(!silent){
    if(any(apply(cbind(...), 1, \(x){
      if(all(is.na(x))){
        NA}else{
          sum(x==min(x,na.rm=T),na.rm=T)>1
        }
    }),na.rm = T)){
      warning("Duplicated minimum")
    }
  }
  unname(apply(cbind(...), 1, \(x){
    if(all(is.na(x))){NA}else{which.min(x)}}))

}

#' @export
#' @importFrom rlang dots_list
#' @rdname whichp
which.pmin_chr <- function(...,silent = F){
  dots <- dots_list(...,.named = T)
  vn <- names(dots)
  vn[which.pmin(...,silent=silent)]

}




