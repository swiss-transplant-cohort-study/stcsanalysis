#' Indicate the index of the row-wise minimal value
#'
#' @param ... Vectors of the same type where which.min is applied.
#' @param silent lgl. Indicate if the function should return a warning in case of duplicated minimal.
#' @param ties chr. Method use in case of ties. Either \code{"warning-first"}, default which return a warning a select the name of the first input, OR \code{"first"}, return the first input, OR \code{"collapse"}, collapse the tied input with \code{" | "}.
#'
#' @return an integer vector indicating the row-wise index of the (first) minimal value
#'
#' @details See original idea https://github.com/radiant-rstats/radiant.data/blob/master/R/radiant.R
#' @name whichp
#' @family whichp



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
      warning("Ties at minimum")
    }
  }
  unname(apply(cbind(...), 1, \(x){
    if(all(is.na(x))){NA}else{which.min(x)}}))

}

#' @export
#' @importFrom rlang dots_list
#' @rdname whichp
which.pmin_chr <- function(...,ties = c("warning-first","first","collapse")){
  ties <- match.arg(ties)

  dots <- dots_list(...,.named = T)
  vn <- names(dots)

  switch(ties,
         "warning-first" ={vn[which.pmin(...,silent=F)]},
         "first"= {vn[which.pmin(...,silent=T)]},
         "collapse" = {
           sapply(apply(cbind(...),1,\(x)which(x==noinf_min(x))),\(xi){
             if(all(is.na(xi))){
               NA_character_
             }else{
               paste(vn[xi],collapse = " | ")
             }
           })
         })
}



