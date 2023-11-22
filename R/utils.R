#' Assigned names to a vector
#'
#' @param x a vector.
#' @param nm a character vector. Default is \code{NULL}.
#'
#' @details If \code{nm = NULL}, the names are the value of the vector (for the unnamed element.). \code{vec_names} is different from \code{rlang::set_names}: each each element of \code{x}, it assigns the values of \code{x} as its name when its \code{names(x)} is missing (values is \code{""}).
#'
#' @return a named vector.
#' @export
vec_names <- function(x, nm = NULL){
  if(is.null(nm)){
    if(is.null(names(x))){nm <- x}else{nm<- names(x)}
    nm[nm==""] <- x[nm==""]
  }

  names(x) <- nm
  x
}



## PRIVATE ----

#' @keywords internal
check_input <- function(x,...,stop_fun = warning){
  UseMethod("check_input",x)
}


#' @keywords internal
check_input.character <- function(x, level = NULL, stop_fun = warning){
  mc <- match.call()
  if(!is.null(level)){
    if(!all(x%in%level,na.rm=T)){
      stop_fun(paste0("Found ",deparse(mc$x)," different from ",paste(unique(level),collapse=", "),". They are convert into NA's"))
      x[!x%in%level] <- NA_character_
    }

  }
  x
}



#' @keywords internal
check_input.numeric <- function(x, lb = NULL,ub = NULL, stop_fun = warning){
  mc <- match.call()
  if(!is.null(lb)){
    if(any(x<lb,na.rm = T)){
      stop_fun(paste0("Found ",deparse(mc$x),"<",lb))
    }
  }

  if(!is.null(ub)){
    if(any(x>ub,na.rm = T)){
      stop_fun(paste0("Found ",deparse(mc$x),">",ub))
    }
  }
  x
}

#' @keywords internal
toupper_dataset <- function(x,stcs){
  x <- tolower(x)
  uc <- c(stcs$variablemetadata$dataset |> unique(),"VariableMetadata")
  lc <- tolower(uc)
  uc[match(x,lc)]

}

