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




#' Merge value and their associated comment
#'
#' @param value a vector different values.
#' @param comment a character vector of associated comment
#'
#' @return a character vector of length 1, where values and comments are paste, sorted and separated with \code{" | "}
#' @export
#' @examples
#' paste_valuecomment(c("other", "disease A"), c("comment 1", NA))
#' paste_valuecomment(NA, NA)
#'
paste_valuecomment <- function(value, comment){
  comment_ind <- !is.na(comment)
  out <- value
  out[comment_ind] <- paste(value[comment_ind], comment[comment_ind], sep=":")
  out <- sort(unique(out))
  if(length(out)!=0L){
    paste(out, collapse = " | ")
  }else{
    NA_character_
  }
}


#' Return minimum without infinity
#'
#' @param x a vector different values.
#'
#' @return the minimal value. When all inputs are NA. return NA.
#' @importFrom lubridate is.Date NA_Date_
#' @export
#' @examples
#' noinf_min(c(NA, NA))
#' noinf_min(c(NA, 4))
#'
noinf_min <- function(x){
  if(all(is.na(x))){
    if(is.character(x)){
      NA_character_
    }else if(is.Date(x)){
      NA_Date_
    }else if(is.integer(x)){
      NA_integer_
    }else if(is.numeric(x)){
      NA_real_
    }else{
      NA
    }
  }else{
    min(x, na.rm=TRUE)
  }
}



## PRIVATE ----



check_startstop <- function(start, stop, stop_fun = warning){
  if(!all(start<=stop)){
    stop_fun("stop before start.")
  }
}


#' @keywords internal
check_input <- function(x, ..., stop_fun = warning){
  UseMethod("check_input", x)
}


#' @keywords internal
check_input.character <- function(x, level = NULL, stop_fun = warning){
  mc <- match.call()
  if(!is.null(level)){
    if(!all(x%in%level, na.rm = TRUE)){
      stop_fun(paste0("Found ", deparse(mc$x), " different from ", paste(unique(level), collapse=", "), ". They are convert into NA's"))
      x[!x%in%level] <- NA_character_
    }

  }
  x
}



#' @keywords internal
check_input.numeric <- function(x, lb = NULL, ub = NULL, stop_fun = warning){
  mc <- match.call()
  if(!is.null(lb)){
    if(any(x<lb, na.rm = TRUE)){
      stop_fun(paste0("Found ", deparse(mc$x), "<", lb))
    }
  }

  if(!is.null(ub)){
    if(any(x>ub, na.rm = TRUE)){
      stop_fun(paste0("Found ", deparse(mc$x), ">", ub))
    }
  }
  x
}

#' @keywords internal
mendatory_tailored_tables_error <- function(stcs, mendatory_tab){
  if(any(!mendatory_tab%in%names(stcs))){
    stop("The following tables are mendatory: ", paste(mendatory_tab, collapse=", "), ".")
  }
}


#' @keywords internal
toupper_dataset <- function(x, stcs){
  x <- tolower(x)
  uc <- c(unique(stcs$variablemetadata$dataset), "VariableMetadata")
  lc <- tolower(uc)
  uc[match(x, lc)]

}


