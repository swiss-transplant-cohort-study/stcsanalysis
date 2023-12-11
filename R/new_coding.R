#' Recode a variables given the input from a dataframe
#'
#' @param ... (named) vector.
#' @param data a data frame contains the recoding.
#' @param .new_var the names of the variable in the recoding data contains the new recoding.
#'
#' @return a vector
#'
#' @details
#' When \code{...} is named the names must match column in \code{data}.
#' when \code{...} is unnamed, the variables variable in \code{...} and \code{data} are match in order. If \code{data} contains more variables than specified in \code{...} (+1), the functions stop.
#'
#' If \code{data} contains duplicated values in the subset of column used for the recoding, they are removed and a warning is printed.
#'
#'
#' @export
#' @importFrom dplyr anti_join
new_coding <- function(...,data,.new_var = "new_var"){

  dotargs <- list(...)

  stopifnot(".new_var must be in the recoding data." = any(.new_var%in%names(data)))

  ## standardize unnamed to all ""
  if(all(is.null(names(dotargs)))){
    names(dotargs) = rep("",length(dotargs))
  }


  ## add names if unnamed
  if(""%in%names(dotargs)){
    if(length(dotargs)+1L!=ncol(data)){
      stop("The size of the input and the recoding data must match or be named.")
    }

    ## renames dotargs
    nl <- names(data)
    torename <- names(dotargs)==""

    nl <- nl[!nl%in%c(names(dotargs),.new_var)]
    names(dotargs)[torename] <- nl

  }

  stopifnot("The input names should all be in the recoding data" = all(names(dotargs)%in%names(data)))

  stopifnot("The input names should not be the output vector" = !any(.new_var==names(dotargs)))


  dotargs <- data.frame(dotargs)


  ## Clean recoding dataframe
  data <- data[c(names(dotargs),.new_var)]

  dups <- duplicated(data)

  if(any(dups)){
    warning("Duplicates in the recoding data frame are removed.")
    data <- data[!dups,,drop=FALSE]
  }

  ## check if join will create NA's
  match_na <- anti_join(dotargs,data[,names(dotargs),drop=F], by = names(dotargs))
  if(nrow(match_na)>0L){
    warning("Recoding data is not complete for all input, NA's are created in the recoding.")
  }

  (dotargs |>
    left_join(data, by = names(dotargs),
              relationship = "many-to-one"))[[.new_var]]

}


