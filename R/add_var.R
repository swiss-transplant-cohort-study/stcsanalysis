#' Generic function to add columns
#'
#' @param data A data frame with a STCS key (\code{by}).
#' @param stcs A list containing the STCS data frame.
#' @param from chr. The name of a dataset in the stcs object
#' @param .var chr. The name of variables to add. It can be a named vector (to rename) of length >1.
#' @param by chr. A named vector used the matching. Argument passed to \code{left_join}. \code{names(by)} matches variable in data and \code{by} matches variable in \code{stcs$from}.
#' @param .filter an optional argument which can be use to filter \code{stcs$from} before matching.
#' @param relationship chr. Argument passed to \code{left_join}.
#'
#'@return A data frame with the new column.
#'@name add_var
#'@family add_var
#'@examples
#'\dontrun{
#' stcs <- stcs_read("path/to/stcs/dataset/")
#'
#' data_patientkey(stcs) |>
#'    add_var(stcs,"sex")
#'
#' data_patientkey(stcs) |>
#'   expand_var_from(stcs,"patlongkey",from="patientlongitudinal","patientkey") |>
#'   add_var(stcs,"bmi")
#'}


#'@rdname add_var
#'@export
add_var <- function(data, stcs, .var, from = detect_from(stcs,.var),  by = detect_by(stcs,.var,from), .filter){
  mc <- match.call()


  if(is.null(mc[["from"]])){
    mc[["from"]] <- from
  }

  if(is.null(mc[["by"]])&!is.null(by)){
    mc[["by"]] <- by
  }
  mc[["relationship"]]="many-to-one"


  mc[[1]] <- quote(stcsanalysis::new_var_from)
  eval(mc, envir = parent.frame())



}


#'@rdname add_var
#'@export
expand_var_from <- function(data, stcs, .var,from, by, .filter){
  mc <- match.call()
  mc[["relationship"]]="many-to-many"
  mc[[1]] <- quote(stcsanalysis::new_var_from)
  eval(mc, envir = parent.frame())
}


#'@importFrom dplyr is_grouped_df left_join distinct if_any
#'@importFrom rlang set_names enquo syms
#'@export
#'@rdname add_var
new_var_from <- function(data, stcs, .var, from,  by, .filter, relationship){

  mc <- match.call()


  ## check is grouped
  stopifnot("'data' must NOT be grouped. Use dplyr::ungroup()." = !is_grouped_df(data))

  ## check dataset exist
  if(is.null(stcs[[from]])){stop(paste0(from," must be a dataset in 'stcs'."))}


  ## .var
  .var <- vec_names(.var)

  ## check variable exist
  if(!all(.var%in%colnames(stcs[[from]]))){stop(paste0(.var," must be a variable in 'stcs[[",from,"]]'."))}

  ## check variable
  if(any(names(.var)%in%colnames(data))){stop(paste0(.var,"/names(...) must be NOT be in 'data'."))}

  ## return admin (special without by)
  if(from=="admin"){
    data[[names(.var)]] = stcs[["admin"]][[.var]]
    return(data)
  }


  ## by
  by <- vec_names(by)

  ## check by
  if(!all(by%in%colnames(stcs[[from]]))){stop(paste0(by," must be a variable in 'stcs[[",from,"]]'."))}
  if(!all(names(by)%in%colnames(data))){stop(paste0(by,"/names(...) must be a variable in 'data'."))}



  if(is.null(mc[[".filter"]])){
    data|>
      left_join(stcs[[from]]|>
                  select(all_of(c(unname(by),.var)))|>
                  filter(!if_any(c(!!!syms(unname(by))),is.na)) |>
                  #filter(!is.na(!!sym(unname(by)))) |>
                  distinct(),
                by = by,
                relationship = relationship)
  }else{
    .filter <- enquo(.filter)
    data|>
      left_join(stcs[[from]]|>
                  filter(!!.filter)|>
                  select(all_of(c(unname(by),.var)))|>
                  filter(!if_any(c(!!!syms(unname(by))),is.na)) |>
                  #filter(!is.na(!!sym(unname(by)))) |>
                  distinct(),
                by = by,
                relationship = relationship)
  }

}



