#' Detect if the tables/key can be automatically retrieve
#'
#' @param stcs A list containing the STCS data frame.
#' @param .var chr. The name of variables to add.
#' @param from chr. The name of the dataset (optional)
#'
#'@return A data frame with the new column.
#'@name detect



#'@rdname detect
#'@export
detect_from <- function(stcs, .var){
  from <-
    sapply(.var,\(vi){
      out <- stcs[["variablemetadata"]] |>
        filter(!!sym("variable")==vi) |>
        pull("dataset") |>
        unique() |> tolower()
      if(length(out)==0L){out=NA_character_}
      out}) |>
    unique()

  if(any(is.na(from))){
    stop("Detection of the table failed for variable: ", paste(.var,collapse = " + "),".\n Tables found: ", paste(from,collapse = " + "),".\n At least 1 variable is not in the stcs dataset.")
  }
  if(length(from)>1L){stop("Detection of the table failed for variable: ", paste(.var,collapse = " + "),".\n Tables found: ", paste(from,collapse = " + "),".\n You must select variable from only 1 tables.")}
  from
}

#'@rdname detect
#'@export
#'@importFrom stringr str_split_1
detect_by <- function(stcs,.var, from=NULL){

  meta <- stcs[["variablemetadata"]]
  if(!is.null(from)){
    meta <-
      meta |>
        filter(tolower(!!sym("dataset"))==from)
  }

  if(nrow(meta)==0){stop(from," is not a table in the stcsdataset.")}

  ## return for admin (without key)
  if(from=="admin"){
    return(NULL)
  }

  by <-
    sapply(.var,\(vi){
      out <-
        meta|>
        filter(!!sym("variable")==vi) |>
        pull("dataset_pk") |>
        unique()
      if(length(out)==0L){out=NA_character_}
      out}) |>
    unique()

  if(any(is.na(by))){
    stop("Detection of the mathing keys failed for variable: ", paste(.var,collapse = " + "),".\n Keys found: ", paste(by,collapse = " + "),".\n At least 1 variable is not in the stcs dataset.")
  }
  if(length(by)>1L){
    stop("Detection of the keys failed for variable: ", paste(.var,collapse = " + "),".\n Keys found: ", paste(from,collapse = " + "),".\n You must select variables associated to only 1 key.")}

  str_split_1(by, ", ")
}
