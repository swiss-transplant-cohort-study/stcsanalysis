#' Validate the VariableMetadata
#'
#' @param stcs a list containing the STCS dataset.
#'
#' @return a warining with variable mismatches or the \code{stcs} object.
#'
#' @importFrom tidyr unnest
#' @importFrom dplyr  full_join
#'
#' @export
validate_metadata <- function(stcs){

  out <-
    tibble("dataset_l"= names(stcs[tolower(names(stcs))!="variablemetadata"]),
          "variable" = lapply(stcs[tolower(names(stcs))!="variablemetadata"],colnames)) |>
    unnest(!!sym("variable")) |>
    mutate("in_dataset" = TRUE) |>
    full_join(
      stcs[["variablemetadata"]] |>
        select(all_of(c("dataset","variable"))) |>
        mutate("dataset_l" = tolower(!!sym("dataset")),
               "in_metadata" = TRUE),
      by = c("dataset_l","variable")) |>
    select(-all_of(c("dataset_l"))) |>
    filter(!(!!sym("in_dataset")&!!sym("in_metadata")))

  if(nrow(out)>0L){
    warning("STCS dataset and variable metadata does not matches")
    return(out)
  }

  stcs

}
