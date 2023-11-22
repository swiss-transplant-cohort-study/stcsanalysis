#' Print variables which are available given your current dataset
#'
#' @param stcs list. A list of STCS tables
#' @param data dataframe. A data frame with one or more STCS keys
#'
#' @return Invisible input data frame.
#'
#' @return inputed data
#' @importFrom purrr walk2
#' @export
var_available <- function(data,stcs){
  allkeys <-
    stcs[["variablemetadata"]] |>
    pull("dataset_pk") |>
    na.omit() |>
    unique()

  for(i in seq_along(allkeys)){
    ki_sep <- str_split_1(allkeys[i],pattern = ", ")
    if(all(ki_sep%in%colnames(data))){

      out <-
        stcs[["variablemetadata"]] |>
        filter(!!sym("dataset_pk")==allkeys[i]) |>
        filter(!(!!sym("variable")%in%colnames(data))) |>
        select(all_of(c("dataset","variable"))) |>
        nest("variable"=!!sym("variable")) |>
        mutate("dataset" = tolower(!!sym("dataset")))
      cat("Associate to key:",allkeys[i],"\n")
      walk2(out[["dataset"]],out[["variable"]],\(x,y){
        cat(" - In table:",x,"\n")
        cat(sort(y[["variable"]]),sep = ", ",fill = T,labels="  ")
        cat("\n")

      })
      cat("\n")
    }
  }

  invisible(data)
}
