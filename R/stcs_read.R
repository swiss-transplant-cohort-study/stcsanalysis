#' Read STCS datastcs
#'
#' @param dir chr. The directory with .csv files.
#' @param filename chr. The name of the STCS dataset.
#' @param delim chr. Default is ",". Passed to \code{read_delim}.
#' @param lazy lgl. Default is FALSE. Passed to \code{read_delim}.
#' @param progress lgl. Default is FALSE. Passed to \code{read_delim}.
#' @param na chr. Default is "". Passed to \code{read_delim}.
#' @param col_types Variables type. Passed to \code{read_delim}. See \code{cols}.
#' @param ... other arguments passed to \code{read_delim}.
#'
#' @return a list of stcs tables.
#' @name read
#' @family read
#'@examples
#'\dontrun{
#' stcs <- stcs_read("path/to/stcs/dataset/")
#'}
#'


#' @export
#' @rdname read
#' @importFrom readr cols
#' @importFrom dplyr select mutate
#' @importFrom purrr pmap
#' @importFrom rlang sym
#' @importFrom tidyr nest
#' @importFrom tidyselect all_of
stcs_read <- function(dir, delim=",",lazy = FALSE, progress = FALSE, na = "", ...){

  stopifnot("dir must exists" = dir.exists(dir))

  ct_meta <- cols(.default = col_character())

  variablemetadata <- stcs_read1(dir,"VariableMetaData.csv",col_types= cols(.default = col_character()),delim=delim,lazy=lazy,progress=progress,na=na, ...=...)

  out <-
    variablemetadata |>
    select(all_of(c("dataset","variable","datatype"))) |>
    nest(data=c(!!sym("variable"),!!sym("datatype")))|>
    filter(tolower(!!sym("dataset"))!="variablemetadata") |>
    mutate("data" = pmap(list(!!sym("dataset"),!!sym("data")),\(fi,di){
      ct = get_stcs_coltypes(variable=di$variable, datatype =  di$datatype)
      stcs_read1(dir=dir, filename= paste0(fi,".csv"),col_types  = ct,delim=delim,lazy=lazy,progress=progress,na=na, ...=...)
    }))
  names(out[["data"]]) <- tolower(out[["dataset"]])
  out <- out[["data"]]
  out[["variablemetadata"]] <- variablemetadata
  out

}



#' @export
#' @importFrom listenv listenv
#' @importFrom future %lazy% %<-%
#' @rdname read
stcs_future_read <- function(dir, delim=",",lazy = FALSE, progress = FALSE, na = "", ...){

  stopifnot("dir must exists" = dir.exists(dir))

  ct_meta <- cols(.default = col_character())

  variablemetadata <- stcs_read1(dir,"VariableMetaData.csv",col_types= cols(.default = col_character()),delim=delim,lazy=lazy,progress=progress,na=na, ...=...)

  out <-
    variablemetadata |>
    select(all_of(c("dataset","variable","datatype"))) |>
    nest(data=c(!!sym("variable"),!!sym("datatype")))|>
    filter(tolower(!!sym("dataset"))!="variablemetadata")

  stcs <-listenv()
  for(i in seq_len(nrow(out))){
    ct = get_stcs_coltypes(variable=out$data[[i]]$variable, datatype = out$data[[i]]$datatype)
    stcs[[tolower(out$dataset[i])]] %<-%{stcs_read1(dir=dir, filename= paste0(out$dataset[i],".csv"),col_types  = ct,delim=delim,lazy=lazy,progress=progress,na=na, ...=...)}%lazy% TRUE

  }


  stcs[["variablemetadata"]] <- variablemetadata
  stcs

}





#' @export
#' @rdname read
#' @importFrom readr read_delim
stcs_read1 <- function(dir, filename, col_types=NULL,delim=",",lazy=FALSE,progress=FALSE,na="",...){

  read_delim(file.path(dir,filename),col_types =col_types,delim = delim,lazy=lazy,progress=progress,na=na, ...=...)

}


# PRIVATE ----
#' @importFrom readr col_logical col_integer col_double col_character col_date col_datetime
get_stcs_coltypes <- function(variable, datatype){
  stopifnot("arguments should have the same length" = length(variable)==length(datatype))
  out <- datatype
  names(out) = variable
  lapply(out,\(dti){
    switch(dti,
           "Logical" = {col_logical()},
           "Integer" = {col_integer()},
           "Double" = {col_double()},
           "Character" = {col_character()},
           "Date" = {col_date(format = "%Y-%m-%d")},
           "Date-Time" = {col_datetime(format = c("%Y-%m-%d %H:%M"))})}) |>
    do.call(what="cols")
}
