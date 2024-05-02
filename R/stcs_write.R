#' Write a list of STCS dataset into CSV file
#'
#' @param dir chr. Path of the directory containing the exported files data. Dir is created if it does not exists.
#' @param stcs a list of STCS datasets
#' @param delim the column delimiting character passed to \code{write_delim()}. Default is ", ".
#' @param progress Function or lgl. Argument pass to \code{write_delim()} Default is FALSE.
#' @param na character string used for missing values. Passed to \code{write_delim()}. Default is "".
#' @param ... other arguments passed to \code{write_delim()}.
#'
#' @name write
#' @family write
#' @examples
#'\dontrun{
#' stcs_write(dir = "path/to/empty/folder", stcs = stcs)
#'}
#'
NULL



#' @export
#' @importFrom readr write_delim
#' @importFrom dplyr bind_rows tibble
#' @importFrom lubridate is.POSIXct
#' @rdname write
stcs_write_csv <- function(stcs, dir, delim = ", ", progress=FALSE, na ="", ...){

  if(!dir.exists(dir)){
    dir.create(dir)
  }
  stopifnot("dir must be empty" = length(list.files(dir))==0L)

  tb_csv <-
    stcs[["variablemetadata"]] |>
    select(all_of(c("dataset"))) |>
    distinct() |>
    mutate("csv"= paste0(!!sym("dataset"), ".csv"),
           "dataset" = tolower(!!sym("dataset"))) |>
    bind_rows(tibble("dataset"="variablemetadata", "csv"="VariableMetadata.csv"))


  for(i in seq_len(nrow(tb_csv))){
    fi <- tb_csv[["csv"]][i]

    write_delim(x = stcs[[tb_csv[["dataset"]][i]]] |>
                  mutate(across(where(is.POSIXct), \(x){format(x, "%Y-%m-%d %H:%M")})),
                file = file.path(dir, fi),
                delim = delim,
                progress = progress,
                na = na)
  }

}



#' @export
#' @importFrom openxlsx openxlsx_getOp openxlsx_setOp createWorkbook addWorksheet writeData saveWorkbook
#' @rdname write
stcs_write_xlsx <- function(stcs, dir){
  op0 <- openxlsx_getOp("openxlsx.dateFormat")

  openxlsx_setOp("openxlsx.dateFormat", "yyyy-mm-dd")

  if(!dir.exists(dir)){
    dir.create(dir)
  }
  stopifnot("dir must be empty" = length(list.files(dir))==0L)

  tb_xlsx <-
    stcs[["variablemetadata"]] |>
    select(all_of(c("dataset"))) |>
    distinct() |>
    mutate(
      "sheet" = !!sym("dataset"),
      "xlsx"= paste0(!!sym("dataset"), ".xlsx"),
      "dataset" = tolower(!!sym("dataset"))) |>
    bind_rows(tibble("dataset"="variablemetadata", "sheet"="VariableMetadata", "xlsx"="VariableMetadata.xlsx"))


  for(i in seq_len(nrow(tb_xlsx))){

    fi <- tb_xlsx[["xlsx"]][i]
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = tb_xlsx[["sheet"]][i])
    writeData(wb, sheet = tb_xlsx[["sheet"]][i], x=stcs[[tb_xlsx[["dataset"]][i]]])

    saveWorkbook(wb, file = file.path(dir, fi), overwrite = TRUE)

  }

  openxlsx_setOp("openxlsx.dateFormat",  op0)



}






