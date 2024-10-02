#' Return a list of tailored tables
#'
#'@param stcs A list containing the STCS data frame.
#'@param tables chr. Define the name of the requested tailored tables. Default is \code{tables = c("patientbl","patientsurvival","transplantationbl","transplantationsurvival","organsurvival","psq"))}.
#'
#'@return a list of dataframe.
#'
#'@export
tailored_tables <- function(stcs,tables = c("patientbl","patientsurvival","transplantationbl","transplantationsurvival","organsurvival","psq")){
  tables <- match.arg(tables, several.ok = TRUE)

  tailored <-
    tables |>
    sapply(\(chri)eval(parse(text = paste0("tailored_",chri)))) |>
    lapply(\(funi){funi(stcs)})

  names(tailored) <- tables
  tailored

}
