#' Filter the row of all tables given the patientkey
#'
#' @param stcs list. A list of STCS tables
#' @param patientkeys chr. A vector of \code{patientkey} to keep.
#'
#' @return a list of stcs filtered tables.
#'
#' @family filter
#' @export
stcs_filter_patientkey <- function(stcs, patientkeys){
  patientkeys <- unique(na.omit(patientkeys))
  donorkeys <- patientkey2donorkey(patientkeys, stcs)

  lapply(stcs, \(x){
    x |>
      filter_stcs_tab("patientkey", patientkeys)|>
      filter_stcs_tab("donorkey", donorkeys)
  })


}
