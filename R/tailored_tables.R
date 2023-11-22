#' Tailored analysis tables
#'
#'@param stcs A list containing the STCS data frame.
#'
#'@return A data frame.
#'
#'@details
#'
#'\code{tailored_organ()}: PK: \code{organkey}. Baseline and outcome of organs.
#'
#'@name tail_tbl


#'@export
#'@rdname tail_tbl
tailored_organ <- function(stcs){

  data_organkey(stcs) |>
    add_var(stcs,c("soaskey","organ","tpxdate"),from = "organ",by = "organkey") |>
    add_var(stcs,c("patientkey","tpx","soascaseid"),from = "transplantation",by = "soaskey") |>
    add_var(stcs,c("patid","sex","yob"),from = "patient",by = "patientkey") |>
    select(all_of(c("organkey","soaskey","patientkey","patid","soascaseid","tpxdate","organ","tpx","sex","yob")))

}
