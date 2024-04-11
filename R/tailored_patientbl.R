#' Tailored analysis tables with baseline patient data.
#'
#'@param stcs A list containing the STCS data frame.
#'
#'@return A data frame.
#'
#'@details
#'
#'\code{tailored_patientbl()}: PK: \code{patientkey}.
#'
#'\itemize{
#' \item{1.} \code{patientkey}: Identifier of patients. Source: \code{Patient}.
#' \item{2.} \code{patid}: STCS Identifier of patients. Source: \code{Patient}.
#' \item{3.} \code{enrollment_age}: Patient age at enrollment. Source: \code{Patient}.
#' \item{4.} \code{sex}: Patient sex. Source: \code{Patient}.
#' \item{5.} \code{bg}: Patient blood group. Source: \code{Patient}.
#' \item{6.} \code{ethnicity}: Patient ethnicity. Source: \code{Patient}.
#' \item{7.} \code{ethnicity_comment}: Comment on \code{patient_ethnicity}. Source: \code{Patient}.
#' \item{8.} \code{pre_tpx_count}: Number of pre-STSC transplantation. Source: \code{Patient}.
#' \item{9.} \code{pre_organ_count}: Number of pre-STSC organ. Source: \code{Patient}.
#' \item{10.} \code{weight}: Weight of patient at enrollment. Source: \code{PatientLongitudinal}.
#' \item{11.} \code{height}: Height of patient at enrollment. Source: \code{PatientLongitudinal}.
#' \item{12.-26.} \code{hla...}: (Optional) Patient HLA. Source: \code{PatientHLA}.
#' }
#'
#'@export
tailored_patientbl <- function(stcs){

  mendatory_tailored_tables_error(stcs,c("patient","patientlongitudinal"))

  out <-
    stcs[["patient"]] |>
    select(all_of(c("patientkey","patid","enrollment_age","sex","bg","ethnicity","ethnicity_comment","pre_tpx_count","pre_organ_count"))) |>
    left_join(
      stcs[["patientlongitudinal"]] |>
        filter(!!sym("type")=="bl") |>
        select(all_of(c("patientkey","weight","height"))),
      by = "patientkey",relationship = "one-to-one")

  if("patienthla"%in%names(stcs)){
    out |>
      left_join(stcs[["patienthla"]], by = "patientkey",relationship = "one-to-one")

  }

  out

}
