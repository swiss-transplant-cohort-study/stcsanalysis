#' Tailored analysis tables with main dates related to patients
#'
#'@param stcs A list containing the STCS data frame.
#'
#'@return A data frame.
#'
#'@details
#'
#'
#'\code{tailored_patientsurvival()}: PK: \code{patientkey}.
#'
#'\itemize{
#'
#' \item{1.} \code{patientkey}: Identifier of patients. Source: \code{Patient}.
#' \item{2.} \code{enrollment_date}: Enrollment date. Source: \code{Patient}.
#' \item{3.} \code{deathdate}: Death date. Source: \code{Stop}.
#' \item{4.} \code{first_dropoutdate}: First dropout date. Source: \code{Stop}.
#' \item{5.} \code{first_dropoutdateaccuracy}: Accuracy of the first dropout date. Source: \code{Stop}.
#' \item{6.} \code{last_activedropoutdate}: Last dropout date without \code{backstcs}. Source: \code{Stop}.
#' \item{7.} \code{last_activedropoutdateaccuracy}: Accuracy of the last dropout date without \code{backstcs}. Source: \code{Stop}.
#' \item{8.} \code{last_patlongkeys}: Identifier of the last patient-assessment. Source: \code{PatientLongitudinal}.
#' \item{9.} \code{last_assdate}: Date of the last patient-assessment. Source: \code{PatientLongitudinal}.
#' \item{10.-17.} \code{last_[...]_toggle_date}: Assessment date of the last mentioned filled toggle (keep: \code{"Yes"}, \code{"No"}, removed: \code{"Missing"}, \code{NA}). Source: \code{PatientLongitudinal}.
#' \item{18.} \code{extraction_date}: Extraction date. Source: \code{Admin}.
#' \item{19.} \code{last_complete_patientlongitudinal_crf_date}: Assessment date of the latest \code{crf_status} of the patient assessment defined as \code{"Complete"}. Source: \code{PatientLongitudinal}.
#' \item{20.} \code{last_complete_stop_crf_date}: Date (latest between: \code{deathdate}, \code{dropoutdate}, \code{lastalivedate}, \code{backstcsdate}) of the latest \code{crf_status} defined as \code{"Complete"}. Source: \code{Stop}.
#' \item{21.} \code{last_consent_withdrawal_date}: Last consent date when the its consent status is \code{"Withdrawal"}. Source \code{Patient}.
#' \item{22.} \code{confidental data}: Optional.
#' \item{23.} \code{last_complete_psq_crf_date}: Optional. Date of the latest \code{crf_status} of PSQ defined as \code{"Complete"}. Source: \code{PSQ}.
#'
#'}
#'
#'@name tailored_table
#'@export
#'@importFrom dplyr slice_max slice_min summarise
#'@importFrom tidyr pivot_longer
#'@importFrom tidyselect contains ends_with
tailored_patientsurvival <- function(stcs){

  mendatory_tailored_tables_error(stcs,c("admin","consent","patient","patientlongitudinal","stop"))

  out <-
    stcs[["patient"]] |>
    select(all_of(c("patientkey","enrollment_date"))) |>
    add_var(stcs,.var = "deathdate",from ="stop",by = "patientkey",.filter = !is.na(!!sym("deathdate"))) |>
    left_join(
      stcs[["stop"]] |>
        filter(!is.na(!!sym("dropoutdate"))) |>
        group_by(!!sym("patientkey")) |>
        slice_min(!!sym("dropoutdate")) |>
        select(all_of(c("patientkey","first_dropoutdate"="dropoutdate","first_dropoutdateaccuracy"="dropoutdateaccuracy"))),
      by = "patientkey",relationship = "one-to-one") |>
    add_var(stcs,.var = c("last_activedropoutdate"="dropoutdate","last_activedropoutdateaccuracy"="dropoutdateaccuracy"),
            from ="stop",by = "patientkey",.filter = is.na(!!sym("backstcsdate"))&!is.na(!!sym("dropoutdate"))) |>
    left_join(
      stcs[["patientlongitudinal"]] |>
        select(all_of(c("patientkey","assdate","patlongkey"))) |>
        group_by(!!sym("patientkey")) |>
        slice_max(!!sym("assdate")) |>
        summarise("last_patlongkeys" = paste(!!sym("patlongkey"), collapse = ","),
                  "last_assdate"=unique(!!sym("assdate"))),
      by = "patientkey",relationship = "one-to-one") |>
    left_join(
      stcs[["patientlongitudinal"]] |>
        select(all_of(c("patientkey","assdate")),contains("_toggle")) |>
        mutate(across(contains("_toggle"),truemissing_to_na)) |>
        pivot_longer(ends_with("_toggle"),values_to = "toggle_value",names_to = "toggle", names_pattern = "(.*)_toggle",values_drop_na = T) |>
        group_by(across(all_of(c("patientkey","toggle")))) |>
        summarise(last_date = max(!!sym("assdate")),.groups = "drop") |>
        pivot_wider(values_from = !!sym("last_date"),names_from = !!sym("toggle"),names_glue = "last_{toggle}_toggle_date"),
      by = "patientkey",relationship = "one-to-one") |>
    add_var(stcs,"extraction_date",from = "admin") |>
    left_join(
      stcs[["patientlongitudinal"]] |>
        select(all_of(c("patientkey","assdate","crf_status"))) |>
        filter(!!sym("crf_status")=="Complete") |>
        group_by(!!sym("patientkey")) |>
        summarise("last_complete_patientlongitudinal_crf_date" = max(!!sym("assdate")),.groups = "drop"),
      by = "patientkey",relationship = "one-to-one")|>
    left_join(
      stcs[["stop"]] |>
        mutate("date" = pmax(!!sym("dropoutdate"),!!sym("lastalivedate"),!!sym("deathdate"),!!sym("backstcsdate"),na.rm = T)) |>
        select(all_of(c("patientkey","date","crf_status"))) |>
        filter(!!sym("crf_status")=="Complete") |>
        group_by(!!sym("patientkey")) |>
        summarise("last_complete_stop_crf_date" = max(!!sym("date")),.groups = "drop"),
      by = "patientkey",relationship = "one-to-one")|>
    left_join(
      stcs[["patient"]] |>
        filter(!!sym("last_consent_status")=="Withdrawal") |>
        select(all_of(c("patientkey","last_consent_withdrawal_date"="last_consent_date"))),
      by = "patientkey",relationship = "one-to-one")


  if("dob"%in%colnames(stcs$patient)){
    out <-
      out |>
      add_var(stcs,.var="dob",from="patient",by="patientkey")
  }

  if("psq"%in%names(stcs)){
    out <-
      out |>
      left_join(
        stcs[["psq"]] |>
          select(all_of(c("patientkey","psq_date","crf_status"))) |>
          filter(!!sym("crf_status")=="Complete") |>
          group_by(!!sym("patientkey")) |>
          summarise("last_complete_psq_crf_date" = max(!!sym("psq_date")),.groups = "drop"),
        by = "patientkey",relationship = "one-to-one")
  }

  out

}
