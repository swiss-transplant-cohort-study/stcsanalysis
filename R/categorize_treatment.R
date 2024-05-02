#' Extract treatment data for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .drug_category chr. The drug category to filter. Several choice between \code{"drug_idpro"}, \code{"drug_ind"}, \code{"drug_is"}, \code{"drug_other"}
#' @param .patientkey chr. The column name containing patientkey
#' @param .startdate chr. The column name containing the starting date.
#' @param .stopdate chr. The column name containing the stop date.
#'
#' @return a data frame containing \code{"drug_category"}, \code{"name"}, \code{"comment"}, \code{"code"}, \code{"n_occurence"}, \code{"range"}.
#' @importFrom dplyr rename count arrange
#' @export
categorize_treatment <- function(data, stcs,
                                 .drug_category = c("drug_idpro", "drug_ind", "drug_is", "drug_other"),
                                 .patientkey = "patientkey",
                                 .startdate = NULL,
                                 .stopdate = NULL){

  .drug_category <- match.arg(.drug_category, several.ok = TRUE)
  check_startstop(data[[.startdate]], data[[.stopdate]])

  data |>
    select(all_of(c("patientkey"=.patientkey, "startdate"=.startdate, "stopdate"=.stopdate))) |>
    distinct() |>
    inner_join(
      stcs[["medicationtreatment"]] |>
        filter(!!sym("drug_category")%in%.drug_category) |>
        select(all_of(c("patientkey", "name", "comment", "code", "drug_category", "medication_startdate", "medication_stopdate", "medication_stopdateaccuracy"))),
      by = "patientkey",
      relationship = "many-to-many") |>
    filter(is.na(!!sym("medication_startdate"))|is_truemissing(!!sym("medication_startdate"))|!!sym("medication_startdate")<=!!sym("stopdate"))|>
    filter(is.na(!!sym("medication_stopdate"))|is_truemissing(!!sym("medication_stopdate"))|impute_lastday(!!sym("medication_stopdate"), !!sym("medication_stopdateaccuracy"))>=!!sym("startdate"))|>
    count(!!sym("drug_category"), !!sym("name"), !!sym("comment"), !!sym("code"), name = "n_occurence") |>
    mutate("range" = paste0(.startdate, " to ", .stopdate)) |>
    filter(!((is.na(!!sym("name"))&is.na(!!sym("comment"))&is.na(!!sym("code"))))) |>
    arrange(!!sym("drug_category"), !!sym("name"), !!sym("comment"))

}



