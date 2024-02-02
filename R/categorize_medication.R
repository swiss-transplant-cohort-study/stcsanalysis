#' Extract drug data for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .drug_category chr. The drug category to filter. Several choice between \code{"drug_idpro"}, \code{"drug_ind"}, \code{"drug_is"}, \code{"drug_other"}
#' @param .patientkey chr. The column name containing patientkey
#' @param .date chr. The column name containign the date
#' @param .days_range numeric. A vector of length selecting the date range around \code{date} to look for medication.
#'
#' @return a data frame containing \code{"drug_category"}, \code{"name"}, \code{"comment"}, \code{"code"}, \code{"n_occurence"}, \code{"range"} describing the selected drugs and their occurrence.
#' @importFrom dplyr rename count arrange
#' @export
categorize_medication <- function(data, stcs,
                            .drug_category = c("drug_idpro", "drug_ind", "drug_is", "drug_other"),
                            .patientkey = "patientkey",
                            .date = "date",
                            .days_range = c(-Inf, Inf)){

  .drug_category <- match.arg(.drug_category,
                              c("drug_idpro", "drug_ind", "drug_is", "drug_other"),
                              TRUE)

  stopifnot(length(.days_range)<=2L)
  .days_range <- sort(.days_range)

  data |>
    select(all_of(c("patientkey" = .patientkey, "date" =.date))) |>
    distinct() |>
    inner_join(
      stcs[["medicationtreatment"]] |>
        filter(!!sym("drug_category")%in%.drug_category) |>
        select(all_of(c("patientkey","name","comment","code","drug_category","medication_startdate","medication_stopdate","medication_stopdateaccuracy"))),
      by = "patientkey",
      relationship = "many-to-many") |>
    filter(is.na(!!sym("medication_startdate"))|is_truemissing(!!sym("medication_startdate"))|!!sym("medication_startdate")<=replace_na(date+days(.days_range[2]),make_date(2999,12,31)))|>
    filter(is.na(!!sym("medication_startdate"))|is_truemissing(!!sym("medication_startdate"))|impute_lastday(!!sym("medication_stopdate"),!!sym("medication_stopdateaccuracy"))>=replace_na(date+days(.days_range[1]),make_date(1,1,1)))|>
    count(!!sym("drug_category"),!!sym("name"),!!sym("comment"),!!sym("code")) |>
    mutate("range" = paste(.date,paste(.days_range,collapse = "/"),"days.",collapse = " ")) |>
    rename("n_occurence" = !!sym("n"))|>
    filter(!((is.na(!!sym("name"))&is.na(!!sym("comment"))&is.na(!!sym("code"))))) |>
    arrange(!!sym("drug_category"),!!sym("name"),!!sym("comment"))

}



