#' Extract drug data for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .diseasekey chr. The column name containing diseasekey
#' @param .date chr. The column name containing the date
#' @param .days_range numeric. A vector of length selecting the date range around \code{date} to look for medication.
#'
#' @return a data frame containing \code{"drug_category"}, \code{"name"}, \code{"comment"}, \code{"code"}, \code{"n_occurence"}, \code{"range"} describing the selected drugs and their occurrence.
categorize_infsite <- function(data, stcs,
                               .diseasekey = "diseasekey",
                               .date = "date",
                               .days_range = c(-Inf, Inf)){

  stopifnot(length(.days_range)<=2L)
  .days_range <- sort(.days_range)

  data |>
    select(all_of(c("diseasekey" = .diseasekey, "inputdate" =.date))) |>
    distinct() |>
    inner_join(
      stcs[["infectionsite"]] |>
        select(all_of(c("diseasekey","infsite","comment"))) |>
        add_var(stcs,.var = c("date","dateaccuracy"),from = "patientdisease", by ="diseasekey"),
      by = "diseasekey",
      relationship = "many-to-many") |>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|(!!sym("date")<=replace_na(!!sym("inputdate")+days(.days_range[2]),make_date(2999,12,31))))|>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|(!!sym("date")>=replace_na(!!sym("inputdate")+days(.days_range[1]),make_date(1,1,1))))|>
    count(!!sym("infsite"),!!sym("comment")) |>
    mutate("range" = paste(.date,paste(.days_range,collapse = "/"),"days.",collapse = " ")) |>
    rename("n_occurence" = !!sym("n"))|>
    filter(!((is.na(!!sym("infsite"))&is.na(!!sym("comment")))))

}



