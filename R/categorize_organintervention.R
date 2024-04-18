#' Extract interventions data for recoding
#'
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .organkey chr. The column name containing organkey
#' @param .startdate chr. The column name containing the starting date.
#' @param .stopdate chr. The column name containing the stop date.
#'
#' @details the date of the organ-event is use when the intervention date is NA.
#'
#' @return a data frame containing \code{"txint"}, \code{"txint_comment"},, \code{"organevent"} \code{"organevent_comment"}, \code{"n_occurence"}, \code{"range"}.
#' @importFrom dplyr rename count arrange
#' @export
categorize_organintervention <- function(data, stcs,
                                  .startdate = NULL,
                                  .stopdate = NULL,
                                  .organkey = "organkey"){


  data |>
    select(all_of(c("organkey"=.organkey,"startdate" = .startdate, "stopdate" =.stopdate))) |>
    distinct() |>
    inner_join(
      stcs[["organintervention"]] |>
        select(all_of(c("organkey","txintkey","organeventkey","date", "txint_comment","txint"))) |>
        add_var(stcs,c("organevent","organevent_date","organevent_dateaccuracy","organevent_comment"),from = "organevent",by="organeventkey") |>
        mutate("dateaccuracy" = replace(!!sym("organevent_dateaccuracy"),!is.na(!!sym("date")),NA_character_),
               "date" = case_when(is.na(!!sym("date"))~!!sym("organevent_date"),
                                  TRUE~!!sym("date"))) |>
        select(all_of(c("organkey","txintkey","organeventkey","date","dateaccuracy", "txint","txint_comment","organevent","organevent_comment"))),
      by = "organkey",
      relationship = "one-to-many") |>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")<=!!sym("stopdate"))|>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|impute_lastday(!!sym("date"),!!sym("dateaccuracy"))>=!!sym("startdate"))|>
    count(!!sym("txint"),!!sym("txint_comment"),!!sym("organevent"),!!sym("organevent_comment"),name ="n_occurence")|>
    mutate("range" = paste0(.startdate," to ",.stopdate)) |>
    arrange(!!sym("txint"),!!sym("txint_comment"))

}



