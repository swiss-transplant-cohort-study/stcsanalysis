#' Extract organ event data for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .organevent_category chr. The event category to filter. Several choice between \code{"Allograftdisease"}, \code{"Complications"}, \code{"Underlyingdisease"}
#' @param .organkey chr. The column name containing organkey
#' @param .startdate chr. The column name containing the starting date.
#' @param .stopdate chr. The column name containing the stop date.
#'
#' @return a data frame containing \code{"organevent_category"}, \code{"organevent"}, \code{"organevent_comment"}, \code{"n_occurence"}, \code{"range"}.
#' @importFrom dplyr rename count arrange
#' @export
categorize_organevent <- function(data, stcs,
                                  .organevent_category = c("Allograftdisease","Complications","Underlyingdisease"),
                                  .startdate = NULL,
                                  .stopdate = NULL,
                                  .organkey = "organkey"){


  .organevent_category <- match.arg(.organevent_category,
                                    c("Allograftdisease","Complications","Underlyingdisease"),
                                    TRUE)
  data |>
    select(all_of(c("organkey"=.organkey,"startdate" = .startdate, "stopdate" =.stopdate))) |>
    distinct() |>
    inner_join(
      stcs[["organevent"]] |>
        select(all_of(c("organkey","organevent_category","organevent","organevent_date","organevent_dateaccuracy","organevent_comment"))) |>
        filter(!!sym("organevent_category")%in%.organevent_category),
      by = "organkey",
      relationship = "one-to-many") |>
    filter(is.na(!!sym("organevent_date"))|is_truemissing(!!sym("organevent_date"))|!!sym("organevent_date")<=!!sym("stopdate"))|>
    filter(is.na(!!sym("organevent_date"))|is_truemissing(!!sym("organevent_date"))|impute_lastday(!!sym("organevent_date"),!!sym("organevent_dateaccuracy"))>=!!sym("startdate"))|>
    count(!!sym("organevent_category"),!!sym("organevent"),!!sym("organevent_comment"),name ="n_occurence")|>
    mutate("range" = paste0(.startdate," to ",.stopdate)) |>
    arrange(!!sym("organevent_category"),!!sym("organevent"),!!sym("organevent_comment"))

}



