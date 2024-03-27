#' Extract rejection treatment data for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .biorejkey chr. The column name containing biorejkey
#'
#' @return a data frame containing \code{"drug_category"}, \code{"name"}, \code{"comment"}, \code{"code"}, \code{"n_occurence"}, \code{"range"}.
#' @importFrom dplyr rename count arrange
#' @export
categorize_rejectiontreatment <- function(data, stcs,
                                 .biorejkey = "biorejkey"){



  data |>
    select(all_of(c("biorejkey"=.biorejkey))) |>
    distinct() |>
    inner_join(
      stcs[["rejectiontreatment"]] |>
        select(all_of(c("biorejkey","rjtreat","rjtreat_comment"))),
      by = "biorejkey",
      relationship = "one-to-many") |>
    count(!!sym("rjtreat"),!!sym("rjtreat_comment"),name ="n_occurence")|>
    arrange(!!sym("rjtreat"),!!sym("rjtreat_comment"))

}



