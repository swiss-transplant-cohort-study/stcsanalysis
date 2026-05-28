#' Serology combination
#'
#' Recode the serology recipient-donor into: \code{"D+/R+"}, \code{"D-/R+"}, \code{"D+/R-"}, \code{"D-/R-"} and when serology is Missing/NA: \code{"D+/"}, \code{"D-/"}, \code{"/R+"}, \code{"/R-"}.
#'
#' @param rec_serology chr. Serology of the recipient. Either \code{"Positive"} or \code{"Negative"}.
#' @param don_serology chr. Serology of the Donor. Either \code{"Positive"} or \code{"Negative"}.
#'
#' @return a character vector.
#' @importFrom dplyr case_match
#' @export
serology_combination <- function(rec_serology, don_serology){
  rec_serology <- check_input(rec_serology, c("Positive", "Negative", NA_character_))
  don_serology <- check_input(don_serology, c("Positive", "Negative", NA_character_))

  case_when(is.na(rec_serology) & is.na(don_serology) ~ NA_character_,
            TRUE~paste0(case_match(don_serology, "Positive"~"D+", "Negative"~"D-", NA_character_ ~ ""), "/",
                        case_match(rec_serology, "Positive"~"R+", "Negative"~"R-", NA_character_ ~ "")))
}
