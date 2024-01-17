#' Serology combinaison
#'
#' Recode the serology recipient-donor into: "R+/D+", "R+/D-", "R-/D+", "R-/D-"
#'
#' @param rec_serology chr. Serology of the recipient. Either "Positive" or Negative.
#' @param don_serology chr. Serology of the Donor. Either "Positive" or Negative.
#'
#' @return a character vector.
#' @export
serology_combinaison <- function(rec_serology, don_serology){
  rec_serology <- check_input(rec_serology,c("Positive","Negative",NA_character_))
  don_serology <- check_input(don_serology,c("Positive","Negative",NA_character_))

  case_when(is.na(rec_serology)|is.na(don_serology)~NA_character_,
            TRUE~paste0(case_match(rec_serology,"Positive"~"R+","Negative"~"R-"),"/",case_match(don_serology,"Positive"~"D+","Negative"~"D-")))
}
