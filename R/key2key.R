#' @keywords internal
#' @importFrom stats na.omit
patientkey2donorkey <- function(patientkeys, stcs){
  stcs[["transplantation"]] |>
    select(all_of(c("donorkey", "patientkey"))) |>
    filter((!!sym("patientkey"))%in%unique(na.omit(patientkeys))) |>
    pull("donorkey") |>
    unique()
}

#' @keywords internal
#' @importFrom stats na.omit
organkey2soaskey <- function(organkeys, stcs){
  stcs[["organ"]] |>
    select(all_of(c("organkey", "soaskey"))) |>
    filter((!!sym("organkey"))%in%unique(na.omit(organkeys))) |>
    pull("soaskey") |>
    unique()
}

#' @keywords internal
#' @importFrom stats na.omit
soaskey2patientkey <- function(soaskeys, stcs){
  stcs[["transplantation"]] |>
    select(all_of(c("patientkey", "soaskey"))) |>
    filter((!!sym("soaskey"))%in%unique(na.omit(soaskeys))) |>
    pull("patientkey") |>
    unique()
}

