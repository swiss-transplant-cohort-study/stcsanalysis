#' @keywords internal
#' @importFrom stats na.omit
patientkey2donorkey <- function(patientkeys, stcs){
  stcs[["transplantation"]] |>
    select(all_of(c("donorkey", "patientkey"))) |>
    filter((!!sym("patientkey"))%in%unique(na.omit(patientkeys))) |>
    pull("donorkey")
}

