#' @keywords internal
#' @importFrom stats na.omit
patientkey2donorkey <- function(patientkeys,stcs){
  stcs[["transplantation"]] |>
    select(all_of(c("donorkey","patientkey"))) |>
    filter((!!sym("patientkey"))%in%unique(na.omit(patientkeys))) |>
    pull("donorkey")
}

# soaskey2organkey <- function(soaskeys,stcs){
#   stcs[["organ"]] |>
#     select(all_of(c("soaskey","organkey"))) |>
#     filter((!!sym("soaskey"))%in%unique(na.omit(soaskeys))) |>
#     pull("organkey")
# }
#
# soaskey2donorkey <- function(soaskeys,stcs){
#   stcs[["transplantation"]] |>
#     select(all_of(c("soaskey","donorkey"))) |>
#     filter((!!sym("soaskey"))%in%unique(na.omit(soaskeys))) |>
#     pull("donorkey") |>
#     na.omit() |>
#     unique()
# }
