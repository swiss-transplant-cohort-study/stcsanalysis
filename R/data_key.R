#' Get STCS key
#'
#' @param stcs list. A list of STCS tables
#'
#' @return A data.frame with the requested key.
#' @name data_key
#' @family data_key
#' @examples
#'\dontrun{
#' stcs <- stcs_read("path/to/stcs/dataset/")
#'
#' data_patientkey(stcs)
#'
#' data_organkey(stcs)
#'
#' data_soaskey(stcs)
#'}


#' @export
#' @rdname data_key
data_patientkey <- function(stcs){
  stcs[["patient"]] |>
    select(all_of("patientkey"))
}

#' @export
#' @rdname data_key
data_organkey <- function(stcs){
  stcs[["organ"]] |>
    select(all_of("organkey"))
}

#' @export
#' @rdname data_key
data_soaskey <- function(stcs){
  stcs[["transplantation"]] |>
    select(all_of("soaskey"))
}
