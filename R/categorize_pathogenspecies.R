#' Extract infection pathogen species data for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .diseasekey chr. The column name containing diseasekey
#'
#' @return a data frame containing \code{"patdiagnosis"}, \code{"pathogen_species"}, \code{"comment"}, \code{"n_occurence"}.
#' @export
categorize_pathogenspecies <- function(data, stcs,
                               .diseasekey = "diseasekey"){
  infpath <- c("Bacteria","Fungi","Parasites","Pathogen unknown","Virus")
  out <-
    data |>
    select(all_of(c("diseasekey" = .diseasekey))) |>
    add_var(stcs,"patdiagnosis",from = "patientdisease",by ="diseasekey")

  if(any(!out$patdiagnosis%in%infpath)){warning("Some .diseasekey are not infections.")}
  out |>
    filter(!!sym("patdiagnosis")%in%infpath) |>
    distinct() |>
    inner_join(
      stcs[["infectionpathogen"]] |>
        select(all_of(c("diseasekey","pathogen_species","comment"))),
      by = "diseasekey",
      relationship = "one-to-many") |>
    count(!!sym("patdiagnosis"),!!sym("pathogen_species"),!!sym("comment")) |>
    rename("n_occurence" = !!sym("n"))|>
    filter(!((is.na(!!sym("pathogen_species"))&is.na(!!sym("comment"))))) |>
    arrange(!!sym("patdiagnosis"),!!sym("pathogen_species"),!!sym("comment"))

}



