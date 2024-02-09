#' Extract infection site data
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .diseasekey chr. The column name containing \code{diseasekey}.
#'
#' @return a data frame containing \code{"infsite"},  \code{"comment"}, \code{"n_occurence"}.
#' @export
categorize_infsite <- function(data, stcs,
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
      stcs[["infectionsite"]] |>
        select(all_of(c("diseasekey","infsite","comment"))),
      by = "diseasekey",
      relationship = "one-to-many") |>
    count(!!sym("infsite"),!!sym("comment")) |>
    rename("n_occurence" = !!sym("n"))|>
    filter(!((is.na(!!sym("infsite"))&is.na(!!sym("comment"))))) |>
    arrange(!!sym("infsite"),!!sym("comment"))

}



