#' Extract infection pathogen species data for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .diseasekey chr. The column name containing diseasekey
#' @param add_infsite lgl. Should the infection site be included for recoding. Default is \code{"no"}. Either \code{c("no","all","other","other+na")}.
#'
#' @return a data frame containing \code{"patdiagnosis"}, \code{"pathogen_species"}, \code{"comment"}, \code{"n_occurence"}.
#'
#' @details
#' when \code{add_infsite = "no"}, the infection site are kept NA's.
#' when \code{add_infsite = "yes"}, the infection site is added for all pathogens.
#' when \code{add_infsite = "other"}, the infection site is added only when \code{"pathogen_species"} is "Other" (eg: one of \code{c("Other bacteria","Other fungi","Other parasites","Other viruses")}).
#' when \code{add_infsite = "other+na"}, the infection site is added only when \code{"pathogen_species"} is "Other" and \code{"comment"} is NA.
#'
#' @export
categorize_pathogenspecies <- function(data, stcs,
                                       .diseasekey = "diseasekey", add_infsite = c("no","all","other","other+na")){

  add_infsite <- match.arg(add_infsite, c("no","all","other","other+na"))

  infpath <- c("Bacteria","Fungi","Parasites","Pathogen unknown","Virus")
  out <-
    data |>
    select(all_of(c("diseasekey" = .diseasekey))) |>
    add_var(stcs,"patdiagnosis",from = "patientdisease",by ="diseasekey")

  if(any(!out$patdiagnosis%in%infpath)){warning("Some .diseasekey are not infections.")}
  out <-
    out |>
    filter(!!sym("patdiagnosis")%in%infpath) |>
    distinct() |>
    inner_join(
      stcs[["infectionpathogen"]] |>
        select(all_of(c("diseasekey","pathogen_species","comment"))),
      by = "diseasekey",
      relationship = "one-to-many")

  if(add_infsite=="no"){
    out <-
      out |>
      mutate("infsite" = NA_character_)
  }else{
    other_path <- c("Other bacteria","Other fungi","Other parasites","Other viruses")
    out <-
      out |>
      left_join(
        stcs[["infectionsite"]] |>
          filter(!!sym("diseasekey")%in%out$diseasekey) |>
          group_by(!!sym("diseasekey")) |>
          summarise("infsite" = paste_valuecomment(!!sym("infsite"),!!sym("comment"))),
        by = "diseasekey",relationship = "many-to-one")
    if(add_infsite=="other"){
      out <-
        out |>
        mutate("infsite" = case_when(!!sym("pathogen_species")%in%other_path~!!sym("infsite"),
                                     TRUE~NA_character_))
    }else if(add_infsite=="other+na"){
      out <-
        out |>
        mutate("infsite" = case_when((!!sym("pathogen_species")%in%other_path)&is.na(!!sym("comment"))~!!sym("infsite"),
                                     TRUE~NA_character_))
    }
  }

  out|>
    count(!!sym("patdiagnosis"),!!sym("pathogen_species"),!!sym("comment"),!!sym("infsite")) |>
    rename("n_occurence" = !!sym("n"))|>
    filter(!((is.na(!!sym("pathogen_species"))&is.na(!!sym("comment"))))) |>
    arrange(!!sym("patdiagnosis"),!!sym("pathogen_species"),!!sym("comment"))

}


