#' Extract Other disease for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .patientkey chr. The column name containing patientkey
#' @param .date chr. The column name containign the date
#' @param .is_pre lgl. Level of the is_pre variable filter. Default is \code{.is_pre = c(TRUE,FALSE)}.
#' @param .days_range numeric. A vector of length selecting the date range around \code{date} to look for medication.
#'
#'
#' @return a data frame containing \code{"disease_category"}, \code{"is_pre"}, \code{"patdiagnosis"}, \code{"n_occurence"}, \code{"range"}.
#' @importFrom dplyr rename count
#' @export
categorize_otherdisease <- function(data, stcs,
                                    .patientkey = "patientkey",
                                    .date = "date",
                                    .is_pre = NULL,
                                    .days_range = c(-Inf, Inf)){

  stopifnot(length(.days_range)<=2L)

  .days_range <- sort(.days_range)

  if(!is.null(.is_pre)){
    categorize_otherdisease_pre(data=data, stcs=stcs,
                                .patientkey=.patientkey,
                                .date=.date,
                                .is_pre=.is_pre ,
                                .days_range=.days_range)


  }else{
    categorize_otherdisease_nopre(data=data, stcs=stcs,
                                .patientkey=.patientkey,
                                .date=.date,
                                .days_range=.days_range)

  }

}


categorize_otherdisease_pre <- function(data, stcs,
                                        .patientkey,
                                        .date,
                                        .is_pre ,
                                        .days_range){

  stopifnot(".is_pre must be lgl:"=is.logical(.is_pre))

  data |>
    select(all_of(c("patientkey" = .patientkey, "inputdate" =.date))) |>
    distinct() |>
    inner_join(
      stcs[["patientdisease"]] |>
        filter(!!sym("disease_category")=="Other") |>
        filter(!!sym("is_pre")%in%.is_pre) |>
        select(all_of(c("patientkey","date","dateaccuracy","disease_category", "is_pre","patdiagnosis"))),
      by = "patientkey",
      relationship = "many-to-many") |>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")<=replace_na(!!sym("inputdate")+days(.days_range[2]),make_date(2999,12,31)))|>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")>=replace_na(!!sym("inputdate")+days(.days_range[1]),make_date(0999,12,31)))|>
    count(!!sym("disease_category"),!!sym("is_pre"),!!sym("patdiagnosis")) |>
    mutate("range" = paste(.date,paste(.days_range,collapse = "/"),"days.",collapse = " ")) |>
    rename("n_occurence" = !!sym("n"))|>
    filter(!is.na(!!sym("patdiagnosis"))) |>
    arrange(!!sym("disease_category"), !!sym("patdiagnosis"))
}

categorize_otherdisease_nopre  <- function(data, stcs,
                                           .patientkey,
                                           .date,
                                           .days_range){
  data |>
    select(all_of(c("patientkey" = .patientkey, "inputdate" =.date))) |>
    distinct() |>
    inner_join(
      stcs[["patientdisease"]] |>
        filter(!!sym("disease_category")=="Other") |>
        select(all_of(c("patientkey","date","dateaccuracy","disease_category","patdiagnosis"))),
      by = "patientkey",
      relationship = "many-to-many") |>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")<=replace_na(!!sym("inputdate")+days(.days_range[2]),make_date(2999,12,31)))|>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")>=replace_na(!!sym("inputdate")+days(.days_range[1]),make_date(0999,12,31)))|>
    count(!!sym("disease_category"),!!sym("patdiagnosis")) |>
    mutate("range" = paste(.date,paste(.days_range,collapse = "/"),"days.",collapse = " ")) |>
    rename("n_occurence" = !!sym("n"))|>
    filter(!is.na(!!sym("patdiagnosis")))|>
    arrange(!!sym("disease_category"), !!sym("patdiagnosis"))

}
