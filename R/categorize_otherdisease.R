#' Extract Other disease for recoding
#'
#' @param data data.frame. A data frame containing the variables describe in \code{.patientkey} and \code{.date}
#' @param stcs list. The stcs object.
#' @param .patientkey chr. The column name containing patientkey.
#' @param .startdate chr. The column name containing the starting date.
#' @param .stopdate chr. The column name containing the stop date.
#' @param .is_pre lgl. Level of the is_pre variable filter. Default is \code{.is_pre = c(TRUE,FALSE)}.
#'
#'
#' @return a data frame containing \code{"disease_category"}, \code{"is_pre"}, \code{"patdiagnosis"}, \code{"n_occurence"}, \code{"range"}.
#' @importFrom dplyr rename count
#' @export
categorize_otherdisease <- function(data, stcs,
                                    .patientkey = "patientkey",
                                    .startdate = NULL,
                                    .stopdate = NULL,
                                    .is_pre = NULL){

  check_startstop(data[[.startdate]],data[[.stopdate]])


  if(!is.null(.is_pre)){
    categorize_otherdisease_pre(data=data, stcs=stcs,
                                .patientkey=.patientkey,
                                .startdate=.startdate,
                                .stopdate=.stopdate,
                                .is_pre=.is_pre)


  }else{
    categorize_otherdisease_nopre(data=data, stcs=stcs,
                                .patientkey=.patientkey,
                                .startdate=.startdate,
                                .stopdate=.stopdate)

  }

}


categorize_otherdisease_pre <- function(data, stcs,
                                        .patientkey,
                                        .startdate,
                                        .stopdate,
                                        .is_pre){

  stopifnot(".is_pre must be lgl:"=is.logical(.is_pre))

  data |>
    select(all_of(c("patientkey" = .patientkey, "startdate" =.startdate, "stopdate" =.stopdate))) |>
    distinct() |>
    inner_join(
      stcs[["patientdisease"]] |>
        filter(!!sym("disease_category")=="Other") |>
        filter(!!sym("is_pre")%in%.is_pre) |>
        select(all_of(c("patientkey","date","dateaccuracy","disease_category", "is_pre","patdiagnosis"))),
      by = "patientkey",
      relationship = "many-to-many") |>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")<=!!sym("stopdate"))|>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")>=!!sym("startdate"))|>
    count(!!sym("disease_category"),!!sym("is_pre"),!!sym("patdiagnosis"),name = "n_occurence") |>
    mutate("range" = paste0(.startdate," to ",.stopdate)) |>
    filter(!is.na(!!sym("patdiagnosis"))) |>
    arrange(!!sym("disease_category"), !!sym("patdiagnosis"))
}

categorize_otherdisease_nopre  <- function(data, stcs,
                                           .patientkey,
                                           .startdate,
                                           .stopdate){
  data |>
    select(all_of(c("patientkey" = .patientkey, "startdate" =.startdate, "stopdate" =.stopdate))) |>
    distinct() |>
    inner_join(
      stcs[["patientdisease"]] |>
        filter(!!sym("disease_category")=="Other") |>
        select(all_of(c("patientkey","date","dateaccuracy","disease_category","patdiagnosis"))),
      by = "patientkey",
      relationship = "many-to-many") |>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")<=!!sym("stopdate"))|>
    filter(is.na(!!sym("date"))|is_truemissing(!!sym("date"))|!!sym("date")>=!!sym("startdate"))|>
    count(!!sym("disease_category"),!!sym("patdiagnosis"),name = "n_occurence") |>
    mutate("range" = paste0(.startdate," to ",.stopdate)) |>
    filter(!is.na(!!sym("patdiagnosis")))|>
    arrange(!!sym("disease_category"), !!sym("patdiagnosis"))

}
