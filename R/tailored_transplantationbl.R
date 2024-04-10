#' Tailored analysis tables with with baseline transplantation data.
#'
#'@param stcs A list containing the STCS data frame.
#'
#'@return A data frame.
#'
#'@details
#'
#'\code{tailored_transplantationbl()}: PK: \code{soaskey}.
#'
#'
#'\itemize{
#'
#' \item{1.} \code{soaskey}: Identifier of transplantations. Source: \code{Transplantation}.
#' \item{2.} \code{patientkey}: Identifier of patients. Source: \code{Transplantation}.
#' \item{3.} \code{donorkey}: Identifier of donors. Source: \code{Transplantation}.
#' \item{4.} \code{organ_count}: Number of organs in the tpx. Source: \code{Transplantation}.
#' \item{5.} \code{tpx}: List of organs. Source: \code{Transplantation}.
#' \item{6.} \code{tpx_resec}: transplantation level Re, Sec. Source: \code{Transplantation}.
#' \item{7.} \code{pre_tpxspecific_count}: Number of pre-transplantation (\code{tpx} specific). Source: \code{Transplantation}.
#' \item{8.} \code{tpxspecific_order}: Order of transplantion (\code{tpx} specific). Source : \code{Transplantation}.
#' \item{9.} \code{tpx_order}: Order of transplantion. Source: \code{Transplantation}.
#' \item{10.} \code{tpx_stcsorder}: Order of transplantion within the STCS. Source: \code{Transplantation}.
#' \item{11.} \code{abocomp}: Blood groups compatibility. Source: \code{Transplantation}.
#' \item{12.-18.} \code{hla...mismatch}: HLA mismatch. Source: \code{Transplantation}.
#' \item{19.} \code{sumhlamismatch}: Sum of mismatch. Source: \code{Transplantation}.
#' \item{20.} \code{hlaavailable}: Number of HLA mismatch available. Source: \code{Transplantation}.
#' \item{21.} \code{procedure_provider}: Procedure provider. Source: \code{Transplantation}.
#' \item{22.-32.} \code{rec_anti...}: (Optional) Recipient Serology. Source: \code{PatientSerology}.
#' \item{33.-43.} \code{don_anti...}: (Optional) Donor Serology. Source: \code{DonorSerology}.
#' \item{44.-54.} \code{combi_anti...}: (Optional) Serology combination. Source: derived.
#'
#' }
#'
#'@name tailored_table
#'@export
tailored_transplantationbl <- function(stcs){

  mendatory_tailored_tables_error(stcs,c("transplantation"))
  out <-
    stcs[["transplantation"]] |>
    select(
      all_of(c("soaskey","patientkey","donorkey","organ_count","tpx","tpx_resec","pre_tpxspecific_count","tpxspecific_order",
               "tpx_order","tpx_stcsorder","abocomp","hlaamismatch","hlabmismatch","hlacmismatch","hladpbmismatch","hladqbmismatch",
               "hladrb1mismatch","hladrb35mismatch","sumhlamismatch","hlaavailable","procedure_provider")))


  if("patientserology"%in%names(stcs)){
    out <-
      out |>
      left_join(
        stcs[["patientserology"]] |>
          select(all_of(c("soaskey","test","result"))) |>
          pivot_wider(names_from = "test",values_from = "result",names_prefix = "rec_"),
        by = "soaskey", relationship = "one-to-one")

  }


  if("donorserology"%in%names(stcs)){
    out <-
      out |>
      left_join(
        stcs[["donorserology"]] |>
          pivot_wider(names_from = "test",values_from = "result",names_prefix = "don_"),
        by = "donorkey", relationship = "many-to-one")

  }

  if(all(c("donorserology","patientserology")%in%names(stcs))){

    out <-
      out |>
      left_join(
        stcs[["patientserology"]] |>
          left_join(stcs[["donorserology"]] |>
                      select(all_of(c("donorkey","test","don_result"="result"))),
                    by=c("donorkey", "test"), relationship = "many-to-one") |>
          mutate("combi" = serology_combination(truemissing_to_na(!!sym("result")), truemissing_to_na(!!sym("don_result")))) |>
          select(all_of(c("soaskey", "test", "combi"))) |>
          pivot_wider(names_from = "test", values_from = "combi", names_prefix = "combi_"),
        by = "soaskey", relationship = "one-to-one")
  }

  out



}
