#' Tailored analysis tables with main dates related to transplantation
#'
#'@param stcs A list containing the STCS data frame.
#'
#'@return A data frame.
#'
#'@details
#'
#'\code{tailored_transplantationsurvival()}: PK: \code{soaskey}.
#'
#'\itemize{
#'
#' \item{1.} \code{soaskey}: Identifier of transplantations. Source: \code{Transplantation}.
#' \item{2.} \code{patientkey}: Identifier of patients. Source: \code{Transplantation}.
#' \item{3.} \code{donorkey}: Identifier of patients. Source: \code{Transplantation}.
#' \item{4.} \code{listing_date}: Date of listing. Source: \code{Transplantation}.
#' \item{5.} \code{removal_date}: Date of removal. Source: \code{Transplantation}.
#' \item{6.} \code{soas_tpxdate}: (Latest) Date of transplantation. Source: \code{Transplantation}.
#' \item{7.} \code{next_tpx_tpxdate}: Date of the next transplantation (all \code{tpx}). Source: \code{Transplantation}.
#' \item{8.} \code{next_tpxspecific_tpxdate}: Date of the next transplantation (\code{tpx} specific). Source: \code{Transplantation}.
#' \item{9.} \code{last_complete_biosample_crf_date}: (Optional) Date of latest completed biosample \code{crf_status}. Source: \code{Biosample}.
#'
#' }
#'
#'@export
tailored_transplantationsurvival <- function(stcs){

  mendatory_tailored_tables_error(stcs, c("admin", "organ", "transplantation"))

  out <-
    stcs[["transplantation"]] |>
    select(all_of(c("soaskey", "patientkey", "donorkey", "listing_date", "removal_date", "tpxspecific_order", "tpx_order", "tpx"))) |>
    left_join(
      stcs[["organ"]] |>
        group_by(!!sym("soaskey")) |>
        summarise("soas_tpxdate" = min(!!sym("tpxdate"))),
      by = "soaskey", relationship = "one-to-one")

  out <-
    out |>
    left_join(out |>
                filter(!is.na(!!sym("soas_tpxdate"))) |>
                select(all_of(c("patientkey", "next_tpx_tpxdate"="soas_tpxdate", "tpx_order"))) |>
                mutate("tpx_order" = !!sym("tpx_order")-1L),
              by = c("patientkey", "tpx_order"), relationship = "one-to-one") |>
    left_join(out |>
                filter(!is.na(!!sym("soas_tpxdate"))) |>
                select(all_of(c("patientkey", "next_tpxspecific_tpxdate"="soas_tpxdate", "tpxspecific_order", "tpx"))) |>
                mutate("tpxspecific_order" = !!sym("tpxspecific_order")-1L),
              by = c("patientkey", "tpxspecific_order", "tpx"), relationship = "one-to-one") |>
    select(-all_of(c("tpxspecific_order", "tpx_order", "tpx")))


  if("biosample"%in%names(stcs)){
    out <-
      out |>
      left_join(
        stcs[["biosample"]] |>
          select(all_of(c("soaskey", "sin_date", "crf_status"))) |>
          filter(!!sym("crf_status")=="Complete") |>
          group_by(!!sym("soaskey")) |>
          summarise("last_complete_biosample_crf_date" = max(!!sym("sin_date")), .groups = "drop"),
        by = "soaskey", relationship = "one-to-one")
  }

  out

}
