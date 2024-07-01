#' Tailored analysis tables with main dates related to organs.
#'
#'@param stcs A list containing the STCS data frame.
#'
#'@return A data frame.
#'
#'@details
#'
#'\code{tailored_organsurvival()}: PK: \code{organkey}.
#'
#'\itemize{
#' \item{1.} \code{patientkey}: Identifier of patients. Source: \code{Organ}.
#' \item{2.} \code{donorkey}: Identifier of donors. Source: \code{Organ}.
#' \item{3.} \code{soaskey}: Identifier of transplantation. Source: \code{Organ}.
#' \item{4.} \code{organkey}: Identifier of organs. Source: \code{Organ}.
#' \item{5.} \code{tpxdate}: Date of transplantation. Source: \code{Organ}.
#' \item{6.} \code{glo_date}: Graft loss date. Source: \code{Graftloss}.
#' \item{7.} \code{pnf_date}: Date of PNF (\code{tpxdate} when \code{dgf} is \code{"PNF"}). Source: \code{Organ}.
#' \item{8.} \code{next_tpxdate}: Next \code{tpxdate} (\code{tpxdate} with \code{organ_order}+1)
#' \item{9.} \code{next_organspecific_tpxdate}: Next \code{tpxdate} of the same organ (\code{tpxdate} with \code{organspecific_order}+1)
#' \item{10.-18.} \code{last_[...]_toggle_date}: Assessment date of the last mentioned filled toggle (keep: \code{"Yes"}, \code{"No"}, removed: \code{"Missing"}, \code{NA}). Source: \code{OrganLongitudinal}.
#' \item{19.} \code{last_complete_organlongitudinal_crf_date}: Assessment date of the latest \code{crf_status} of the organ assessment defined as \code{"Complete"}. Source: \code{OrganLongitudinal}.
#' }
#
#'
#'@export
#'@importFrom stringr fixed
tailored_organsurvival <- function(stcs){

  mendatory_tailored_tables_error(stcs, c("admin", "organ", "organlongitudinal", "graftloss"))


  out <-
    stcs[["organ"]] |>
    select(all_of(c("patientkey", "donorkey", "soaskey", "organkey", "tpxdate","organ","organ_order","organspecific_order"))) |>
    add_var(stcs, .var = c("glo_date"="date"), from = "graftloss", by = "organkey") |>
    add_var(stcs, .var = c("pnf_date"="date"), from = "graftloss", by = "organkey", .filter = !!sym("pnfpgd_ind"))


  out <-
    out |>
    left_join(out |>
                select(all_of(c("patientkey", "next_tpxdate"="tpxdate", "organ_order"))) |>
                mutate("organ_order" = !!sym("organ_order")-1L),
              by = c("patientkey", "organ_order"), relationship = "one-to-one") |>
    left_join(out |>
                select(all_of(c("patientkey", "next_organspecific_tpxdate"="tpxdate", "organspecific_order", "organ"))) |>
                mutate("organspecific_order" = !!sym("organspecific_order")-1L),
              by = c("patientkey", "organspecific_order", "organ"), relationship = "one-to-one") |>
    select(-all_of(c("organspecific_order", "organ_order", "organ")))

  out <-
    out |>
    left_join(
      stcs[["organlongitudinal"]] |>
        select(all_of(c("organkey", "assdate")), ends_with(fixed("_toggle"))) |>
        mutate(across(contains("_toggle"), truemissing_to_na)) |>
        pivot_longer(ends_with("_toggle"), values_to = "toggle_value", names_to = "toggle", names_pattern = "(.*)_toggle", values_drop_na = TRUE) |>
        group_by(across(all_of(c("organkey", "toggle")))) |>
        summarise(last_date = max(!!sym("assdate")), .groups = "drop") |>
        pivot_wider(values_from = !!sym("last_date"), names_from = !!sym("toggle"), names_glue = "last_{toggle}_toggle_date"),
      by = "organkey", relationship = "one-to-one") |>
    left_join(
      stcs[["organlongitudinal"]] |>
        select(all_of(c("organkey", "assdate", "crf_status"))) |>
        filter(!!sym("crf_status")=="Complete") |>
        group_by(!!sym("organkey")) |>
        summarise("last_complete_organlongitudinal_crf_date" = max(!!sym("assdate")), .groups = "drop"),
      by = "organkey", relationship = "one-to-one")



  out


}
