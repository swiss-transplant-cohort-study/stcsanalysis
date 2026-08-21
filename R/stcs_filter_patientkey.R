#' Filter the row of all tables given the patientkey
#'
#' @param stcs list. A list of STCS tables
#' @param patientkeys chr. A vector of \code{patientkey} to keep.
#'
#' @return a list of stcs filtered tables.
#'
#' @family filter
#' @name stcs_filter

#'@rdname stcs_filter
#' @export
stcs_filter_patientkey <- function(stcs, patientkeys){
  patientkeys <- unique(na.omit(patientkeys))

  pk_warn <- patientkeys[!patientkeys %in% stcs[["patient"]][["patientkey"]]]
  if(length(pk_warn)>0L){
    warning("patientkey missing in stcs$patient: ",paste(pk_warn,collapse = ", "))
  }

  donorkeys <- patientkey2donorkey(patientkeys, stcs)

  lapply(stcs, \(x){
    x |>
      filter_stcs_tab("patientkey", patientkeys)|>
      filter_stcs_tab("donorkey", donorkeys)
  })


}

#'@rdname stcs_filter
#' @export
stcs_filter_organkey <- function(stcs, organkeys){
  organkeys <- unique(na.omit(organkeys))

  ok_warn <- organkeys[!organkeys %in% stcs[["organ"]][["organkey"]]]
  if(length(ok_warn)>0L){
    warning("organkeys missing in stcs$organ: ", paste(ok_warn,collapse = ", "))
  }

  soaskeys <- organkey2soaskey(organkeys, stcs)
  patientkeys <- soaskey2patientkey(soaskeys, stcs)
  donorkeys <- patientkey2donorkey(patientkeys, stcs)

  lapply(stcs, \(x){
    x |>
      filter_stcs_tab("patientkey", patientkeys)|>
      filter_stcs_tab("donorkey", donorkeys)|>
      filter_stcs_tab("organkey", organkeys)|>
      filter_stcs_tab("soaskeys", soaskeys)
  })


}



