#' Tailored analysis tables
#'
#'@param stcs A list containing the STCS data frame.
#'
#'@return A data frame.
#'
#'@details
#'
#'\code{tailored_organ()}: PK: \code{organkey}. Baseline and outcome of organs.
#'
#'@name tail_tbl


#'@export
#'@rdname tail_tbl
tailored_organ <- function(stcs){

  data_organkey(stcs) |>
    add_var(stcs,c("soaskey","organ","tpxdate"),from = "organ",by = "organkey") |>
    add_var(stcs,c("patientkey","tpx","soascaseid"),from = "transplantation",by = "soaskey") |>
    add_var(stcs,c("patid","sex","yob"),from = "patient",by = "patientkey") |>
    select(all_of(c("organkey","soaskey","patientkey","patid","soascaseid","tpxdate","organ","tpx","sex","yob")))

}


#'@export
#'@importFrom stringr str_starts
#'@rdname tail_tbl
tailored_psq <- function(stcs){

  Reduce(\(x,y){left_join(x,y,by = c("psqkey","patientkey"),relationship = "one-to-one")},
         lapply(stcs[str_starts(names(stcs),"psq")],psq_wide))

}


## PRIVATE ----

#' @importFrom tidyr pivot_wider
#' @importFrom dplyr ungroup group_by n across
#' @importFrom tidyselect where
#' @importFrom stringr str_pad
psq_wide <- function(x, .key = c("patientkey","psqkey")){

  stopifnot("The data frame must contains a psqkey." = all(.key %in%colnames(x)))
  stopifnot("The data frame must not contains a variable named rowid."= !".rowid"%in%colnames(x))

  out <-
    x |>
    group_by(across(all_of(.key))) |>
    mutate(".rowid" = seq_len(n())) |>
    ungroup()

  if(max(out[[".rowid"]])==1){
    out |>
      select(-all_of(".rowid"))
  }else{
    out |>
      mutate(".rowid" = as.character(!!sym(".rowid"))) |>
      mutate(".rowid" = str_pad(!!sym(".rowid"),
                                width = max(nchar(!!sym(".rowid"))),
                                pad="0")) |>
      group_by(across(all_of(.key))) |>
      pivot_wider(names_from = !!sym(".rowid"),
                  values_from = -all_of(c(.key,".rowid")),
                  names_glue = "{.value}_{.rowid}") |>
      ungroup() |>
      select(-where(\(x){all(is.na(x))}))

  }

}

