#' Tailored analysis tables
#'
#'@param stcs A list containing the STCS data frame.
#'@param silent lgl. Specify if warning should be returned.
#'
#'@return A data frame.
#'
#'@details
#'
#'
#'\code{tailored_psq()}: PK: \code{patlongkey}. All PSQ forms in wide format.
#'
#'@export
#'@importFrom stringr str_starts
tailored_psq <- function(stcs, silent = FALSE){


  optional_tab <- c("psqadherence", "psqdaysleep", "psqeducation", "psqequvas",
                    "psqquol", "psqsleep", "psqtrust", "psq2exercise", "psq2profession",
                    "psq2workcap", "psq3activity", "psq3household", "psq3occupation",
                    "psq3sport")

  mendatory_tailored_tables_error(stcs,"psq")


  if(any(!optional_tab%in%names(stcs))&!silent){
    warning("tailored_psq() is not complete. You will missing data from ",paste(optional_tab[!optional_tab%in%names(stcs)],collapse=", "))
  }

  Reduce(\(x,y){left_join(x,y,by = c("patlongkey","patientkey"),relationship = "one-to-one")},
         lapply(stcs[str_starts(names(stcs),"psq")],psq_wide))

}


## PRIVATE ----

#' @importFrom tidyr pivot_wider
#' @importFrom dplyr ungroup group_by n across
#' @importFrom tidyselect where
#' @importFrom stringr str_pad
psq_wide <- function(x, .key = c("patientkey","patlongkey")){

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
