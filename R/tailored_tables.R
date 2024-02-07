#' Tailored analysis tables
#'
#'@param stcs A list containing the STCS data frame.
#'@param silent lgl. Specify if warning should be returned.
#'
#'@return A data frame.
#'
#'@details
#'
#'\code{tailored_organ()}: PK: \code{organkey}. Baseline and outcome of organs.
#'\code{tailored_patientsurvival()}: PK: \code{patientkey}. Main date for survival of the patients.
#'\code{tailored_psq()}: PK: \code{patlongkey}. All PSQ forms in wide format.
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
#'@rdname tail_tbl
#'@importFrom dplyr slice_max slice_min summarise
#'@importFrom tidyr pivot_longer
#'@importFrom tidyselect contains ends_with
tailored_patientsurvival <- function(stcs){

  mendatory_tailored_tables_error(stcs,c("admin","consent","patient","patientlongitudinal","stop"))

  out <-
    stcs[["patient"]] |>
    select(all_of(c("patientkey","enrollment_date"))) |>
    add_var(stcs,.var = "deathdate",from ="stop",by = "patientkey",.filter = !is.na(!!sym("deathdate"))) |>
    left_join(
      stcs[["stop"]] |>
        filter(!is.na(!!sym("dropoutdate"))) |>
        group_by(!!sym("patientkey")) |>
        slice_min(!!sym("dropoutdate")) |>
        select(all_of(c("patientkey","first_dropoutdate"="dropoutdate","first_dropoutdateaccuracy"="dropoutdateaccuracy"))),
      by = "patientkey",relationship = "one-to-one") |>
    add_var(stcs,.var = c("last_activedropoutdate"="dropoutdate","last_activedropoutdateaccuracy"="dropoutdateaccuracy"),
            from ="stop",by = "patientkey",.filter = is.na(!!sym("backstcsdate"))&!is.na(!!sym("dropoutdate"))) |>
    left_join(
      stcs[["patientlongitudinal"]] |>
        select(all_of(c("patientkey","assdate","patlongkey"))) |>
        group_by(!!sym("patientkey")) |>
        slice_max(!!sym("assdate")) |>
        summarise("last_patlongkeys" = paste(!!sym("patlongkey"), collapse = ","),
                  "last_assdate"=unique(!!sym("assdate"))),
      by = "patientkey",relationship = "one-to-one") |>
    left_join(
      stcs[["patientlongitudinal"]] |>
        select(all_of(c("patientkey","assdate")),contains("_toggle")) |>
        mutate(across(contains("_toggle"),truemissing_to_na)) |>
        pivot_longer(ends_with("_toggle"),values_to = "toggle_value",names_to = "toggle", names_pattern = "(.*)_toggle",values_drop_na = T) |>
        group_by(across(all_of(c("patientkey","toggle")))) |>
        summarise(last_date = max(!!sym("assdate")),.groups = "drop") |>
        pivot_wider(values_from = !!sym("last_date"),names_from = !!sym("toggle"),names_glue = "last_{toggle}_toggle_date"),
      by = "patientkey",relationship = "one-to-one") |>
    add_var(stcs,"extraction_date",from = "admin") |>
    left_join(
      stcs[["patientlongitudinal"]] |>
        select(all_of(c("patientkey","assdate","crf_status"))) |>
        filter(!!sym("crf_status")=="Complete") |>
        group_by(!!sym("patientkey")) |>
        summarise("last_complete_patientlongitudinal_crf_date" = max(!!sym("assdate")),.groups = "drop"),
      by = "patientkey",relationship = "one-to-one")|>
    left_join(
      stcs[["stop"]] |>
        mutate("date" = pmax(!!sym("dropoutdate"),!!sym("lastalivedate"),!!sym("deathdate"),!!sym("backstcsdate"),na.rm = T)) |>
        select(all_of(c("patientkey","date","crf_status"))) |>
        filter(!!sym("crf_status")=="Complete") |>
        group_by(!!sym("patientkey")) |>
        summarise("last_complete_stop_crf_date" = max(!!sym("date")),.groups = "drop"),
      by = "patientkey",relationship = "one-to-one")|>
    left_join(
      stcs[["consent"]] |>
        filter(!!sym("consent_status")=="Withdrawal") |>
        group_by(!!sym("patientkey")) |>
        summarise("fist_consent_withdrawal_date" = min(!!sym("consent_date")),.groups = "drop"),
      by = "patientkey",relationship = "one-to-one")


  if("dob"%in%colnames(stcs$patient)){
    out <-
      out |>
      add_var(stcs,.var="dob",from="patient",by="patientkey")
  }

  if("psq"%in%names(stcs)){
    out <-
      out |>
      left_join(
        stcs[["psq"]] |>
          select(all_of(c("patientkey","psq_date","crf_status"))) |>
          filter(!!sym("crf_status")=="Complete") |>
          group_by(!!sym("patientkey")) |>
          summarise("last_complete_psq_crf_date" = max(!!sym("psq_date")),.groups = "drop"),
        by = "patientkey",relationship = "one-to-one")
  }

  out

}

#'@export
#'@importFrom stringr fixed
#'@rdname tail_tbl
tailored_organsurvival <- function(stcs){

  mendatory_tailored_tables_error(stcs,c("admin","organ","organlongitudinal","graftloss"))


  out <-
    stcs[["organ"]] |>
    select(all_of(c("patientkey","donorkey","soaskey","organkey","tpxdate"))) |>
    add_var(stcs,.var = c("glo_date"="date"),from = "graftloss",by = "organkey") |>
    add_var(stcs,.var = c("pnf_date"="tpxdate"),from = "organ",by = "organkey",.filter = !!sym("dgf")=="PNF")|>
    left_join(
      stcs[["organlongitudinal"]] |>
        select(all_of(c("organkey","assdate")),ends_with(fixed("_toggle"))) |>
        mutate(across(contains("_toggle"),truemissing_to_na)) |>
        pivot_longer(ends_with("_toggle"),values_to = "toggle_value",names_to = "toggle", names_pattern = "(.*)_toggle",values_drop_na = T) |>
        group_by(across(all_of(c("organkey","toggle")))) |>
        summarise(last_date = max(!!sym("assdate")),.groups = "drop") |>
        pivot_wider(values_from = !!sym("last_date"),names_from = !!sym("toggle"),names_glue = "last_{toggle}_toggle_date"),
      by = "organkey",relationship = "one-to-one")

  out


}



#'@export
#'@rdname tail_tbl
tailored_transplantationsurvival <- function(stcs){

  mendatory_tailored_tables_error(stcs,c("admin","organ","transplantation"))

  out <-
    stcs[["transplantation"]] |>
    select(all_of(c("soaskey","patientkey","donorkey","listing_date","removal_date","tpxspecific_order","tpx_order","tpx"))) |>
    left_join(
      stcs[["organ"]] |>
        group_by(!!sym("soaskey")) |>
        summarise("soas_tpxdate" = min(!!sym("tpxdate"))),
      by = "soaskey", relationship = "one-to-one")

  out <-
    out |>
    left_join(out |>
                filter(!is.na(!!sym("soas_tpxdate"))) |>
                select(all_of(c("patientkey","next_tpx_tpxdate"="soas_tpxdate","tpx_order"))) |>
                mutate("tpx_order" = !!sym("tpx_order")-1L),
              by = c("patientkey","tpx_order"),relationship = "one-to-one") |>
    left_join(out |>
                filter(!is.na(!!sym("soas_tpxdate"))) |>
                select(all_of(c("patientkey","next_tpxspecific_tpxdate"="soas_tpxdate","tpxspecific_order","tpx"))) |>
                mutate("tpxspecific_order" = !!sym("tpxspecific_order")-1L),
              by = c("patientkey","tpxspecific_order","tpx"),relationship = "one-to-one") |>
    select(-all_of(c("tpxspecific_order", "tpx_order", "tpx")))


  if("biosample"%in%names(stcs)){
    out <-
      out |>
      left_join(
        stcs[["biosample"]] |>
          select(all_of(c("soaskey","sin_date","crf_status"))) |>
          filter(!!sym("crf_status")=="Complete") |>
          group_by(!!sym("soaskey")) |>
          summarise("last_complete_biosample_crf_date" = max(!!sym("sin_date")),.groups = "drop"),
        by = "soaskey",relationship = "one-to-one")
  }

  out

  }


#'@export
#'@importFrom stringr str_starts
#'@rdname tail_tbl
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


mendatory_tailored_tables_error <- function(stcs, mendatory_tab){
  if(any(!mendatory_tab%in%names(stcs))){
    stop("The following tables are mendatory: ",paste(mendatory_tab,collapse=", "),".")
  }
}


