library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(readr)
library(stcswrangling2)
library(stringr)

egfr_2021(0,10,"Male")

dir = "M:/MEDIZIN/STCS/00_CDM/Data 3LC/core/2023-12-01/csv"
stcs <- stcs_read(dir)




tailored_psq(stcs) |> glimpse()

data_patientkey(stcs)

stcs$patientlab |>
  filter(test=="crea (µmol/l)") |>
  add_var(stcs,c("dob","sex"),"patient") |>
  mutate(age = age_int(dob,truemissing_to_na(date))) |>
  mutate(efgr2021 = egfr_2021(truemissing_to_na(result),age,sex))

data_patientkey(stcs) |>
  expand_var_from(stcs,.var = "patassessmentkey",from = "patientlongitudinal",by = "patientkey") |>
  add_var(stcs,"assdate",from = "patientlongitudinal")


tibble(
  dataset = stcswrangling2:::toupper_dataset(names(stcs),stcs),
  n_var = sapply(stcs, ncol),
  n_obs = sapply(stcs, nrow)) |>
  mutate(dataset_pk = map_chr(dataset,\(dsi){
    if(tolower(dsi)=="variablemetadata"){return(NA_character_)}
    stcs$variablemetadata |>
      select(dataset,dataset_pk) |>
      distinct() |>
      filter(dataset==dsi) |>
      pull(dataset_pk)}))


read_stcs0(dir,"Patient.csv")

read_stcs0(dir,"Patient.csv") |>
  mutate(age2 = age_int(dob,enrollmentdate)) |>
  filter(age!=age2)

data_patientkey(stcs) |>
  expand_var_from(stcs,"soaskey","transplantation","patientkey") |>
  expand_var_from(stcs,"organkey","organ","soaskey") |>
  add_var(stcs,"donorkey","transplantation") |>
  add_var(stcs,"organ") |>
  filter(organ=="Kidney") |>
  pull(patientkey)


stcs |>
  stcs_select_table(tables=c("patient"))

data = stcs$variablemetadata |>
  slice(1:100) |>
  select(dataset,variable)


data_patientkey(stcs) |>
  add_var(stcs,c("age","bg","sex","npretpx"),"patient") |>
  var_available(stcs)

data_soaskey(stcs) |>
  var_available(stcs)


data_organkey(stcs) |>
  add_var(stcs,c("soaskey","organ","tpxdate"),from = "organ",by = "organkey") |>
  add_var(stcs,c("patientkey","tpx","soascaseid"),from = "transplantation",by = "soaskey") |>
  add_var(stcs,c("patid","sex","yob"),from = "patient",by = "patientkey") |>
  select(all_of(c("organkey","soaskey","patientkey","patid","soascaseid","tpxdate","organ","tpx","sex","yob")))

stcs$patientdisease |> glimpse()
stcs$infectionpathogen
