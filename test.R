library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(readr)
library(stcsanalysis)
library(stringr)
library(lubridate)


dir = "M:/MEDIZIN/STCS/00_CDM/Data 3LC/core/2024-01-15/csv"
stcs <- stcs_read(dir)

stcs_select_table(stcs,setdiff(names(stcs),"graftloss")) |>
  tailored_patientsurvival()



tb_ps <- tailored_patientsurvival(stcs)

tb_gg<-
  tb_ps |>
  mutate(stop_date = pmin(deathdate,laststcs_glodate,last_dropoutdate,cutoff_date,na.rm = T),
         stop = which.pmin_chr(death=deathdate,lastglo=laststcs_glodate,dropout=last_dropoutdate,cutoff=cutoff_date)) |>
  mutate(fupstop_nday = as.integer(stop_date-last_assdate)) |>
  arrange(desc(fupstop_nday)) |>
  select(patientkey,enrollment_date, stop,stop_date,last_assdate,fupstop_nday,deathdate,laststcs_glodate,last_dropoutdate,cutoff_date)


library(ggplot2)
breaks=c(-800,-200,0,200,400,600,800,1000,2000,5000)
tb_gg |>
  ggplot()+
  geom_histogram(aes(x=fupstop_nday),breaks=breaks,color="white")+
  stat_bin(breaks=breaks,aes(x = fupstop_nday,y=after_stat(count), label=after_stat(count)), geom="text", hjust = -.1, angle = 90)+
  scale_y_continuous(limits = c(0,3200))+
  labs(x = "ndays",y=NULL,title= "Number of days between last FUP and the stop date (death, dropout, cutoff, lastglo)")+
  theme_bw()


tb_gg$fupstop_nday |> quantile(c(0,.5,.75,.85,.95,1))


stcs$organ |>
  select(patientkey,organkey) |>
  add_var(stcs,c("glodate"="date"),from = "graftloss", by = "organkey") |>
  group_by(patientkey) |>
  filter(any(!is.na(glodate))) |>
  summarise(first_glodate = min(glodate,na.rm = T),
            laststcs_glodate = max(glodate))

stcs$graftloss |>
  group_by(patientkey) |>
  summarise(first_glodate = min(date))

stcs$stop |>
  filter(!is.na(lastalivedate)) |>
  count(patientkey) |>
  arrange(desc(n))


stcs$stop |>
  filter(patientkey=="pat002618") |>
  add_var(stcs,"patid")


stcs$patientlongitudinal |>
  add_var(stcs,"patid") |>
  filter(patid=="80004837") |>
  arrange(desc(assdate))


dz


op0 <- openxlsx_getOp("openxlsx.dateFormat")

openxlsx_setOp("openxlsx.dateFormat", "yyyy-mm-dd")


wb <- createWorkbook()
addWorksheet(wb, sheetName = "donor")
writeData(wb,sheet = "donor",x=stcs$donor)

saveWorkbook(wb, file = file.path("donor.xlsx"), overwrite = TRUE)


stcs |>
  stcs_select_table(c("patient","donor"))|>
  stcs_write_csv("export")

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
