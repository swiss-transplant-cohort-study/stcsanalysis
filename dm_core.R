library(stcsanalysis)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(dm)

params <- list(
  dir = 'M:/MEDIZIN/STCS/00_CDM/Data 3LC/core/2023-10-16')


## 1 @SPECIAL-PARAMETERS ----

stcs <- read_stcs(params$dir)

# cb <- stcswrangling:::read_stcs_codebook(params$path_cb)

stcs2dm <- function(core){

  dm0 <- do.call("dm",args=core)

  pks <-
    core$variablemetadata |>
    select(dataset,dataset_pk) |>
    distinct() |> drop_na() |>
    mutate(dataset_pk = str_split(dataset_pk,", "))

  for(i in seq_len(nrow(pks))){
    dsi <- pks$dataset[i]
    pki <- pks$dataset_pk[[i]]
    q <- quote(dm_add_pk(dm = dm0))
    q[["table"]] = sym(dsi)
    q[["columns"]]= quote(all_of())
    q[["columns"]] [[2]]<-pki
    dm0 <- eval(q)

    #dm0 <- dm_add_pk(dm0, !!sym(dsi),all_of(eval(pki)), check = T)
  }


  if("allcancerstatus"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,allcancerstatus,c(diseasekey),patientdisease, check = T)
  }

  if("allograftdisease"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,allograftdisease ,organkey,organ, check = T)
  }
  if("biopsyrejection"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,biopsyrejection ,organkey,organ, check = T)
  }
  if("biosample"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,biosample ,soaskey,transplantation, check = T)
  }
  if("biosampleusage"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,biosampleusage ,biosampkey,biosample, check = T)
  }
  if("complications"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,complications ,organkey,organ, check = T)
  }
  if("consent"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,consent ,patientkey,patient, check = T)
  }

  if("deathcause"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,deathcause ,patientkey,patient, check = T)
  }

  if("perioperativecare"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,perioperativecare,soaskey,transplantation, check = T)
  }

  if("graftloss"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,graftloss,organkey,organ, check = T)
  }

  if("hcplog"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,hcplog,patientkey,patient, check = T)
    dm0 <- dm_add_fk(dm0,hcplog,providerkey,careprovider, check = T)
  }

  if("hladonor"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,hladonor,c(donorkey),donor, check = T)
  }

  if("hlapatient"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,hlapatient,c(patientkey),patient, check = T)
  }


  if("infectionsite"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,infectionsite,c(diseasekey),patientdisease, check = T)
  }

  if("infectionpathogen"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,infectionpathogen,c(diseasekey),patientdisease, check = T)
  }

  if("interventions"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,interventions,c(organkey),organ, check = T)
  }

  if("immunologyantihla"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,immunologyantihla,c(soaskey),transplantation, check = T)
  }

  if("immunologycm"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,immunologycm,c(soaskey),transplantation, check = T)
  }

  if("immunologydsa"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,immunologydsa,c(soaskey),transplantation, check = T)
  }

  if("livertumorstaging"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,livertumorstaging,c(organkey),organ, check = T)
    dm0 <- dm_add_fk(dm0,livertumorstaging,c(undiskey),underlyingdisease, check = T)
  }

  if("livertumormarkerlab"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,livertumormarkerlab,c(organkey),organ, check = T)
  }


  if("livertumortherapy"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,livertumortherapy,c(organkey),organ, check = T)
    dm0 <- dm_add_fk(dm0,livertumortherapy,c(undiskey),underlyingdisease, check = T)

  }

  if("liverglocauses"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,liverglocauses,c(organkey),graftloss, check = T)

  }

  if("lungfctassessment"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,lungfctassessment,c(organkey),organ, check = T)
  }

  if("lungfev1best"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,lungfev1best,c(organkey),organ, check = T)
  }

  if("medicationtreatment"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,medicationtreatment,c(patientkey),patient, check = T)
  }

  if("organ"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,organ,c(soaskey),transplantation, check = T)
  }

  if("organlab"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,organlab,c(organkey),organ, check = T)
  }

  if("organlongitudinal"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,organlongitudinal,c(organkey),organ, check = T)
    dm0 <- dm_add_fk(dm0,organlongitudinal,c(patassessmentkey),patientlongitudinal, check = T)

  }

  if("patientlongitudinal"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,patientlongitudinal,c(patientkey),patient, check = T)
  }

  if("patientdisease"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,patientdisease,c(patientkey),patient, check = T)
  }

  if("patientlab"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,patientlab,c(patientkey),patient, check = T)
  }

  if("psq"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,psq,c(patassessmentkey),patientlongitudinal, check = T)
    dm0 <- dm_add_fk(dm0,psq,c(patientkey),patient, check = T)

  }

  psqs <- c("psqadherence",
            "psqdaysleep", "psqeducation", "psqequvas", "psqquol", "psqsleep",
            "psqtrust", "psq2exercise", "psq2profession", "psq2workcap",
            "psq3activity", "psq3houshold", "psq3occupation", "psq3sport")
  psq_quote <- quote(dm_add_fk(dm = dm0,columns = c(psqkey),ref_table = psq, check = T))

  for(i in seq_along(psqs)){
    if(psqs[i]%in%names(dm0)){
      qi <- psq_quote
      qi[["table"]] = sym(psqs[i])
      dm0 <- eval(expr = qi)
    }
  }


  if("rejectiontreatment"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,rejectiontreatment,c(biorejkey),biopsyrejection, check = T)
  }

  if("serologydonor"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,serologydonor,c(donorkey),donor, check = T)
  }

  if("serologypatient"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,serologypatient,c(soaskey),transplantation, check = T)
  }


  if("stimulationlab"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,stimulationlab,c(organkey),organ, check = T)
  }

  if("stop"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,stop,c(patientkey),patient, check = T)
  }

  if("transplantation"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,transplantation,c(patientkey),patient, check = T)
    #dm0 <- dm_add_fk(dm0,transplantation,c(donorkey),donor, check = T)
  }

  if("underlyingdisease"%in%names(dm0)){
    dm0 <- dm_add_fk(dm0,underlyingdisease,c(organkey),organ, check = T)
  }

  dm0

}

nopsq <- !str_detect(names(stcs),"psq")

stcs0 <-
  stcs |>
  stcs_select_table(tables = names(stcs)[nopsq]) |>
  stcs_select_organ(organ = c("Liver"))

dm0 <-
  stcs0 |>
  stcs2dm()

dm0|>
  dm_draw("BT",view_type = "keys_only")

gdb
