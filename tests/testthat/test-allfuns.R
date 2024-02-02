
options("lifecycle_verbosity"="error")


# LOAD STCS DATA ----
if(file.exists(file.path("..","..","stcspath.txt"))){
  dir <- readLines(file.path("..","..","stcspath.txt"))
}else if(file.exists(file.path("..","..","..","stcsanalysis","stcspath.txt"))){
  dir <- readLines(file.path("..","..","..","stcsanalysis","stcspath.txt"))
}else{
  dir <-readLines("H:/project/stcsanalysis/stcspath.txt")
}

dir <- stringr::str_trim(dir)
dir <- dir[dir!=""]

dir_name <- strsplit(dir,"/|\\\\")


path_diff <- !apply(do.call("cbind",dir_name),1,\(x){length(unique(x))==1L})

dir_name <- sapply(dir_name,\(x){x[which(path_diff)[1]]})


for (i in seq_along(dir)){

stcs <- stcs_read(dir[i])


# filter the dataset ----

pk <-
  data_patientkey(stcs) |>
  expand_var_from(stcs,"soaskey","transplantation","patientkey") |>
  expand_var_from(stcs,"organkey","organ","soaskey") |>
  dplyr::filter(!is.na(organkey)) |>
  add_var(stcs,c("tpxdate"),"organ","organkey") |>
  dplyr::filter(tpxdate>Sys.Date()-lubridate::dyears(3)) |>
  dplyr::pull(patientkey) |>
  unique()

stcs <-
  stcs_filter_patientkey(stcs,pk) |>
  validate_metadata()



## Utilities ----

test_that(paste0(dir_name[i],": var_available()"), {

  expect_output(data_soaskey(stcs) |>
                   var_available(stcs))

})



## detection ----

test_that(paste0(dir_name[i],": Run detection"), {

  out <-
  data_organkey(stcs) |>
    add_var(stcs,"coldisch")|>
    add_var(stcs,c("tpxdate",id="organid"))|>
    colnames()


  expect_equal(out,c("organkey", "coldisch", "tpxdate", "id"))

})



## Selecting/Filtering ----

test_that(paste0(dir_name[i],": selecting and filter"), {

  orgs <- c("Heart", "Lung")
  pk <- data_organkey(stcs) |>
    add_var(stcs,c("organ","soaskey"),from = "organ",by = "organkey") |>
    add_var(stcs,"patientkey","transplantation") |>
    dplyr::group_by(patientkey) |>
    dplyr::filter(all(organ%in%orgs)) |>
    dplyr::ungroup() |>
    dplyr::pull(patientkey) |>
    unique()


  out <-
    stcs |>
    stcs_filter_patientkey(pk) |>
    stcs_select_organ(organ= orgs) |>
    stcs_remove_emptytab() |>
    stcs_select_table(tables=c("patient","organ","transplantation")) |>
    stcs_anonymize()|>
    validate_metadata()

  expect_equal(length(out),5L)


})


## Tailored tables ----

test_that(paste0(dir_name[i],": Run tailored_organ()"), {

  out <- tailored_organ(stcs)

  expect_s3_class(out,"data.frame")

})

test_that(paste0(dir_name[i],": Run tailored_patientsurvival()"), {

  out <- tailored_patientsurvival(stcs)

  expect_s3_class(out,"data.frame")

})

test_that(paste0(dir_name[i],": Run tailored_organsurvival()"), {

  out <- tailored_organsurvival(stcs)

  expect_s3_class(out,"data.frame")

})

test_that(paste0(dir_name[i],": Run tailored_organsurvival()"), {

  out <- tailored_psq(stcs)

  expect_s3_class(out,"data.frame")

})


## categorize

test_that(paste0(dir_name[i],": Run categorize_infsite()"), {

  out <- stcs$patientdisease |>
    dplyr::filter(disease_category=="Infection") |>
    add_var(stcs,"enrollment_date",from = "patient","patientkey") |>
    categorize_infsite(stcs,.date = "enrollment_date")

  expect_s3_class(out,"data.frame")

})


test_that(paste0(dir_name[i],": Run categorize_medication()"), {

  out <- stcs$patient |>
    categorize_medication(stcs,.date = "enrollment_date")

  expect_s3_class(out,"data.frame")

})


test_that(paste0(dir_name[i],": Run categorize_medication()"), {

  out <- stcs$patient |>
    categorize_otherdisease(stcs,.date = "enrollment_date")

  expect_s3_class(out,"data.frame")

})


}

# NOT BASED ON STCS ----

## recoding ----

test_that("recode()", {

  rec <- data.frame(input1 = LETTERS,
                    input2 = letters,
                    output = 1L:26L)

  expect_equal(new_coding(input2=c("a","b"),input1=c("A","B"),data = rec,.new_var = "output"),c(1L,2L))

})


## egfr ----

test_that("Run egfr_2021()", {

  data <-
    expand.grid(crea = c(50,200,1000),
                age = c(20,60),
                sex= c("Female","Male"),stringsAsFactors = F)
  data$egfr <- egfr_2021(data$crea,data$age,data$sex)


  expect_equal(data$egfr,c(133.593163688188, 31.0592504679203, 4.50222262164412, 104.17030388951,
                           24.2186910654752, 3.51064295302982, 144.284189204047, 41.4938585174298,
                           6.01478096419053, 112.506713823456, 32.3551574944189, 4.69007203340876))

})


## Age ----

test_that("Run age_int()", {
  data <-
    expand.grid(from = as.Date("2020-2-29"),
                to = as.Date(c("2019-2-28","2021-2-28","2021-3-1","2022-2-28","2023-2-28","2024-2-28","2024-2-29")))
  data$age = age_int(data$from, data$to)

  expect_equal(data$age,c(-1L, 0L,1L,1L,2L,3L,4L))

})


test_that("Run age_months()", {
  data <-
    expand.grid(from = as.Date("2020-2-29"),
                to = as.Date(c("2019-1-28","2020-2-28","2021-2-28","2021-3-1","2022-2-28","2023-2-28","2024-2-28","2024-2-29")))
  data$agem = age_months(data$from, data$to)

  expect_equal(data$agem,c(-13L, 0L,11L,12L,23L,35L,47L,48L))

})


test_that("Run serology_combinaison()", {
  out <-
    expand.grid(rec = c("Positive","Negative",NA_character_),
                don = c("Positive","Negative",NA_character_),stringsAsFactors = F) |>
    dplyr::mutate(comb = serology_combinaison(rec,don)) |>
    dplyr::pull(comb)

  expect_equal(out,c("R+/D+", "R-/D+", NA_character_, "R+/D-", "R-/D-",
                     NA_character_, NA_character_, NA_character_, NA_character_))

})

