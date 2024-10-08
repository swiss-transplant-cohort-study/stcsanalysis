
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
    stcs_select_organrelevance(organ= orgs) |>
    stcs_remove_emptytab() |>
    stcs_select_table(tables=c("patient","organ","transplantation")) |>
    stcs_anonymize()|>
    validate_metadata()

  expect_equal(length(out),5L)


})


## Tailored tables ----

test_that(paste0(dir_name[i],": Run tailored_tables()"), {

  out <- tailored_tables(stcs)

  lapply(out,\(xi){expect_s3_class(xi,"data.frame")})

})



## Categorization ----

test_that(paste0(dir_name[i],": Run categorize_infsite()"), {

  out <- stcs$patientdisease |>
    dplyr::filter(patdiagnosis=="Virus") |>
    categorize_infsite(stcs)

  expect_s3_class(out,"data.frame")
  expect_true(nrow(out)>0L)

})


test_that(paste0(dir_name[i],": Run categorize_pathogenspecies()"), {

  out <- stcs$patientdisease |>
    dplyr::filter(patdiagnosis=="Virus") |>
    categorize_pathogenspecies(stcs)

  expect_s3_class(out,"data.frame")
  expect_true(nrow(out)>0L)

})


test_that(paste0(dir_name[i],": Run categorize_diagnosis()"), {

  out <- stcs$patient |>
    dplyr::mutate(startdate = enrollment_date-lubridate::days(50),
                  stopdate = enrollment_date+lubridate::days(50)) |>
    categorize_diagnosis(stcs,.startdate = "startdate",.stopdate = "stopdate")

  expect_s3_class(out,"data.frame")
  expect_true(nrow(out)>0L)

})


test_that(paste0(dir_name[i],": Run categorize_diagnosis(), pre"), {

  out <- stcs$patient |>
    dplyr::mutate(startdate = enrollment_date-lubridate::days(5),
                  stopdate = enrollment_date+lubridate::days(50)) |>
    categorize_diagnosis(stcs,.startdate = "startdate",.stopdate = "stopdate",
                            .is_pre = FALSE)

  expect_s3_class(out,"data.frame")

})


test_that(paste0(dir_name[i],": Run categorize_treatment()"), {

  out <- stcs$patient |>
    dplyr::mutate(startdate = enrollment_date-lubridate::days(3),
                  stopdate = enrollment_date+lubridate::days(3)) |>
    categorize_treatment(stcs,.startdate = "startdate",.stopdate = "stopdate")

  expect_s3_class(out,"data.frame")
  expect_true(nrow(out)>0L)

})



test_that(paste0(dir_name[i],": Run categorize_rejectiontreatment()"), {

  out <- stcs$biopsyrejection |>
    categorize_rejectiontreatment(stcs)

  expect_s3_class(out,"data.frame")
  expect_true(nrow(out)>0L)

})



test_that(paste0(dir_name[i],": Run categorize_organevent()"), {

  out <- stcs$organ |>
    dplyr::mutate(startdate = tpxdate-lubridate::days(3),
                  stopdate = tpxdate+lubridate::days(30)) |>
    categorize_organevent(stcs,.startdate = "startdate",.stopdate = "stopdate")

  expect_s3_class(out,"data.frame")
  expect_true(nrow(out)>0L)

})

test_that(paste0(dir_name[i],": Run categorize_organintervention()"), {

  out <- stcs$organ |>
    dplyr::mutate(startdate = tpxdate-lubridate::days(3),
                  stopdate = tpxdate+lubridate::days(30)) |>
    categorize_organintervention(stcs,.startdate = "startdate",.stopdate = "stopdate")

  expect_s3_class(out,"data.frame")
  expect_true(nrow(out)>0L)

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

  expect_equal(data$agem,c(-13L, 0L, 11L, 12L, 23L, 35L, 47L, 48L))

})


test_that("Run serology_combination()", {
  out <-
    expand.grid(rec = c("Positive","Negative",NA_character_),
                don = c("Positive","Negative",NA_character_),stringsAsFactors = F) |>
    dplyr::mutate(comb = serology_combination(rec,don)) |>
    dplyr::pull(comb)

  expect_equal(out,c("R+/D+", "R-/D+", NA_character_, "R+/D-", "R-/D-",
                     NA_character_, NA_character_, NA_character_, NA_character_))

})


test_that("Run which.pmin_chr()", {
  out <- c(which.pmin_chr(a=1:5,b =5:1,ties="first"),
           which.pmin_chr(a=1:5,b =5:1,ties="collapse"))

  expect_equal(out,c("a", "a", "a", "b", "b", "a", "a", "a | b", "b", "b"))

})

test_that("Run noinf_min()", {
  out <- c(noinf_min(c(NA,NA)),
           noinf_min(c(NA,4)))

  expect_equal(out,c(NA,4))

})

test_that("Run paste_valuecomment()", {
  out <- c(paste_valuecomment(c("other","disease A"),c("comment 1", NA)),
           paste_valuecomment(NA,NA))

  expect_equal(out,c("disease A | other:comment 1",NA))

})


test_that("Run is_truemissing()", {
out <-
  list(fct = factor(c("Missing","Other")),
           chr= c("Missing","Other"),
           date = c(lubridate::make_date(1700L,1L,1L),
                    lubridate::make_date(1700L,1L,2L)),
           int = c(-9999L,9998L),
           dbl = c(-9999,9999.00001),
           dttm = c(as.POSIXct("1700-01-01 00:00:00", tz = "Europe/Zurich"),
                    as.POSIXct("1700-01-01 00:00:01", tz = "Europe/Zurich")),
           lgl = c(TRUE,FALSE)) |>
  lapply(is_truemissing)

expect_equal(out,  list(fct =c(TRUE,FALSE),
                        chr= c(TRUE,FALSE),
                        date = c(TRUE,FALSE),
                        int = c(TRUE,FALSE),
                        dbl = c(TRUE,FALSE),
                        dttm = c(TRUE,FALSE),
                        lgl = c(FALSE,FALSE)))


})



test_that("Run truemissing_to_na()", {
  out <-
    list(fct = factor(c("Missing","Other")),
         chr= c("Missing","Other"),
         date = c(lubridate::make_date(1700L,1L,1L),
                  lubridate::make_date(1700L,1L,2L)),
         int = c(-9999L,9998L),
         dbl = c(-9999,9999.00001),
         dttm = c(as.POSIXct("1700-01-01 00:00:00", tz = "Europe/Zurich"),
                  as.POSIXct("1700-01-01 00:00:01", tz = "Europe/Zurich")),
         lgl = c(TRUE,FALSE)) |>
    lapply(truemissing_to_na)

  expect_equal(out,  list(fct = factor(c(NA_character_,"Other"), levels=c("Missing","Other")),
                          chr= c(NA_character_,"Other"),
                          date = c(lubridate::NA_Date_,
                                   lubridate::make_date(1700L,1L,2L)),
                          int = c(NA_integer_,9998L),
                          dbl = c(NA_real_,9999.00001),
                          dttm = c(as.POSIXct(NA_character_, tz = "Europe/Zurich"),
                                   as.POSIXct("1700-01-01 00:00:01", tz = "Europe/Zurich")),
                          lgl = c(TRUE,FALSE)))


})


