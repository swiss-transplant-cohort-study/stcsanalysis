# stcsanalysis 0.1.6

* Tested on AT from: 2024-05-01, 2024-05-14, 2024-05-21, 2024-06-17 
* correction for AT 0.0.1: pnf_date in tailored_organsurvival(), 
* correction for AT 0.0.1: handle new disease_category in categorize_diagnosis(), 
* add variable in  tailored_organsurvival(): next_tpxdate, next_organspecific_tpxdate
* add variable in tailored_organbl(): bmi
* correction in category_diagnosis.


# stcsanalysis 0.1.5

* Tested on AT from: 2024-02-13, 2024-02-06, 2024-02-01, 
* Developed AT from: 2024-02-13,
* which.pmin_chr() include ties argument
* new utils function noinf_min()
* add_var(): debug when by is multiple columns
* change parametrization of categorize function using .startdate, .stopdate.
* add bl tailored tables: tailored_patientbl(), tailored_transplantationbl()
* rename serology_combinaison() to serology_combination()
* add categorize_organevent(), categorize_organintervention()
* set tz = "Europe/Zurich", for date time.
* remove future_stcs_read() and dependency to future, listenv


# stcsanalysis 0.1.4

* Tested on AT from: 2024-02-13
* Developed AT from: 2024-02-13,
* add consent_comment in stcs_anonymize()

# stcsanalysis 0.1.3

* Tested on AT from: 2024-02-06
* Developed AT from: 2024-02-06,
* correction paste_valuecomment

# stcsanalysis 0.1.2

* Tested on AT from: 2024-02-01
* Developed AT from: 2024-02-01,
* add pkgdown

# stcsanalysis 0.1.1

* Tested on AT from: 2024-02-01
* Developed AT from: 2024-02-01,
* add crf_status in patientsurvival
* add transplantationsurvival
* add categorize_pathogenspecies()
* add consent withdrawal in patient survival
* handle infsite in categorize_pathogenspecies


# stcsanalysis 0.1.0

* Tested on AT from: 2024-01-24
* Developed AT from: 2024-01-24, 2024-01-15, 2023-12-11, 2023-10-18
* Added a `NEWS.md` file to track changes to the package.
* add_var function()
* read_stcs()+ future_read_stcs()
* write_stcs_csv() + write_stcs_xlsx()
* data_patientkey(), data_organkey(), data_soaskey()
* recode(), age_int(), which.pmin()
* stcs_filter_patientkey(), stcs_select_organ(), stcs_select_table(), stcs_anonymize()
* impute_lastday()
