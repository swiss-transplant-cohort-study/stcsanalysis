#' eGFR
#'
#' @param crea_mumoll Numeric. Creatinin in \eqn{\mu}mol/l
#' @param age Numeric. Age in years
#' @param sex Character. Sex either "Male" or "Female"
#'
#' @return eGFR in mL/min/1.73m\eqn{^2} as described in https://www.kidney.org/content/ckd-epi-creatinine-equation-2021. The conversion factor of creatinin is 1 (\eqn{\mu}mol/l) = 88.42 (mg/dL)
#'
#' @references Inker, L. A., Eneanya, N. D., Coresh, J., Tighiouart, H., Wang, D., Sang, Y., ... & Levey, A. S. (2021). New creatinine-and cystatin C–based equations to estimate GFR without race. New England Journal of Medicine, 385(19), 1737-1749.
#'
#' @export
#' @importFrom dplyr case_when
egfr_2021 <- function(crea_mumoll,age,sex){
  age <- check_input(age,0,140)
  crea_mumoll <- check_input(crea_mumoll,0,4000)
  sex <- check_input(sex,c("Male","Female"))


  ## from https://www.kidney.org/content/ckd-epi-creatinine-equation-2021
  #eGFRcr = 142 x min(Scr/κ, 1)α x max(Scr/κ, 1)-1.200 x 0.9938Age x 1.012 [if female]
  k <- case_when(sex=="Female"~0.7,sex=="Male"~0.9)
  alpha <- case_when(sex=="Female"~-.241,sex=="Male"~-.302)
  corr_sex <- case_when(sex=="Female"~1.012,sex=="Male"~1)
  # convert to mg/dl
  crea_mgdl <- crea_mumoll/88.42
  142*pmin(crea_mgdl/k,1)^alpha*pmax(crea_mgdl/k,1)^(-1.2)*0.9938^age*corr_sex
}

