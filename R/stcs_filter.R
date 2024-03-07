#' Select variable of the STCS dataset
#'
#' @param stcs list. A list of STCS tables
#' @param organ chr. A vector of STCS organ names.
#' @param tables chr. A vector of STCS tables name.
#' @param vars2rm chr. A vector of variables to remove.
#'
#' @return a list of modified \code{stcs} tables with updated VariableMetaData.
#'
#' @details
#'
#' \code{stcs_anonymize()}: remove sensitive variables.
#'
#' \code{stcs_remove_emptytab()}: remove empty tables, (if any)
#'
#' \code{stcs_select_organrelevance()}: select variable based on their \code{organ_relevance}.
#'
#' \code{stcs_select_table()}: select given the tables.
#'
#' @name select
#' @family select

#'@rdname select
#'@export
stcs_anonymize <- function(stcs, vars2rm = c("initials","dob","soascaseid","soasdonorid", "donor_dob", "consent_comment")){
  vars2rm <- arg_match(vars2rm, values = c("initials","dob","soascaseid","soasdonorid", "donor_dob", "consent_comment"),
                       multiple= TRUE)


  ## filter metadata with the correct vars2rm
  data_select <- stcs[["variablemetadata"]] |>
    select(all_of(c("dataset","variable"))) |>
    mutate("dataset"=tolower(!!sym("dataset")))

  if("initials"%in%vars2rm){
    data_select <- data_select |>
      filter(!(!!sym("dataset")=="patient" & !!sym("variable")=="initials"))
  }

  if("dob"%in%vars2rm){
    data_select <- data_select |>
      filter(!(!!sym("dataset")=="patient" & !!sym("variable")=="dob"))
  }

  if("soascaseid"%in%vars2rm){
    data_select <- data_select |>
      filter(!(!!sym("dataset")=="transplantation" & !!sym("variable")=="soascaseid"))
  }

  if("soasdonorid"%in%vars2rm){
    data_select <- data_select |>
      filter(!(!!sym("dataset")=="donor" & !!sym("variable")=="soasdonorid"))
  }

  if("donor_dob"%in%vars2rm){
    data_select <- data_select |>
      filter(!(!!sym("dataset")=="donor" & !!sym("variable")=="dob"))
  }

  if("consent_comment"%in%vars2rm){
    data_select <- data_select |>
      filter(!(!!sym("dataset")=="consent" & !!sym("variable")=="consent_comment"))
  }

  ## select variable in meta
  stcs_select_meta(stcs,data_select)
}



#'@rdname select
#'@export
stcs_remove_emptytab <- function(stcs){

  stcs_select_table(stcs, names(stcs)[sapply(stcs,nrow)!=0L])

}

#'@importFrom rlang arg_match
#'@importFrom stringr str_detect regex
#'@importFrom tidyselect all_of
#'@importFrom dplyr filter pull select
#'@rdname select
#'@export
stcs_select_organrelevance <- function(stcs, organ = c("Heart", "Islets", "Kidney", "Liver", "Lung", "Pancreas", "Small bowel")){
  organ <- arg_match(organ,values=c("Heart", "Islets", "Kidney", "Liver", "Lung", "Pancreas", "Small bowel"),
                     multiple= TRUE)
  ## Check if we have organs we reject
  if(any(!stcs$organ$organ%in%organ)){warning("Your dataset contains organs not listed in 'organ'. You might loose information.")}

  ## vector to regex
  organ <- paste0(organ,collapse = "|")

  ## filter metadata with the correct organs
  data_select <- stcs[["variablemetadata"]] |>
    filter(str_detect(!!sym("organ_relevance"),regex(organ))) |>
    select(all_of(c("dataset","variable")))

  ## select variable in meta
  stcs_select_meta(stcs,data_select)
}


#'@rdname select
#'@export
stcs_select_table <- function(stcs, tables){
  val0 <- c((stcs[["variablemetadata"]][["dataset"]]) |> unique() |> sort() |> tolower(),"variablemetadata")
  tables <- tolower(tables)
  tables <- arg_match(tables, values=val0,multiple = TRUE)

  tables <- c("admin","variablemetadata",tables)|> unique() |> sort()

  stcs_select_meta(stcs,
                   stcs[["variablemetadata"]] |>
                     select(all_of(c("dataset","variable"))) |>
                     filter(tolower(!!sym("dataset"))%in%tables))

}



# PRIVATE ----

#' @importFrom dplyr inner_join
stcs_select_meta <- function(stcs,data){
  stopifnot("data must contains 2 variables named dataset and variables"=all(colnames(data)%in%c("dataset","variable")))

  ## save orginial meta variable

  meta0 <- stcs[["variablemetadata"]]
  ## save variable names
  nms <- names(stcs)[names(stcs)!="variablemetadata"]

  stopifnot("All dataset in stcs must be listed in variablemetadata"=all(nms%in%c(tolower(unique(stcs[["variablemetadata"]][["dataset"]])))))

  ## add admin

  data <-
    rbind(
      meta0 |>
        filter(tolower(!!sym("dataset"))=="admin") |>
        select(all_of(c("dataset","variable"))),
      data) |>
    mutate("dataset"=tolower(!!sym("dataset"))) |>
    distinct()

  ## select variable in meta
  stcs <-
    lapply( nms,\(ni){
      stcs[[ni]] |>
        select(all_of(data |> filter(!!sym("dataset")==ni) |> pull("variable")))
    })
  names(stcs) <- nms

  ## remove empty dataset
  stcs <- stcs[sapply(stcs,ncol)!=0L]

  ## add new metadata
  stcs[["variablemetadata"]] <-
    meta0 |>
    mutate("short_name" = tolower(!!sym("dataset"))) |>
    inner_join(
      data,
      by = c("short_name"="dataset","variable"),relationship = "one-to-one"
    ) |>
    select(-all_of("short_name"))

  stcs

}


filter_stcs_tab <- function(x,key,keys){
  if(key%in%colnames(x)){
    x |>
      filter((!!sym(key))%in%keys|is.na(!!sym(key)))
  }else{
    x
  }
}




