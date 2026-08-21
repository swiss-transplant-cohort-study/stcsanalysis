#' Filter for a regex in all character variables.
#'
#' Iterate through all dataset, character variables and filter those tables
#'
#' @param stcs list. A list of STCS tables
#' @param pattern regex. A regex pattern pass to \code{str_detect()}.
#' @param negate lgl. Default is \code{FALSE}. Passed to \code{str_detect()}.
#'
#' @returns A tibble with the result displayed as a nested table.
#'
#'
#'@export
#'@importFrom dplyr relocate rename
#'@importFrom purrr map
stcs_search <- function(stcs, pattern, negate = FALSE){
  ret <-
    tibble(
      "dataset" = names(stcs),
      "tab" = stcs) |>
    mutate("result" = map(!!sym("tab"), \(tabi){
      tibble("variable" = colnames(select(tabi, where(is.character)))) |>
        mutate("result" = map(!!sym("variable"), \(vi){
          sel <- str_detect(tabi[[vi]], pattern = pattern, negate = negate)
          tabi |>
            filter(sel) |>
            mutate(".result_value" = tabi[[vi]][replace_na(sel, FALSE)]) |>
            nest("result" = - !!sym(".result_value"))
        })) |>
        filter(sapply(!!sym("result"), nrow)>0L) |>
        unnest(all_of("result"))
    })) |>
    select(- all_of("tab")) |>
    unnest(all_of("result")) |>
    rename("value" = !!sym(".result_value"))|>
    relocate(!!sym("value"), .after = all_of("variable"))
  ret
}





