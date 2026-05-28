#' Search the minimal keys in a data frame
#'
#' Recursively identifies minimal keys in a data frame.
#'
#' @param x data.frame. The data.frame to retrieve the unique key.
#' @param vmax int. Default is 2. Define the number of variable that create together a unique key.
#' @param key_list list. Use for recursive search.
#'
#' @returns a list of variables which are together unique identifiers of the rows.
#' @export
#'
#' @examples
#'
#'  df <- data.frame(
#'        id = c(1, 2, 3, 4),
#'        grp = c("a", "a", "b", "a"),
#'        val = c(10, 20, 30, 30))
#'
#'  minimal_key(df)
#'
#'@importFrom vctrs vec_unique_count
#'@importFrom utils combn
minimal_key <- function(x, vmax = 2, key_list = list()){

  vmax <- min(ncol(x), vmax)
  lvl <- length(key_list)+1

  cn <- colnames(x)
  if(lvl>1){
    cn <- cn[!cn%in%key_list[[1]]]
  }


  if(lvl>length(cn)|length(cn)==0){
    candidates_key <- list()
  }else{
    candidates_key <- combn(cn, lvl, simplify = FALSE)
  }


  if(length(key_list)!=0){
    key_ul <- unlist(key_list, recursive = FALSE)
    any_sub <- sapply(candidates_key, \(cki){
      any(sapply(key_ul, \(ki)all(ki%in%cki)))
    })
    candidates_key <- candidates_key[!any_sub]
  }


  if(length(candidates_key) == 0){
    return(key_list)
  }else{
    uk_ind <- sapply(candidates_key, \(cki){
      nrow(x) == vec_unique_count(x[,cki])
    })
    key_list[[lvl]] <- candidates_key[uk_ind]
    if(vmax==length(key_list)){
      return(key_list)
    }else{
      return(minimal_key(x = x, vmax = vmax, key_list = key_list))
    }

  }


}



