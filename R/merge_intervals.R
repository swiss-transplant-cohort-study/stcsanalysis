#' Merge Interval
#'
#' Merge overlapping intervals.
#'
#' @param x0 vct. Vector of starting time. Must be complete.
#' @param x1 vct. Vector of ending time. Must be complete and satisfy \code{x0 <= x1}.
#' @param .lag scalar value indicating the time lag when merging interval. See Examples below.
#'
#' @returns a \code{data.frame} with 1 row per intervals, without overlapping.
#'
#' @examples
#'
#' # return 2 intervals
#' merge_intervals(c(1, 3, 4), c(2, 4, 5))
#'
#' # return 1 interval
#' merge_intervals(c(1, 3, 4), c(2, 4, 5), .lag = 1)
#' @export
merge_intervals <- function(x0, x1, .lag = 0L) {
  stopifnot("x0 and x1 must have the same length." = length(x0) == length(x1))
  stopifnot("x0 and x1 must have the same data-type." = inherits(x0, class(x1)) && inherits(x1, class(x0)))

  stopifnot("x0 cannot have NA's." = !any(is.na(x0)))
  stopifnot("x1 cannot have NA's." = !any(is.na(x1)))

  stopifnot("x0 cannot have Missing type." = !any(is.na(truemissing_to_na(x0))))
  stopifnot("x1 cannot have Missing type." = !any(is.na(truemissing_to_na(x1))))
  stopifnot("x1 must be after x0." = all(x0 <= x1))

  o <- order(x0, x1)
  x0 <- x0[o]
  x1 <- x1[o]

  res_x0 <- x0[1]
  res_x1 <- x1[1]

  # overlap rule

  for (i in seq_along(x0)[-1]) {
    if (x0[i] <= (res_x1[length(res_x1)] + .lag)) {
      res_x1[length(res_x1)] <- max(res_x1[length(res_x1)], x1[i])
    } else {
      res_x0 <- c(res_x0, x0[i])
      res_x1 <- c(res_x1, x1[i])
    }
  }

  data.frame(x0 = res_x0, x1 = res_x1)
}
