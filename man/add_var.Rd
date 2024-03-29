% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_var.R
\name{add_var}
\alias{add_var}
\alias{expand_var_from}
\alias{new_var_from}
\title{Generic function to add columns}
\usage{
add_var(
  data,
  stcs,
  .var,
  from = detect_from(stcs, .var),
  by = detect_by(stcs, .var, from),
  .filter
)

expand_var_from(data, stcs, .var, from, by, .filter)

new_var_from(data, stcs, .var, from, by, .filter, relationship)
}
\arguments{
\item{data}{A data frame with a STCS key (\code{by}).}

\item{stcs}{A list containing the STCS data frame.}

\item{.var}{chr. The name of variables to add. It can be a named vector (to rename) of length >1.}

\item{from}{chr. The name of a dataset in the stcs object}

\item{by}{chr. A named vector used the matching. Argument passed to \code{left_join}. \code{names(by)} matches variable in data and \code{by} matches variable in \code{stcs$from}.}

\item{.filter}{an optional argument which can be use to filter \code{stcs$from} before matching.}

\item{relationship}{chr. Argument passed to \code{left_join}.}
}
\value{
A data frame with the new column.
}
\description{
Generic function to add columns
}
\examples{
\dontrun{
stcs <- stcs_read("path/to/stcs/dataset/")

data_patientkey(stcs) |>
   add_var(stcs,"sex")

data_patientkey(stcs) |>
  expand_var_from(stcs,"patlongkey",from="patientlongitudinal","patientkey") |>
  add_var(stcs,"bmi")
}
}
\concept{add_var}
