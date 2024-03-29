% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stcs_filter.R
\name{select}
\alias{select}
\alias{stcs_anonymize}
\alias{stcs_remove_emptytab}
\alias{stcs_select_organrelevance}
\alias{stcs_select_table}
\title{Select variable of the STCS dataset}
\usage{
stcs_anonymize(
  stcs,
  vars2rm = c("initials", "dob", "soascaseid", "soasdonorid", "donor_dob",
    "consent_comment")
)

stcs_remove_emptytab(stcs)

stcs_select_organrelevance(
  stcs,
  organ = c("Heart", "Islets", "Kidney", "Liver", "Lung", "Pancreas", "Small bowel")
)

stcs_select_table(stcs, tables)
}
\arguments{
\item{stcs}{list. A list of STCS tables}

\item{vars2rm}{chr. A vector of variables to remove.}

\item{organ}{chr. A vector of STCS organ names.}

\item{tables}{chr. A vector of STCS tables name.}
}
\value{
a list of modified \code{stcs} tables with updated VariableMetaData.
}
\description{
Select variable of the STCS dataset
}
\details{
\code{stcs_anonymize()}: remove sensitive variables.

\code{stcs_remove_emptytab()}: remove empty tables, (if any)

\code{stcs_select_organrelevance()}: select variable based on their \code{organ_relevance}.

\code{stcs_select_table()}: select given the tables.
}
\concept{select}
