% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailored_organsurvival.R
\name{tailored_organsurvival}
\alias{tailored_organsurvival}
\title{Tailored analysis tables with main dates related to organs.}
\usage{
tailored_organsurvival(stcs)
}
\arguments{
\item{stcs}{A list containing the STCS data frame.}
}
\value{
A data frame.
}
\description{
Tailored analysis tables with main dates related to organs.
}
\details{
\code{tailored_organsurvival()}: PK: \code{organkey}.

\itemize{
\item{1.} \code{patientkey}: Identifier of patients. Source: \code{Organ}.
\item{2.} \code{donorkey}: Identifier of donors. Source: \code{Organ}.
\item{3.} \code{soaskey}: Identifier of transplantation. Source: \code{Organ}.
\item{4.} \code{organkey}: Identifier of organs. Source: \code{Organ}.
\item{5.} \code{tpxdate}: Date of transplantation. Source: \code{Organ}.
\item{6.} \code{glo_date}: Graft loss date. Source: \code{Graftloss}.
\item{7.} \code{pnf_date}: Date of PNF (\code{tpxdate} when \code{dgf} is \code{"PNF"}). Source: \code{Organ}.
\item{8.} \code{next_tpxdate}: Next \code{tpxdate} (\code{tpxdate} with \code{organ_order}+1)
\item{9.} \code{next_organspecific_tpxdate}: Next \code{tpxdate} of the same organ (\code{tpxdate} with \code{organspecific_order}+1)
\item{10.-18.} \code{last_[...]_toggle_date}: Assessment date of the last mentioned filled toggle (keep: \code{"Yes"}, \code{"No"}, removed: \code{"Missing"}, \code{NA}). Source: \code{OrganLongitudinal}.
\item{19.} \code{last_complete_organlongitudinal_crf_date}: Assessment date of the latest \code{crf_status} of the organ assessment defined as \code{"Complete"}. Source: \code{OrganLongitudinal}.
}
}
