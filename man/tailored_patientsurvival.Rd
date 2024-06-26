% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailored_patientsurvival.R
\name{tailored_patientsurvival}
\alias{tailored_patientsurvival}
\title{Tailored analysis tables with main dates related to patients}
\usage{
tailored_patientsurvival(stcs)
}
\arguments{
\item{stcs}{A list containing the STCS data frame.}
}
\value{
A data frame.
}
\description{
Tailored analysis tables with main dates related to patients
}
\details{
\code{tailored_patientsurvival()}: PK: \code{patientkey}.

\itemize{

\item{1.} \code{patientkey}: Identifier of patients. Source: \code{Patient}.
\item{2.} \code{enrollment_date}: Enrollment date. Source: \code{Patient}.
\item{3.} \code{deathdate}: Death date. Source: \code{Stop}.
\item{4.} \code{first_dropoutdate}: First dropout date. Source: \code{Stop}.
\item{5.} \code{first_dropoutdateaccuracy}: Accuracy of the first dropout date. Source: \code{Stop}.
\item{6.} \code{last_activedropoutdate}: Last dropout date without \code{backstcs}. Source: \code{Stop}.
\item{7.} \code{last_activedropoutdateaccuracy}: Accuracy of the last dropout date without \code{backstcs}. Source: \code{Stop}.
\item{8.} \code{last_patlongkeys}: Identifier of the last patient-assessment. Source: \code{PatientLongitudinal}.
\item{9.} \code{last_assdate}: Date of the last patient-assessment. Source: \code{PatientLongitudinal}.
\item{10.-17.} \code{last_[...]_toggle_date}: Assessment date of the last mentioned filled toggle (keep: \code{"Yes"}, \code{"No"}, removed: \code{"Missing"}, \code{NA}). Source: \code{PatientLongitudinal}.
\item{18.} \code{extraction_date}: Extraction date. Source: \code{Admin}.
\item{19.} \code{last_complete_patientlongitudinal_crf_date}: Assessment date of the latest \code{crf_status} of the patient assessment defined as \code{"Complete"}. Source: \code{PatientLongitudinal}.
\item{20.} \code{last_complete_stop_crf_date}: Date (latest between: \code{deathdate}, \code{dropoutdate}, \code{lastalivedate}, \code{backstcsdate}) of the latest \code{crf_status} defined as \code{"Complete"}. Source: \code{Stop}.
\item{21.} \code{last_consent_withdrawal_date}: Last consent date when the its consent status is \code{"Withdrawal"}. Source \code{Patient}.
\item{22.} \code{confidental data}: Optional.
\item{23.} \code{last_complete_psq_crf_date}: Optional. Date of the latest \code{crf_status} of PSQ defined as \code{"Complete"}. Source: \code{PSQ}.

}
}
