% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailored_transplantationbl.R
\name{tailored_transplantationbl}
\alias{tailored_transplantationbl}
\title{Tailored analysis tables with with baseline transplantation data.}
\usage{
tailored_transplantationbl(stcs)
}
\arguments{
\item{stcs}{A list containing the STCS data frame.}
}
\value{
A data frame.
}
\description{
Tailored analysis tables with with baseline transplantation data.
}
\details{
\code{tailored_transplantationbl()}: PK: \code{soaskey}.


\itemize{

\item{1.} \code{soaskey}: Identifier of transplantations. Source: \code{Transplantation}.
\item{2.} \code{patientkey}: Identifier of patients. Source: \code{Transplantation}.
\item{3.} \code{donorkey}: Identifier of donors. Source: \code{Transplantation}.
\item{4.} \code{organ_count}: Number of organs in the tpx. Source: \code{Transplantation}.
\item{5.} \code{tpx}: List of organs. Source: \code{Transplantation}.
\item{6.} \code{tpx_resec}: transplantation level Re, Sec. Source: \code{Transplantation}.
\item{7.} \code{pre_tpxspecific_count}: Number of pre-transplantation (\code{tpx} specific). Source: \code{Transplantation}.
\item{8.} \code{tpxspecific_order}: Order of transplantion (\code{tpx} specific). Source : \code{Transplantation}.
\item{9.} \code{tpx_order}: Order of transplantion. Source: \code{Transplantation}.
\item{10.} \code{tpx_stcsorder}: Order of transplantion within the STCS. Source: \code{Transplantation}.
\item{11.} \code{abocomp}: Blood groups compatibility. Source: \code{Transplantation}.
\item{12.-18.} \code{hla...mismatch}: HLA mismatch. Source: \code{Transplantation}.
\item{19.} \code{sumhlamismatch}: Sum of mismatch. Source: \code{Transplantation}.
\item{20.} \code{hlaavailable}: Number of HLA mismatch available. Source: \code{Transplantation}.
\item{21.} \code{procedure_provider}: Procedure provider. Source: \code{Transplantation}.
\item{22.} \code{dontype}: Donor type. Source: \code{Donor}.
\item{23.} \code{donrelsubtype}: Donor subtype. Source: \code{Donor}.
\item{24.} \code{identical_twins}: Identical twins. Source: \code{Donor}.
\item{25.} \code{kpd}: Kidney paired donation. Source: \code{Donor}.
\item{26.} \code{altruistic}: Altruistic donation. Source: \code{Donor}.
\item{27.} \code{don_bg}: Blood-group of the donor. Source: \code{Donor}.
\item{28.} \code{don_sex}: Sex of the donor. Source: \code{Donor}.
\item{29.} \code{doncod}: Cause of donor death. Source: \code{Donor}.
\item{30.} \code{doncod_comment}: Comment on cause of donor death. Source: \code{Donor}.
\item{31.} \code{donage}: Age of donor. Source: \code{Donor}.
\item{32.} \code{donage_complete}: Calculated age of the donor. Source: \code{Donor}.
\item{33.-43.} \code{rec_anti...}: (Optional) Recipient Serology. Source: \code{PatientSerology}.
\item{44.-54.} \code{don_anti...}: (Optional) Donor Serology. Source: \code{DonorSerology}.
\item{55.-65.} \code{combi_anti...}: (Optional) Serology combination. Source: derived.

}
}
