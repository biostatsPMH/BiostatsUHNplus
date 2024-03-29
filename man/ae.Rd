% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ae}
\alias{ae}
\title{Simulated adverse events for patients receiving two study agents.}
\format{
A data frame with 394 rows and 9 variables:
\describe{
\item{Subject}{Patient ID}
\item{ae_detail}{Adverse event detail, also known as lowest level term}
\item{ae_category}{Adverse event category, also known as system organ class}
\item{CTCAE5_LLT_NM}{Common Terminology Criteria for Adverse Events (CTCAE) version 5}
\item{AE_VERBATIM_TRM_TXT}{Adverse event verbatim text entered by clinical registered nurse, for "Other, specify"}
\item{AE_SEV_GD}{Adverse event severity grade, scale from 1 to 5}
\item{AE_ONSET_DT_INT}{Adverse event onset date}
\item{CTC_AE_ATTR_SCALE}{Attribution scale of adverse event to first study agent}
\item{CTC_AE_ATTR_SCALE_1}{Attribution scale of adverse event to second study agent}
}
}
\usage{
ae
}
\description{
Simulated adverse events for patients receiving two study agents.
}
\keyword{datasets}
