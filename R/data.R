#'  Simulated adverse events for patients receiving two study agents. 
#'  
#' @format A data frame with 394 rows and 9 variables:
#' \describe{ 
#'   \item{Subject}{Patient ID}
#'   \item{ae_detail}{Adverse event detail, also known as lowest level term}
#'   \item{ae_category}{Adverse event category, also knoa as system organ class}
#'   \item{CTCAE5_LLT_NM}{Common Terminology Criteria for Adverse Events (CTCAE) version 5}
#'   \item{AE_VERBATIM_TRM_TXT}{Adverse event verbatim text entered by clinical registered nurse, for "Other, specify"}
#'   \item{AE_SEV_GD}{Adverse event severity grade, scale from 1 to 5}
#'   \item{AE_ONSET_DT_INT}{Adverse event onset date}
#'   \item{CTC_AE_ATTR_SCALE}{Attribution scale of adverse event to first study agent}
#'   \item{CTC_AE_ATTR_SCALE_1}{Attribution scale of adverse event to second study agent}
#' } 
"ae" 

#'  Simulated demography for patients. 
#'  
#' @format A data frame with 12 rows and 2 variables:
#' \describe{ 
#'   \item{Subject}{Patient ID}
#'   \item{GENDER_CODE}{Patient gender}
#' } 
"demography" 

#'  Enrollment data 
#'  
#'  Simulated enrollment for patients. 
#'  
#' @format A data frame with 12 rows and 3 variables:
#' \describe{ 
#'   \item{Subject}{Patient ID}
#'   \item{COHORT}{Study cohort for patient}
#'   \item{ENROL_DATE_INT}{Enrollment date of patient to study}
#' } 
"enrollment" 

#'  Simulated ineligibility for patients. 
#'  
#' @format A data frame with 11 rows and 2 variables:
#' \describe{ 
#'   \item{Subject}{Patient ID}
#'   \item{INELIGIBILITY_STATUS}{Recorded ineligibility status of patient to study}
#' } 
"ineligibility" 

#'  Simulated study agent 1 for patients. 
#'  
#' @format A data frame with 12 rows and 2 variables:
#' \describe{ 
#'   \item{Subject}{Patient ID}
#'   \item{TX1_DATE_INT}{Study agent 1 start date of patient on study}
#' } 
"drug1_admin" 

#'  Simulated study agent 2 for patients. 
#'  
#' @format A data frame with 12 rows and 2 variables:
#' \describe{ 
#'   \item{Subject}{Patient ID}
#'   \item{TX2_DATE_INT}{Study agent 2 start date of patient on study}
#' } 
"drug2_admin" 