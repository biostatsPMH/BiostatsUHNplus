#' Outputs R Markdown table of all and grade 3+ adverse events by comparison group
#'
#' @param comp baseline comparison group. For example, cohort (if provided)
#' @param cutDate recent cutoff date for AEs (i.e. 31AUG2023)
#' @param boundDate lower bound cutoff date for AEs (if provided)
#' @param subjID key identifier field for participant ID in data sets
#' @param subjID_ineligText character text that denotes participant IDs to exclude,
#'    for example, c("New Subject") (if provided)
#' @param baseline_datasets list of data frames that contain baseline participant characteristics,
#'    for example, list(enrollment_DF,demography_DF,ineligibility_DF)
#' @param ae_dataset data frame that contains subject AEs
#' @param ineligVar field that denotes participant ineligibility (if provided)
#' @param ineligVarText character text that denotes participant ineligibility,
#'    for example, c("Yes", "Y") (if provided)
#' @param genderVar field that denotes participant gender
#' @param enrolDtVar field that denotes participant enrollment date (i.e. 10MAY2021)
#' @param ae_detailVar field that denotes participant AE detail (lowest level term)
#' @param ae_severityVar field that denotes participant AE severity grade (numeric)
#' @param ae_onsetDtVar field that denotes participant AE onset date
#' @param ae_detailOtherText character text that denotes referencing verbatim AE field, 
#'   for example, c("Other, specify", "OTHER") (if provided)
#' @param ae_detailOtherVar field that denotes participant AE detail other (if provided)
#' @param ae_verbatimVar field that denotes participant AE detail verbatim (if provided)
#' @param ae_attribVars field(s) that denotes attribution to intervention under study. \cr 
#'    For example, c("CTC_AE_ATTR_SCALE","CTC_AE_ATTR_SCALE_1") 
#'    (if provided)
#' @param ae_attribVarsName character text that denotes name of interventions under study.
#'    For example, c("Drug 1", "Drug 2") (if provided)
#' @param ae_attribVarText character text that denotes related attribution. For example
#'    c("Definite", "Probable", "Possible") (if provided)
#' @param related_ae boolean that denotes if summary is for related AEs. Default is False.
#' @param numSubj vector to override value for number of participants in summary (if provided)
#' @keywords dataframe
#' @return R Markdown table of all and grade 3+ adverse events by treatment arm
#' @importFrom openxlsx createStyle createWorkbook addWorksheet writeData mergeCells addStyle setRowHeights setColWidths saveWorkbook
#' @importFrom plyr join_all
#' @importFrom dplyr select distinct mutate arrange summarise group_by filter if_any all_of across row_number n_distinct
#' @importFrom stringr str_detect
#' @export
#' @examples
#' data("enrollment", "demography", "ineligibility", "ae");
#' rm_ae_all_g3p(
#'   comp="COHORT",
#'   cutDate="31AUG2020",
#'   boundDate=NULL,subjID="Subject",subjID_ineligText=c("New Subject","Test"),
#'   baseline_datasets=list(enrollment,demography,ineligibility),
#'   ae_dataset=ae,ineligVar="INELIGIBILITY_STATUS",ineligVarText=c("Yes","Y"),
#'   genderVar="GENDER_CODE",enrolDtVar="ENROL_DATE_INT",ae_detailVar="ae_detail",
#'   ae_severityVar="AE_SEV_GD",
#'   ae_onsetDtVar="AE_ONSET_DT_INT",ae_detailOtherText="Other, specify",
#'   ae_detailOtherVar="CTCAE5_LLT_NM",ae_verbatimVar="AE_VERBATIM_TRM_TXT",
#'   ae_attribVars=c("CTC_AE_ATTR_SCALE","CTC_AE_ATTR_SCALE_1"),
#'   ae_attribVarsName=c("Drug 1","Drug 2"),
#'   ae_attribVarText=c("Definite", "Probable", "Possible"),
#'   numSubj=c(2,4,5,6))

rm_ae_all_g3p <- function(comp=NULL,pi,presDate,cutDate,boundDate=NULL,
                          subjID,subjID_ineligText=NULL,baseline_datasets,ae_dataset,
                          ineligVar=NULL,ineligVarText=NULL,
                          genderVar,enrolDtVar,ae_detailVar,ae_categoryVar=NULL,
                          ae_severityVar,ae_onsetDtVar,ae_detailOtherText=NULL,ae_detailOtherVar=NULL,
                          ae_verbatimVar=NULL,ae_attribVars=NULL,
                          ae_attribVarsName=NULL,ae_attribVarText=NULL,related_ae=FALSE,numSubj=NULL){
  
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#;
  
  #setwd(setwd);
  options(dplyr.summarise.inform = FALSE)
  if (is.null(boundDate)) {
    boundDate <- "01JAN1990";
  }
  if (is.null(ae_categoryVar)) {
    ae_categoryVar <- ae_detailVar;
  }
  if (is.null(ae_detailOtherVar)) {
    ae_detailOtherVar <- ae_detailVar;
  }
  if (is.null(ae_verbatimVar)) {
    ae_verbatimVar <- ae_detailVar;
  }
  if (is.null(ae_detailOtherText)) {
    ae_detailOtherText <- "Other, specify";
  }
  if (is.null(ineligVar)) {
    ineligVar <- subjID;
  }
  if (is.null(ineligVarText)) {
    ineligVarText <- "Not using this";
  }
  if (is.null(ae_attribVarText)) {
    ae_attribVarText <- c("Definite", "Probable", "Possible");
  }
  if (is.null(comp)) {
    subjectsKeep_DF <- plyr::join_all(baseline_datasets, by = subjID, type = "full") |>
      #### --------------------------------------------- ####
    #### Just modify the below line for variable names ####
    dplyr::mutate(Subject = eval(parse(text=subjID)), comp = "", gender_code = eval(parse(text=genderVar)), PT_ELIG_IND_3 = eval(parse(text=ineligVar)), PARTIC_ENROL_DT_INT = eval(parse(text=enrolDtVar))) |>
      dplyr::select(Subject, comp, gender_code, PT_ELIG_IND_3, PARTIC_ENROL_DT_INT) |>
      dplyr::group_by(Subject) |>
      dplyr::summarise(comp = comp[which(!is.na(comp))[1]], gender_code = gender_code[which(!is.na(gender_code))[1]], PT_ELIG_IND_3 = PT_ELIG_IND_3[which(!is.na(PT_ELIG_IND_3))[1]], PARTIC_ENROL_DT_INT = PARTIC_ENROL_DT_INT[which(!is.na(PARTIC_ENROL_DT_INT))[1]]) |>
      #### --------------------------------------------- ####
    dplyr::mutate(PARTIC_ENROL_DT_INT = toupper(format(as.Date(PARTIC_ENROL_DT_INT, tz = "UTC"), "%d%b%Y"))) |>
      dplyr::filter(!PT_ELIG_IND_3 %in% ineligVarText, !Subject %in% subjID_ineligText) |>
      dplyr::arrange(Subject)
  }
  if (!is.null(comp)) {
    subjectsKeep_DF <- plyr::join_all(baseline_datasets, by = subjID, type = "full") |>
      #### --------------------------------------------- ####
    #### Just modify the below line for variable names ####
    dplyr::mutate(Subject = eval(parse(text=subjID)), comp = eval(parse(text=comp)), gender_code = eval(parse(text=genderVar)), PT_ELIG_IND_3 = eval(parse(text=ineligVar)), PARTIC_ENROL_DT_INT = eval(parse(text=enrolDtVar))) |>
      dplyr::select(Subject, comp, gender_code, PT_ELIG_IND_3, PARTIC_ENROL_DT_INT) |>
      dplyr::group_by(Subject) |>
      dplyr::summarise(comp = comp[which(!is.na(comp))[1]], gender_code = gender_code[which(!is.na(gender_code))[1]], PT_ELIG_IND_3 = PT_ELIG_IND_3[which(!is.na(PT_ELIG_IND_3))[1]], PARTIC_ENROL_DT_INT = PARTIC_ENROL_DT_INT[which(!is.na(PARTIC_ENROL_DT_INT))[1]]) |>
      #### --------------------------------------------- ####
    dplyr::mutate(PARTIC_ENROL_DT_INT = toupper(format(as.Date(PARTIC_ENROL_DT_INT, tz = "UTC"), "%d%b%Y"))) |>
      dplyr::filter(!PT_ELIG_IND_3 %in% ineligVarText, !Subject %in% subjID_ineligText) |>
      dplyr::arrange(Subject) |>
      dplyr::filter(!is.na(comp))
  }
  
  aeKeep_DF <- ae_dataset |>
    #### --------------------------------------------- ####
  #### Just modify the below line for variable names ####
  dplyr::mutate(Subject = eval(parse(text=subjID)), ae_grade_code_dyn_std = eval(parse(text=ae_severityVar)), CTCAE5_LLT_NM = eval(parse(text=ae_detailOtherVar)), AE_VERBATIM_TRM_TXT = eval(parse(text=ae_verbatimVar)), AE_ONSET_DT_INT = eval(parse(text=ae_onsetDtVar)), ae_detail = eval(parse(text=ae_detailVar)), ae_category = eval(parse(text=ae_categoryVar))) |>
    dplyr::select(Subject, ae_grade_code_dyn_std, dplyr::all_of(ae_attribVars), CTCAE5_LLT_NM, AE_VERBATIM_TRM_TXT, AE_ONSET_DT_INT, ae_detail, ae_category) |>
    #### --------------------------------------------- ####
  dplyr::mutate(ae_detail = toupper(ifelse(stringr::str_detect(ae_detail, ae_detailOtherText), trimws(AE_VERBATIM_TRM_TXT), ae_detail)), AE_ONSET_DT_INT = toupper(format(as.Date(AE_ONSET_DT_INT, tz = "UTC"), "%d%b%Y")), ae_category = toupper(ae_category)) |>
    dplyr::mutate(ae_detail = toupper(ifelse(is.na(ae_detail), CTCAE5_LLT_NM, ae_detail))) |>
    dplyr::arrange(Subject)
  
  if (related_ae == TRUE) {
    aeKeep_DF <- aeKeep_DF |>
      dplyr::filter(dplyr::if_any(dplyr::all_of(ae_attribVars), ~ . %in% ae_attribVarText))
  }
  
  aes1_DF <- subjectsKeep_DF |> 
    dplyr::left_join(aeKeep_DF, by = "Subject") |>
    dplyr::select(Subject, comp, AE_ONSET_DT_INT, ae_detail, ae_category, ae_grade_code_dyn_std, dplyr::all_of(ae_attribVars), PARTIC_ENROL_DT_INT, CTCAE5_LLT_NM, AE_VERBATIM_TRM_TXT) |>
    dplyr::arrange(Subject) |>
    #dplyr::filter(as.Date(PARTIC_ENROL_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y"), as.Date(AE_ONSET_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y"), !is.na(ae_detail)) |>
    dplyr::filter(is.na(PARTIC_ENROL_DT_INT) | as.Date(PARTIC_ENROL_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y")) |>
    dplyr::filter(is.na(AE_ONSET_DT_INT) | as.Date(AE_ONSET_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y")) |>
    dplyr::distinct(Subject, comp, AE_ONSET_DT_INT, ae_detail, ae_category, ae_grade_code_dyn_std, PARTIC_ENROL_DT_INT) |>
    dplyr::filter(!ae_detail == "") |>
    dplyr::filter(!ae_category == "") |>
    #dplyr::filter(!ae_grade_code_dyn_std == "") |>
    dplyr::mutate(ae_grade_code_dyn_std = as.numeric(gsub("[^0-9.-]", "", ae_grade_code_dyn_std))) 
  #write.xlsx(aes1_DF, file=paste("aes1_DF", ".xlsx", sep=""), sheetName="AEs check", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE);
  #unique(aes1_DF$ae_detail);
  
  aes2_DF <- aes1_DF |>  
    #dplyr::distinct(Subject, ae_category, ae_detail, ae_grade_code_dyn_std) |>
    #dplyr::group_by(Subject, ae_category, ae_detail, ae_grade_code_dyn_std) |>
    #dplyr::filter(ae_grade_code_dyn_std == max(ae_grade_code_dyn_std)) |>
    dplyr::arrange(Subject) 
  
  # Get number of distinct subjects per group
  distinct_counts <- subjectsKeep_DF |>
    group_by(comp) |>
    summarise(n_distinct_subjects = n_distinct(Subject)) |>
    pull(n_distinct_subjects)
  
  if (!is.null(numSubj)) {
    distinct_counts <- numSubj;
  }
  
  totalSubj <- sum(distinct_counts)       
  col_totals <- c(totalSubj, rep(distinct_counts, each = 2))
  
  # Do All AEs and G3P AEs
  aes3_DF <- aes2_DF;
  aes2_DF$grpAsn2 <- paste(aes2_DF$comp, "_All", sep="");
  aes3_DF$grpAsn2 <- NA;
  aes3_DF$grpAsn2[which(aes3_DF$ae_grade_code_dyn_std %in% c(3, 4, 5))] <- paste(aes3_DF$comp[which(aes3_DF$ae_grade_code_dyn_std %in% c(3, 4, 5))], "_G3P", sep="");
  aes3_DF <- aes3_DF[-which(is.na(aes3_DF$grpAsn2)), ];
  aes3_DF <- rbind(aes2_DF, aes3_DF);
  
  #### Table of all and grade 3+ adverse events by comparison group;
  tab4a <- rm_covsum_nested(data = aes3_DF, id = c(subjID), covs = c("ae_detail"),  maincov = "grpAsn2", testcat = "Fisher", percentage = c("column"), show.tests = F, pvalue = F, effSize = F, full = T, IQR = F, nicenames = F, digits = 1, digits.cat = 1, tableOnly = T);
  tab4 <- rm_covsum_nested(data = aes3_DF, id = c(subjID, "ae_detail"), covs = c("ae_detail"),  maincov = "grpAsn2", testcat = "Fisher", percentage = c("column"), show.tests = F, pvalue = F, effSize = F, full = T, IQR = F, nicenames = F, digits = 1, digits.cat = 1, tableOnly = T);
  colnames(tab4) <- colnames(tab4a); #have to do this so participant count is correct;
  
  #should work to sort in descending order;
  first_row <- tab4[1, ];
  rest_sorted <- tab4[-1, ][order(-as.numeric(sapply(strsplit(tab4[-1, 2], " "), `[`, 1))), ];
  tab4  <- rbind(first_row, rest_sorted);  
  
  # Remove all text in parentheses from character columns
  tab4[] <- lapply(tab4, function(x) {
    if (is.character(x)) {
      gsub("\\s*\\([^\\)]+\\)", "", x)
    } else {
      x
    }
  })
  
  # Remove text in parentheses from column names
  names(tab4) <- gsub("\\s*\\([^\\)]+\\)", "", names(tab4))
  
  # Remove the first row
  fr_tab4 <- tab4[1, ]
  tab4 <- tab4[-1, ]
  
  # Convert to numeric
  tab4[, -1] <- lapply(tab4[, -1], as.numeric)
  
  # Append totals to column names
  names(fr_tab4) <- c("Covariate", paste0(names(fr_tab4[-1]), " (n=", col_totals, ")")) 
  
  # Create a new dataframe with counts and percentages
  tab4_percent <- as.data.frame(mapply(function(col, total) {
    paste0(col, " (", sprintf("%.1f", 100 * col / total), ")")
  }, tab4[,-1], col_totals, SIMPLIFY = FALSE))
  
  tab4_percent <- cbind(tab4[, 1], tab4_percent)
  names(tab4_percent) <- names(fr_tab4) 
  tab4 <- rbind(fr_tab4, tab4_percent)
  
  reportRmd::outTable(tab=tab4)
}