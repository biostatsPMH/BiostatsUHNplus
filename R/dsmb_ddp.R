#' Outputs the DSMB-DDP AE summary tables in Excel format per NCI template
#'
#' @param protocol study protocol name (uppercase, no spaces permitted)
#' @param setwd directory to write Excel summary files to
#' @param title full character vector with name of study
#' @param comp baseline comparison group. For example, cohort (if provided)
#' @param pi character vector name of study principal investigator
#' @param presDate presentation date (i.e. 17NOV2023) for DSMB
#' @param cutDate recent cutoff date for AEs (i.e. 31AUG2023)
#' @param boundDate lower bound cutoff date for AEs (if provided)
#' @param subjID key identifier field for participant ID in data sets
#' @param subjID_ineligText character text that denotes participant IDs to exclude.
#'    For example, c("New Subject") (if provided)
#' @param baseline_datasets list of data frames that contain baseline participant characteristics.
#'    For example, list(enrollment_DF,demography_DF,ineligibility_DF)
#' @param ae_dataset data frame that contains subject AEs
#' @param ineligVar field that denotes participant ineligibility (if provided)
#' @param ineligVarText character text that denotes participant ineligibility.
#'    For example, c("Yes", "Y") (if provided)
#' @param genderVar field that denotes participant gender
#' @param enrolDtVar field that denotes participant enrollment date (i.e. 10MAY2021)
#' @param ae_detailVar field that denotes participant AE detail (lowest level term)
#' @param ae_categoryVar field that denotes participant AE category (system organ class)
#' @param ae_severityVar field that denotes participant AE severity grade (numeric)
#' @param ae_onsetDtVar field that denotes participant AE onset date
#' @param ae_detailOtherText character text that denotes referencing verbatim AE field. 
#'   For example, c("Other, specify", "OTHER") (if provided)
#' @param ae_detailOtherVar field that denotes participant AE detail other (if provided)
#' @param ae_verbatimVar field that denotes participant AE detail verbatim (if provided)
#' @param ae_attribVars field(s) that denotes attribution to intervention under study. \cr 
#'   For example, c("CTC_AE_ATTR_SCALE","CTC_AE_ATTR_SCALE_1") (if provided)
#' @param ae_attribVarsName character text that denotes name of interventions under study.
#'   For example, c("Drug 1", "Drug 2") (if provided)
#' @param ae_attribVarText character text that denotes related attribution. For example
#'   c("Definite", "Probable", "Possible") (if provided)
#' @param related_ae boolean that denotes if summary is for related AEs. Default is False.
#' @param numSubj vector to override value for number of participants in summary (if provided)
#' @param fileNameUnderscore boolean that denotes if spaces should be underscore in filename
#' @keywords dataframe
#' @return three Excel files containing DSMB-CCRU AE summary tables
#' @importFrom openxlsx createStyle createWorkbook addWorksheet writeData mergeCells addStyle setRowHeights setColWidths saveWorkbook
#' @importFrom plyr join_all
#' @importFrom dplyr select distinct mutate arrange summarise group_by filter across row_number n_distinct
#' @importFrom stringr str_detect
#' @export
#' @examples
#' data("enrollment", "demography", "ineligibility", "ae");
#' dsmb_ccru(protocol="EXAMPLE_STUDY",setwd="./man/tables/",
#'   title="Phase X Study to Evaluate Treatments A-D",
#'   comp="COHORT",pi="Dr. Principal Investigator",
#'   presDate="30OCT2020",cutDate="31AUG2020",
#'   boundDate=NULL,subjID="Subject",subjID_ineligText=c("New Subject","Test"),
#'   baseline_datasets=list(enrollment,demography,ineligibility),
#'   ae_dataset=ae,ineligVar="INELIGIBILITY_STATUS",ineligVarText=c("Yes","Y"),
#'   genderVar="GENDER_CODE",enrolDtVar="ENROL_DATE_INT",ae_detailVar="ae_detail",
#'   ae_categoryVar="ae_category",ae_severityVar="AE_SEV_GD",
#'   ae_onsetDtVar="AE_ONSET_DT_INT",ae_detailOtherText="Other, specify",
#'   ae_detailOtherVar="CTCAE5_LLT_NM",ae_verbatimVar="AE_VERBATIM_TRM_TXT",
#'   ae_attribVars=c("CTC_AE_ATTR_SCALE","CTC_AE_ATTR_SCALE_1"),
#'   ae_attribVarsName=c("Drug 1","Drug 2"),
#'   ae_attribVarText=c("Definite", "Probable", "Possible"),
#'   numSubj=c(2,4,5,6))

dsmb_ddp <- function(protocol,setwd,title,comp=NULL,pi,presDate,cutDate,boundDate=NULL,
                      subjID,subjID_ineligText=NULL,baseline_datasets,ae_dataset,
                      ineligVar=NULL,ineligVarText=NULL,
                      genderVar,enrolDtVar,ae_detailVar,ae_categoryVar,
                      ae_severityVar,ae_onsetDtVar,ae_detailOtherText=NULL,ae_detailOtherVar=NULL,
                      ae_verbatimVar=NULL,ae_attribVars=NULL,
                      ae_attribVarsName=NULL,ae_attribVarText=NULL,related_ae=FALSE,
                      numSubj=NULL,fileNameUnderscore=TRUE){
  
  #### Template style for tables;
  ##https://stackoverflow.com/questions/54322814/how-to-apply-thick-border-around-a-cell-range-using-the-openxlsx-package-in-r ;
  OutsideBorders <-
    function(wb_,
             sheet_,
             rows_,
             cols_,
             border_col = "black",
             border_thickness = "thick") {
      left_col = min(cols_)
      right_col = max(cols_)
      top_row = min(rows_)
      bottom_row = max(rows_)
      sub_rows <- list(c(bottom_row:top_row),
                       c(bottom_row:top_row),
                       top_row,
                       bottom_row)
      sub_cols <- list(left_col,
                       right_col,
                       c(left_col:right_col),
                       c(left_col:right_col))
      directions <- list("Left", "Right", "Top", "Bottom")
      mapply(function(r_, c_, d) {
        temp_style <- createStyle(border = d,
                                  borderColour = border_col,
                                  borderStyle = border_thickness)
        addStyle(
          wb_,
          sheet_,
          style = temp_style,
          rows = r_,
          cols = c_,
          gridExpand = TRUE,
          stack = TRUE
        )
      }, sub_rows, sub_cols, directions)
    }
  
  
  backgroundStyle <- createStyle(fontName = "Arial", fontSize = 9, fontColour = "black",
                                 halign = "center", valign = "center", fgFill = "white", 
                                 border = NULL, borderColour = "#AAC1D9",
                                 textDecoration = "bold", wrapText = TRUE, borderStyle = NULL)
  headerStyle1 <- createStyle(fontName = "Arial", fontSize = 12, fontColour = "black",
                              halign = "center", valign = "center", fgFill = "white", 
                              border = NULL, borderColour = "#AAC1D9",
                              textDecoration = "bold", wrapText = TRUE, borderStyle = NULL)
  headerStyle2 <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "black",
                              halign = "center", valign = "center", fgFill = "white", 
                              border = "TopBottomLeftRight", borderColour = "black",
                              textDecoration = "bold", wrapText = TRUE)
  contentStyleR <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "black",
                               halign = "right", valign = "center", fgFill = "white", 
                               border = "LeftRight", borderColour = "black",
                               wrapText = TRUE)
  contentStyleL <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "black",
                               halign = "left", valign = "center", fgFill = "white", 
                               border = "LeftRight", borderColour = "black",
                               wrapText = TRUE)
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#;
  
  #setwd(setwd);
  
  if (is.null(boundDate)) {
    boundDate <- "01JAN1990";
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
  if (related_ae == TRUE & is.null(ae_attribVars)) {
    stop("ae_attribVars is a required argument when related_ae=TRUE")
  }
  if (!is.null(numSubj)) {
    numSubj <- c(sum(numSubj), numSubj)
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
    subjectsKeep_DFa <- subjectsKeep_DF
    subjectsKeep_DFa$comp = ""
    subjectsKeep_DF <- rbind(subjectsKeep_DFa, subjectsKeep_DF)
  }
  
  
  # All AEs by comp;
  sheetNamesA <- NULL;
  #i <- 1;
  #i <- 5;
  for (i in 1:length(unique(subjectsKeep_DF[["comp"]]))) {
    comp <- unique(subjectsKeep_DF[["comp"]])[i]
    tryCatch({
      subjects_DF <- subjectsKeep_DF[which(subjectsKeep_DF$comp %in% c(comp)), ] 
    }, error=function(e){})
    
    aeKeep_DF <- ae_dataset |>
      #### --------------------------------------------- ####
    #### Just modify the below line for variable names ####
    dplyr::mutate(Subject = eval(parse(text=subjID)), ae_grade_code_dyn_std = eval(parse(text=ae_severityVar)), CTCAE5_LLT_NM = eval(parse(text=ae_detailOtherVar)), AE_VERBATIM_TRM_TXT = eval(parse(text=ae_verbatimVar)), AE_ONSET_DT_INT = eval(parse(text=ae_onsetDtVar)), ae_detail = eval(parse(text=ae_detailVar)), ae_category = eval(parse(text=ae_categoryVar))) |>
      dplyr::select(Subject, ae_grade_code_dyn_std, CTCAE5_LLT_NM, AE_VERBATIM_TRM_TXT, AE_ONSET_DT_INT, ae_detail, ae_category) |>
      #### --------------------------------------------- ####
    dplyr::mutate(ae_detail = toupper(ifelse(stringr::str_detect(ae_detail, ae_detailOtherText), trimws(AE_VERBATIM_TRM_TXT), ae_detail)), AE_ONSET_DT_INT = toupper(format(as.Date(AE_ONSET_DT_INT, tz = "UTC"), "%d%b%Y")), ae_category = toupper(ae_category)) |>
      dplyr::mutate(ae_detail = toupper(ifelse(is.na(ae_detail), CTCAE5_LLT_NM, ae_detail))) |>
      dplyr::arrange(Subject)
    
    aes1_DF <- subjects_DF |> 
      dplyr::left_join(aeKeep_DF, by = "Subject") |>
      dplyr::select(Subject, AE_ONSET_DT_INT, ae_detail, ae_category, ae_grade_code_dyn_std, PARTIC_ENROL_DT_INT, CTCAE5_LLT_NM, AE_VERBATIM_TRM_TXT) |>
      dplyr::arrange(Subject) |>
      #dplyr::filter(as.Date(PARTIC_ENROL_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y"), as.Date(AE_ONSET_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y"), !is.na(ae_detail)) |>
      dplyr::filter(is.na(PARTIC_ENROL_DT_INT) | as.Date(PARTIC_ENROL_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y")) |>
      dplyr::filter(is.na(AE_ONSET_DT_INT) | as.Date(AE_ONSET_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y")) |>
      dplyr::distinct(Subject, AE_ONSET_DT_INT, ae_detail, ae_category, ae_grade_code_dyn_std, PARTIC_ENROL_DT_INT) |>
      dplyr::filter(!ae_detail == "") |>
      dplyr::filter(!ae_category == "") |>
      #dplyr::filter(!ae_grade_code_dyn_std == "") |>
      dplyr::mutate(ae_grade_code_dyn_std = as.numeric(gsub("[^0-9.-]", "", ae_grade_code_dyn_std))) 
    
    total_subj_count <- length(unique(subjects_DF[which(!is.na(subjects_DF$gender_code)), ]$Subject));
    if (!is.null(numSubj)) {
      total_subj_count <- numSubj[i];
    }
    
    total_ae_count <- length(aes1_DF$ae_category);
    
    #### Table;
    aes1_DF <- aes1_DF |>
      dplyr::mutate(AE_Grade = ae_grade_code_dyn_std, AE_Detail = ae_detail, AE_Class = ae_category, Study = paste(protocol, sep=""))
      
    # Below takes worst AE for subject by AE detail for each study;
    all_AEs_subject <- aes1_DF %>% 
      dplyr::select(AE_Grade, AE_Detail, Study, Subject) %>%
      dplyr::group_by(Study, AE_Detail, AE_Grade, Subject) %>%   
      dplyr::arrange(Study, Subject, AE_Detail, desc(AE_Grade)) %>%
      dplyr::group_by(Study, Subject, AE_Detail) %>% 
      dplyr::filter(row_number()==1) #takes the top (highest grade) per subject per study

    # Get number of distinct subjects per AE grade
    levelsStrat <- sort(c("1", "2", "3", "4", "5"));
    all_AEs_subject$AE_Grade <- factor(all_AEs_subject$AE_Grade, levels = levelsStrat);
    
    distinct_counts <- all_AEs_subject |>
      group_by(AE_Grade, .drop=FALSE) |>
      summarise(n_distinct_subjects = n_distinct(Subject)) |>
      pull(n_distinct_subjects)
    
    totalAESubj <- all_AEs_subject |>
      group_by(Study) |>
      summarise(n_distinct_subjects = n_distinct(Subject)) |>
      pull(n_distinct_subjects)
    
    #total_subj_count;
    
    col_AE_totals <- c(totalAESubj, distinct_counts)
    
    col_totals <- c(rep(total_subj_count, each = 6))
    
    tab4 <- BiostatsUHNplus:::covsum_nested(data = all_AEs_subject, id = c("Subject", "AE_Detail"), covs = c("AE_Detail"),  maincov = "AE_Grade", testcat = "Fisher", percentage = c("column"), show.tests = F, pvalue = F, effSize = F, full = T, IQR = F, digits = 1, digits.cat = 1, sanitize = FALSE, dropLevels = FALSE, nicenames = TRUE);
    
    # Should work to sort in descending order;
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
    names(fr_tab4) <- c("Covariate", paste0(names(fr_tab4[-1]), " (n=", col_AE_totals, ")")) 
    
    # Create a new dataframe with counts and percentages
    tab4_percent <- as.data.frame(mapply(function(col, total) {
      paste0(col, " (", sprintf("%.1f", 100 * col / total), ")")
    }, tab4[,-1], col_totals, SIMPLIFY = FALSE))
    
    tab4_percent <- cbind(tab4[, 1], tab4_percent)
    names(tab4_percent) <- names(fr_tab4) 
    tab4 <- rbind(fr_tab4, tab4_percent)
    #tab4 <- tab4_percent
    
    to_indent <- which(tab4$Covariate %in% grep("^~~~", tab4$Covariate, value = TRUE))
    to_bold_name <- which(!tab4$Covariate %in% grep("^~~~", tab4$Covariate, value = TRUE))
    bold_cells <- arrayInd(to_bold_name, dim(tab4))
    
    #tab4$Covariate[1] <- gsub("\\\\textbf\\{", "", tab4$Covariate[1])
    #tab4$Covariate[1] <- substr(tab4$Covariate[1], 1, nchar(tab4$Covariate[1]) - 1)
    tab4$Covariate[1] <- "MedDRA Lowest Level Term"
    tab4$Covariate <- gsub("^~~~", "", tab4$Covariate)
    
    tmp <- cbind(tab4[,c(1,3:7)], tab4[,c(2)])
    tmp <-  tmp[-1, ]
    cn1 <- "MedDRA Lowest Level Term"
    cn2 <- paste0("# of patients Grade 1", " (n=", col_AE_totals[2], ")")
    cn3 <- paste0("# of patients Grade 2", " (n=", col_AE_totals[3], ")")
    cn4 <- paste0("# of patients Grade 3", " (n=", col_AE_totals[4], ")")
    cn5 <- paste0("# of patients Grade 4", " (n=", col_AE_totals[5], ")")
    cn6 <- paste0("# of patients Grade 5", " (n=", col_AE_totals[6], ")")
    cn7 <- paste0("Total # of patients", " (n=", col_AE_totals[1], " out of ", total_subj_count, ")")
    
    colnames(tmp) <- c(cn1, cn2, cn3, cn4, cn5, cn6, cn7)
    tmpTN <- paste("All_AEs_", comp, sep="");
    tmpTN <- gsub(" ", "_", tmpTN);
    
    assign(tmpTN, tmp);
    sheetNamesA[i] <- tmpTN;
  }  
  
  
  # Rel AEs by comp;
  sheetNamesR <- NULL;
  if (related_ae == TRUE & !is.null(ae_attribVars)) {
    #i <- 1;
    for (i in 1:length(unique(subjectsKeep_DF[["comp"]]))) {
      comp <- unique(subjectsKeep_DF[["comp"]])[i]
      tryCatch({
        subjects_DF <- subjectsKeep_DF[which(subjectsKeep_DF$comp %in% c(comp)), ] 
      }, error=function(e){})
      
      aeKeep_DF <- ae_dataset |>
        #### --------------------------------------------- ####
      #### Just modify the below line for variable names ####
      dplyr::mutate(Subject = eval(parse(text=subjID)), ae_grade_code_dyn_std = eval(parse(text=ae_severityVar)), CTCAE5_LLT_NM = eval(parse(text=ae_detailOtherVar)), AE_VERBATIM_TRM_TXT = eval(parse(text=ae_verbatimVar)), AE_ONSET_DT_INT = eval(parse(text=ae_onsetDtVar)), ae_detail = eval(parse(text=ae_detailVar)), ae_category = eval(parse(text=ae_categoryVar))) |>
        dplyr::select(Subject, ae_grade_code_dyn_std, dplyr::all_of(ae_attribVars), CTCAE5_LLT_NM, AE_VERBATIM_TRM_TXT, AE_ONSET_DT_INT, ae_detail, ae_category) |>
        #### --------------------------------------------- ####
      dplyr::mutate(ae_detail = toupper(ifelse(stringr::str_detect(ae_detail, ae_detailOtherText), trimws(AE_VERBATIM_TRM_TXT), ae_detail)), AE_ONSET_DT_INT = toupper(format(as.Date(AE_ONSET_DT_INT, tz = "UTC"), "%d%b%Y")), ae_category = toupper(ae_category)) |>
        dplyr::mutate(ae_detail = toupper(ifelse(is.na(ae_detail), CTCAE5_LLT_NM, ae_detail))) |>
        dplyr::arrange(Subject)
      
      aeKeep_DF <- aeKeep_DF |>
        dplyr::filter(dplyr::if_any(dplyr::all_of(ae_attribVars), ~ . %in% ae_attribVarText))
      
      aes1_DF <- subjects_DF |> 
        dplyr::left_join(aeKeep_DF, by = "Subject") |>
        dplyr::select(Subject, AE_ONSET_DT_INT, ae_detail, ae_category, ae_grade_code_dyn_std, PARTIC_ENROL_DT_INT, CTCAE5_LLT_NM, AE_VERBATIM_TRM_TXT) |>
        dplyr::arrange(Subject) |>
        #dplyr::filter(as.Date(PARTIC_ENROL_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y"), as.Date(AE_ONSET_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y"), !is.na(ae_detail)) |>
        dplyr::filter(is.na(PARTIC_ENROL_DT_INT) | as.Date(PARTIC_ENROL_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y")) |>
        dplyr::filter(is.na(AE_ONSET_DT_INT) | as.Date(AE_ONSET_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y")) |>
        dplyr::distinct(Subject, AE_ONSET_DT_INT, ae_detail, ae_category, ae_grade_code_dyn_std, PARTIC_ENROL_DT_INT) |>
        dplyr::filter(!ae_detail == "") |>
        dplyr::filter(!ae_category == "") |>
        #dplyr::filter(!ae_grade_code_dyn_std == "") |>
        dplyr::mutate(ae_grade_code_dyn_std = as.numeric(gsub("[^0-9.-]", "", ae_grade_code_dyn_std))) 
      
      total_subj_count <- length(unique(subjects_DF[which(!is.na(subjects_DF$gender_code)), ]$Subject));
      if (!is.null(numSubj)) {
        total_subj_count <- numSubj[i];
      }
      
      total_ae_count <- length(aes1_DF$ae_category);
      
      #### Table;
      aes1_DF <- aes1_DF |>
        dplyr::mutate(AE_Grade = ae_grade_code_dyn_std, AE_Detail = ae_detail, AE_Class = ae_category, Study = paste(protocol, sep=""))
      
      # Below takes worst AE for subject by AE detail for each study;
      all_AEs_subject <- aes1_DF %>% 
        dplyr::select(AE_Grade, AE_Detail, Study, Subject) %>%
        dplyr::group_by(Study, AE_Detail, AE_Grade, Subject) %>%   
        dplyr::arrange(Study, Subject, AE_Detail, desc(AE_Grade)) %>%
        dplyr::group_by(Study, Subject, AE_Detail) %>% 
        dplyr::filter(row_number()==1) #takes the top (highest grade) per subject per study
      
      # Get number of distinct subjects per AE grade
      levelsStrat <- sort(c("1", "2", "3", "4", "5"));
      all_AEs_subject$AE_Grade <- factor(all_AEs_subject$AE_Grade, levels = levelsStrat);
      
      distinct_counts <- all_AEs_subject |>
        group_by(AE_Grade, .drop=FALSE) |>
        summarise(n_distinct_subjects = n_distinct(Subject)) |>
        pull(n_distinct_subjects)
      
      totalAESubj <- all_AEs_subject |>
        group_by(Study) |>
        summarise(n_distinct_subjects = n_distinct(Subject)) |>
        pull(n_distinct_subjects)
      
      #total_subj_count;
      
      col_AE_totals <- c(totalAESubj, distinct_counts)
      
      col_totals <- c(rep(total_subj_count, each = 6))
      
      tab4 <- BiostatsUHNplus:::covsum_nested(data = all_AEs_subject, id = c("Subject", "AE_Detail"), covs = c("AE_Detail"),  maincov = "AE_Grade", testcat = "Fisher", percentage = c("column"), show.tests = F, pvalue = F, effSize = F, full = T, IQR = F, digits = 1, digits.cat = 1, sanitize = FALSE, dropLevels = FALSE, nicenames = TRUE);
      
      # Should work to sort in descending order;
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
      names(fr_tab4) <- c("Covariate", paste0(names(fr_tab4[-1]), " (n=", col_AE_totals, ")")) 
      
      # Create a new dataframe with counts and percentages
      tab4_percent <- as.data.frame(mapply(function(col, total) {
        paste0(col, " (", sprintf("%.1f", 100 * col / total), ")")
      }, tab4[,-1], col_totals, SIMPLIFY = FALSE))
      
      tab4_percent <- cbind(tab4[, 1], tab4_percent)
      names(tab4_percent) <- names(fr_tab4) 
      tab4 <- rbind(fr_tab4, tab4_percent)
      #tab4 <- tab4_percent
      
      to_indent <- which(tab4$Covariate %in% grep("^~~~", tab4$Covariate, value = TRUE))
      to_bold_name <- which(!tab4$Covariate %in% grep("^~~~", tab4$Covariate, value = TRUE))
      bold_cells <- arrayInd(to_bold_name, dim(tab4))
      
      #tab4$Covariate[1] <- gsub("\\\\textbf\\{", "", tab4$Covariate[1])
      #tab4$Covariate[1] <- substr(tab4$Covariate[1], 1, nchar(tab4$Covariate[1]) - 1)
      tab4$Covariate[1] <- "MedDRA Lowest Level Term"
      tab4$Covariate <- gsub("^~~~", "", tab4$Covariate)
      
      tmp <- cbind(tab4[,c(1,3:7)], tab4[,c(2)])
      tmp <-  tmp[-1, ]
      cn1 <- "MedDRA Lowest Level Term"
      cn2 <- paste0("# of patients Grade 1", " (n=", col_AE_totals[2], ")")
      cn3 <- paste0("# of patients Grade 2", " (n=", col_AE_totals[3], ")")
      cn4 <- paste0("# of patients Grade 3", " (n=", col_AE_totals[4], ")")
      cn5 <- paste0("# of patients Grade 4", " (n=", col_AE_totals[5], ")")
      cn6 <- paste0("# of patients Grade 5", " (n=", col_AE_totals[6], ")")
      cn7 <- paste0("Total # of patients", " (n=", col_AE_totals[1], " out of ", total_subj_count, ")")
      
      colnames(tmp) <- c(cn1, cn2, cn3, cn4, cn5, cn6, cn7)
      tmpTN <- paste("Rel_AEs_", comp, sep="");
      tmpTN <- gsub(" ", "_", tmpTN);
      
      assign(tmpTN, tmp);
      sheetNamesR[i] <- tmpTN;
    } 
  }
  
  
  # Combine sheet names so they alternate
  sheetNames <- as.vector(rbind(sheetNamesA, sheetNamesR))
  
  list_of_datasets <- lapply(sheetNames, function(x) get(x, mode="list"));
  names(list_of_datasets) <- c(sheetNames);
  
  
  # Rename dataset, if it exists
  old_name <- "All_AEs_"
  new_name <- "All_AEs"
  if (old_name %in% names(list_of_datasets)) {
    names(list_of_datasets)[names(list_of_datasets) == old_name] <- new_name
  }
  old_name <- "Rel_AEs_"
  new_name <- "Rel_AEs"
  if (old_name %in% names(list_of_datasets)) {
    names(list_of_datasets)[names(list_of_datasets) == old_name] <- new_name
  }
  
  names(list_of_datasets) <- make.unique(names(list_of_datasets), sep = '_')
  #openxlsx::write.xlsx(list_of_datasets, paste(setwd, "\\", protocol, "_AEs_", presDate, ".xlsx", sep=""));
  
  
  #----------#;
  
  table1_fn <- paste(protocol, "_AEs_",  presDate, ".xlsx", sep=""); 
  #i <- 1;
  for (i in 1:length(names(list_of_datasets))) {
    table1_df <- list_of_datasets[i];
    cn <- colnames(table1_df[[1]]);
    table1_df <- as.data.frame(table1_df);
    colnames(table1_df) <- cn;
    table1_sn <- names(list_of_datasets[i]); 
    table1_sn <- substr(table1_sn, 1, 31);
    
    wb <- createWorkbook();
    addWorksheet(wb, sheetName = table1_sn, gridLines = FALSE);
    writeData(wb, sheet = table1_sn, table1_df, colNames = TRUE, rowNames = FALSE, startCol = 1, startRow = 1);
    setRowHeights(wb, sheet = table1_sn, rows = 1, heights = 50); 
    setColWidths(wb, sheet = table1_sn, cols = c(1, 2, 3, 4, 5, 6, 7), widths = c(34, 15, 15, 15, 15, 15, 15));
    addStyle(wb, sheet = table1_sn, headerStyle2, rows = 1, cols = 1:7, gridExpand = TRUE);
    addStyle(wb, sheet = table1_sn, contentStyleL, rows = 2:(length(table1_df[, 1])+1), cols = 1, gridExpand = TRUE);
    addStyle(wb, sheet = table1_sn, contentStyleR, rows = 2:(length(table1_df[, 1])+1), cols = 2:7, gridExpand = TRUE);
    OutsideBorders(wb, sheet = table1_sn, rows_ = 1:(length(table1_df[, 1])+1), cols_ = 1:7);
    if (fileNameUnderscore == TRUE) {
      table1_fn <- chartr(" ", "_", table1_fn);
    }
    saveWorkbook(wb, paste(setwd, table1_fn, sep=""), overwrite = TRUE);
  }  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#;
}