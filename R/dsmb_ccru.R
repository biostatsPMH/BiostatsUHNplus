#' Outputs the three DSMB-CCRU AE summary tables in Excel format per UHN template
#'
#' @param protocol study protocol name (uppercase, no spaces permitted)
#' @param setwd directory to write Excel summary files to
#' @param title full character vector with name of study
#' @param comp baseline comparison group, for example, cohort (if provided)
#' @param pi character vector name of study principal investigator
#' @param presDate presentation date (i.e. 17NOV2023) for DSMB
#' @param cutDate recent cutoff date for AEs (i.e. 31AUG2023)
#' @param boundDate lower bound cutoff date for AEs (if provided)
#' @param subjID key identifier field for participant ID in data sets
#' @param subjID_ineligText character text that denotes participant IDs to exclude,
#'    for example, c("New Subject") (if provided)
#' @param baseline_datasets list of data frames that contain baseline participant characteristics,
#'    for example, list(enrollment_DF,demography_DF,ineligibility_DF)
#' @param ae_dataset data frame that contains subject AEs
#' @param ineligVar field that denotes participant ineligibility
#' @param ineligVarText character text that denotes participant ineligibility,
#'    for example, c("Yes", "Y") (if provided)
#' @param genderVar field that denotes participant gender
#' @param enrolDtVar field that denotes participant enrolment date (i.e. 10MAY2021)
#' @param ae_detailVar field that denotes participant AE detail (lowest level term)
#' @param ae_categoryVar field that denotes participant AE category (system organ class)
#' @param ae_severityVar field that denotes participant AE severity grade (numeric)
#' @param ae_onsetDtVar field that denotes participant AE onset date
#' @param ae_detailOtherText character text that denotes referencing vebatim AE field, 
#'   for example, c("Other, specify", "OTHER") (if provided)
#' @param ae_detailOtherVar field that denotes participant AE detail other (if provided)
#' @param ae_verbatimVar field that denotes participant AE detail verbatim (if provided)
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
#'   numSubj=c(2,4,5,6))

dsmb_ccru <- function(protocol,setwd,title,comp=NULL,pi,presDate,cutDate,boundDate=NULL,
                      subjID,subjID_ineligText=NULL,baseline_datasets,ae_dataset,
                      ineligVar,ineligVarText=NULL,
                      genderVar,enrolDtVar,ae_detailVar,ae_categoryVar,
                      ae_severityVar,ae_onsetDtVar,ae_detailOtherText=NULL,ae_detailOtherVar=NULL,
                      ae_verbatimVar=NULL,numSubj=NULL,fileNameUnderscore=TRUE){

  #### Template style for tables;
  ##https://stackoverflow.com/questions/54322814/how-to-apply-thick-border-around-a-cell-range-using-the-openxlsx-package-in-r ;
  OutsideBorders <-
    function(wb_,
             sheet_,
             rows_,
             cols_,
             border_col = "#AAC1D9",
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
  headerStyle2 <- createStyle(fontName = "Arial", fontSize = 9, fontColour = "black",
                              halign = "center", valign = "center", fgFill = "#FAF3D4", 
                              border = "TopBottomLeftRight", borderColour = "#AAC1D9",
                              textDecoration = "bold", wrapText = TRUE)
  contentStyleR <- createStyle(fontName = "Arial", fontSize = 8, fontColour = "black",
                               halign = "right", valign = "center", fgFill = "white", 
                               border = "TopBottomLeftRight", borderColour = "#AAC1D9",
                               wrapText = TRUE)
  contentStyleL <- createStyle(fontName = "Arial", fontSize = 8, fontColour = "black",
                               halign = "left", valign = "center", fgFill = "white", 
                               border = "TopBottomLeftRight", borderColour = "#AAC1D9",
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
      dplyr::arrange(Subject)
  }
  
  aeKeep_DF <- ae_dataset |>
    #### --------------------------------------------- ####
  #### Just modify the below line for variable names ####
  dplyr::mutate(Subject = eval(parse(text=subjID)), ae_grade_code_dyn_std = eval(parse(text=ae_severityVar)), CTCAE5_LLT_NM = eval(parse(text=ae_detailOtherVar)), AE_VERBATIM_TRM_TXT = eval(parse(text=ae_verbatimVar)), AE_ONSET_DT_INT = eval(parse(text=ae_onsetDtVar)), ae_detail = eval(parse(text=ae_detailVar)), ae_category = eval(parse(text=ae_categoryVar))) |>
    dplyr::select(Subject, ae_grade_code_dyn_std, CTCAE5_LLT_NM, AE_VERBATIM_TRM_TXT, AE_ONSET_DT_INT, ae_detail, ae_category) |>
    #### --------------------------------------------- ####
  dplyr::mutate(ae_detail = toupper(ifelse(stringr::str_detect(ae_detail, ae_detailOtherText), trimws(AE_VERBATIM_TRM_TXT), ae_detail)), AE_ONSET_DT_INT = toupper(format(as.Date(AE_ONSET_DT_INT, tz = "UTC"), "%d%b%Y")), ae_category = toupper(ae_category)) |>
    dplyr::mutate(ae_detail = toupper(ifelse(is.na(ae_detail), CTCAE5_LLT_NM, ae_detail))) 
  
  #i <- 1;
  for (i in 1:length(unique(subjectsKeep_DF[["comp"]]))) {
    comp <- unique(subjectsKeep_DF[["comp"]])[i]
    tryCatch({
      subjects_DF <- subjectsKeep_DF[which(subjectsKeep_DF$comp %in% c(comp)), ] 
    }, error=function(e){})
    
    #### Do not need to modify below here;
    
    aes1_DF <- subjects_DF |> 
      dplyr::left_join(aeKeep_DF, by = "Subject") |>
      dplyr::select(Subject, AE_ONSET_DT_INT, ae_detail, ae_category, ae_grade_code_dyn_std, PARTIC_ENROL_DT_INT, CTCAE5_LLT_NM, AE_VERBATIM_TRM_TXT) |>
      dplyr::arrange(Subject) |>
      dplyr::filter(as.Date(PARTIC_ENROL_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y"), as.Date(AE_ONSET_DT_INT, "%d%b%Y") <= as.Date(cutDate, "%d%b%Y"), !is.na(ae_detail)) |>
      dplyr::distinct(Subject, AE_ONSET_DT_INT, ae_detail, ae_category, ae_grade_code_dyn_std, PARTIC_ENROL_DT_INT)
    #write.xlsx(aes1_DF, file=paste("aes1_DF", ".xlsx", sep=""), sheetName="AEs check", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE);
    #unique(aes1_DF$ae_detail);
    
    aes2_DF <- aes1_DF |>  
      dplyr::distinct(Subject, ae_category, ae_detail, ae_grade_code_dyn_std) |>
      dplyr::group_by(Subject, ae_detail) |>
      dplyr::filter(ae_grade_code_dyn_std == max(ae_grade_code_dyn_std)) |>
      dplyr::arrange(Subject) 
    
    total_subj_count <- length(unique(subjects_DF$Subject));
    if (!is.null(numSubj)) {
      total_subj_count <- numSubj[i];
    }
    
    total_ae_count <- length(aes1_DF$ae_category);
    
    #### Table 1;
    table1_dfa <- aes2_DF |>
      dplyr::group_by(ae_category, ae_detail) |>
      dplyr::summarise(ind = n_distinct(Subject)) |>
      dplyr::mutate(ind_per = format(round((ind/total_subj_count)*100, 2), nsmall=2))
    table1_dfb <- aes2_DF |>
      dplyr::group_by(ae_category, ae_detail) |>
      dplyr::filter(ae_grade_code_dyn_std %in% c(3:5)) |>
      dplyr::summarise(indH = n_distinct(Subject)) |>
      dplyr::mutate(indH_per = format(round((indH/total_subj_count)*100, 2), nsmall=2))
    table1_df <- table1_dfa |>
      dplyr::left_join(table1_dfb, by = c("ae_category", "ae_detail")) |>
      dplyr::mutate(indH = ifelse(is.na(indH), 0, indH), indH_per = ifelse(is.na(indH_per), "0", indH_per))
    colnames(table1_df) <- c("Category", "Adverse event", "# of subjects that have experienced the AE", paste("the % of subjects that this comprises of the total accrual (N=", total_subj_count, ")", sep=""), "# of subjects that experienced the event at a grade 3 to 5", paste("% of the subjects that this comprises of the total accrual (N=", total_subj_count, ")", sep=""));
    table1_df <- as.data.frame(table1_df);
    
    table1_sn <- paste("ae_detail ", protocol, sep=""); 
    table1_sn <- substr(table1_sn, 1, 31);
    table1_fn <- paste("ae_detail ", protocol, " ", comp, " ", presDate, ".xlsx", sep=""); 
    wb <- createWorkbook();
    addWorksheet(wb, sheetName = table1_sn, gridLines = FALSE);
    writeData(wb, sheet = table1_sn, table1_df, colNames = TRUE, rowNames = FALSE, startCol = 1, startRow = 9);
    mergeCells(wb, sheet = table1_sn, cols = 1:6, rows = 1);
    mergeCells(wb, sheet = table1_sn, cols = 1:6, rows = 2);
    mergeCells(wb, sheet = table1_sn, cols = 1:6, rows = 3);
    mergeCells(wb, sheet = table1_sn, cols = 1:6, rows = 4);
    mergeCells(wb, sheet = table1_sn, cols = 1:6, rows = 5);
    mergeCells(wb, sheet = table1_sn, cols = 1:6, rows = 6);
    mergeCells(wb, sheet = table1_sn, cols = 1:6, rows = 7);
    mergeCells(wb, sheet = table1_sn, cols = 1:6, rows = 8);
    writeData(wb, sheet = table1_sn, title, colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 1);
    writeData(wb, sheet = table1_sn, comp, colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 3);
    writeData(wb, sheet = table1_sn, paste("PI: ", pi, sep=""), colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 5);
    writeData(wb, sheet = table1_sn, paste("Report date: ", presDate, sep=""), colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 7);
    addStyle(wb, sheet = table1_sn, headerStyle1, rows = c(1,3,5,7), cols = 1, gridExpand = TRUE);
    addStyle(wb, sheet = table1_sn, headerStyle2, rows = 9, cols = 1:6, gridExpand = TRUE);
    addStyle(wb, sheet = table1_sn, contentStyleL, rows = 10:(length(table1_df[, 1])+9), cols = 1:2, gridExpand = TRUE);
    addStyle(wb, sheet = table1_sn, contentStyleR, rows = 10:(length(table1_df[, 1])+9), cols = 3:6, gridExpand = TRUE);
    setRowHeights(wb, 1, rows = 1, heights = 50); 
    setRowHeights(wb, 1, rows = 9, heights = 82); 
    setColWidths(wb, 1, cols = c(1, 2, 3, 4, 5, 6), widths = c(34, 34, 15, 15, 15, 15));
    OutsideBorders(wb, sheet_ = 1, rows_ = 9:(length(table1_df[, 1])+9), cols_ = 1:6);
    if (fileNameUnderscore == TRUE) {
      table1_fn <- chartr(" ", "_", table1_fn);
    }
    saveWorkbook(wb, paste(setwd, table1_fn, sep=""), overwrite = TRUE);
    
    #### Table 2; 
    table2_dfa <- aes2_DF |>
      dplyr::group_by(ae_category) |>
      dplyr::summarise(ind = n_distinct(Subject)) |>
      dplyr::mutate(ind_per = format(round((ind/total_subj_count)*100, 2), nsmall=2))
    table2_dfb <- aes2_DF |>
      dplyr::group_by(ae_category) |>
      dplyr::filter(ae_grade_code_dyn_std %in% c(3:5)) |>
      dplyr::summarise(indH = n_distinct(Subject)) |>
      dplyr::mutate(indH_per = format(round((indH/total_subj_count)*100, 2), nsmall=2))
    table2_df <- table2_dfa |>
      dplyr::left_join(table2_dfb, by = c("ae_category")) |>
      dplyr::mutate(indH = ifelse(is.na(indH), 0, indH), indH_per = ifelse(is.na(indH_per), "0", indH_per))
    colnames(table2_df) <- c("Category", "# of subjects that have experienced the AE", paste("the % of subjects that this comprises of the total accrual (N=", total_subj_count, ")", sep=""), "# of subjects that experienced the event at a grade 3 to 5", paste("% of the subjects that this comprises of the total accrual (N=", total_subj_count, ")", sep=""));
    table2_df <- as.data.frame(table2_df);
    
    table2_sn <- paste("category BySubject ", protocol, sep=""); 
    table2_sn <- substr(table2_sn, 1, 31);
    table2_fn <- paste("category BySubject ", protocol, " ", comp, " ", presDate, ".xlsx", sep=""); 
    wb <- createWorkbook();
    addWorksheet(wb, sheetName = table2_sn, gridLines = FALSE);
    writeData(wb, sheet = table2_sn, table2_df, colNames = TRUE, rowNames = FALSE, startCol = 1, startRow = 9);
    mergeCells(wb, sheet = table2_sn, cols = 1:5, rows = 1);
    mergeCells(wb, sheet = table2_sn, cols = 1:5, rows = 2);
    mergeCells(wb, sheet = table2_sn, cols = 1:5, rows = 3);
    mergeCells(wb, sheet = table2_sn, cols = 1:5, rows = 4);
    mergeCells(wb, sheet = table2_sn, cols = 1:5, rows = 5);
    mergeCells(wb, sheet = table2_sn, cols = 1:5, rows = 6);
    mergeCells(wb, sheet = table2_sn, cols = 1:5, rows = 7);
    mergeCells(wb, sheet = table2_sn, cols = 1:5, rows = 8);
    writeData(wb, sheet = table2_sn, title, colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 1);
    writeData(wb, sheet = table2_sn, comp, colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 3);
    writeData(wb, sheet = table2_sn, paste("PI: ", pi, sep=""), colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 5);
    writeData(wb, sheet = table2_sn, paste("Report date: ", presDate, sep=""), colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 7);
    addStyle(wb, sheet = table2_sn, headerStyle1, rows = c(1,3,5,7), cols = 1, gridExpand = TRUE);
    addStyle(wb, sheet = table2_sn, headerStyle2, rows = 9, cols = 1:5, gridExpand = TRUE);
    addStyle(wb, sheet = table2_sn, contentStyleL, rows = 10:(length(table2_df[, 1])+9), cols = 1, gridExpand = TRUE);
    addStyle(wb, sheet = table2_sn, contentStyleR, rows = 10:(length(table2_df[, 1])+9), cols = 2:5, gridExpand = TRUE);
    setRowHeights(wb, 1, rows = 1, heights = 50); 
    setRowHeights(wb, 1, rows = 9, heights = 82); 
    setColWidths(wb, 1, cols = c(1, 2, 3, 4, 5), widths = c(34, 15, 15, 15, 15));
    OutsideBorders(wb, sheet_ = 1, rows_ = 9:(length(table2_df[, 1])+9), cols_ = 1:5);
    if (fileNameUnderscore == TRUE) {
      table2_fn <- chartr(" ", "_", table2_fn);
    }
    saveWorkbook(wb, paste(setwd, table2_fn, sep=""), overwrite = TRUE);
    
    #### Table 3;
    table3_dfa <- aes1_DF |>
      dplyr::group_by(ae_category) |>
      dplyr::summarise(ind = n()) |>
      dplyr::mutate(ind_per = format(round((ind/total_ae_count)*100, 2), nsmall=2))
    table3_dfb <- aes1_DF |>
      dplyr::group_by(ae_category) |>
      dplyr::filter(ae_grade_code_dyn_std %in% c(3:5)) |>
      dplyr::summarise(indH = n()) |>
      dplyr::mutate(indH_per = format(round((indH/total_ae_count)*100, 2), nsmall=2))
    table3_df <- table3_dfa |>
      dplyr::left_join(table3_dfb, by = c("ae_category")) |>
      dplyr::mutate(indH = ifelse(is.na(indH), 0, indH), indH_per = ifelse(is.na(indH_per), "0", indH_per))
    colnames(table3_df) <- c("Category", "# of events that have experienced the AE in this category", paste("% of events in relation to the total events (N=", total_ae_count, ")", sep=""), "# of events that were grade 3 to 5", paste("% of events that this comprises of the total accrual (N=", total_ae_count, ")", sep=""));
    table3_df <- as.data.frame(table3_df);
    
    table3_sn <- paste("category ByEvent ", protocol, sep=""); 
    table3_sn <- substr(table3_sn, 1, 31);
    table3_fn <- paste("category ByEvent ", protocol, " ", comp, " ", presDate, ".xlsx", sep=""); 
    wb <- createWorkbook();
    addWorksheet(wb, sheetName = table3_sn, gridLines = FALSE);
    writeData(wb, sheet = table3_sn, table3_df, colNames = TRUE, rowNames = FALSE, startCol = 1, startRow = 9);
    mergeCells(wb, sheet = table3_sn, cols = 1:5, rows = 1);
    mergeCells(wb, sheet = table3_sn, cols = 1:5, rows = 2);
    mergeCells(wb, sheet = table3_sn, cols = 1:5, rows = 3);
    mergeCells(wb, sheet = table3_sn, cols = 1:5, rows = 4);
    mergeCells(wb, sheet = table3_sn, cols = 1:5, rows = 5);
    mergeCells(wb, sheet = table3_sn, cols = 1:5, rows = 6);
    mergeCells(wb, sheet = table3_sn, cols = 1:5, rows = 7);
    mergeCells(wb, sheet = table3_sn, cols = 1:5, rows = 8);
    writeData(wb, sheet = table3_sn, title, colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 1);
    writeData(wb, sheet = table3_sn, comp, colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 3);
    writeData(wb, sheet = table3_sn, paste("PI: ", pi, sep=""), colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 5);
    writeData(wb, sheet = table3_sn, paste("Report date: ", presDate, sep=""), colNames = FALSE, rowNames = FALSE, startCol = 1, startRow = 7);
    addStyle(wb, sheet = table3_sn, headerStyle1, rows = c(1,3,5,7), cols = 1, gridExpand = TRUE);
    addStyle(wb, sheet = table3_sn, headerStyle2, rows = 9, cols = 1:5, gridExpand = TRUE);
    addStyle(wb, sheet = table3_sn, contentStyleL, rows = 10:(length(table3_df[, 1])+9), cols = 1, gridExpand = TRUE);
    addStyle(wb, sheet = table3_sn, contentStyleR, rows = 10:(length(table3_df[, 1])+9), cols = 2:5, gridExpand = TRUE);
    setRowHeights(wb, 1, rows = 1, heights = 50); 
    setRowHeights(wb, 1, rows = 9, heights = 82); 
    setColWidths(wb, 1, cols = c(1, 2, 3, 4, 5), widths = c(34, 15, 15, 15, 15));
    OutsideBorders(wb, sheet_ = 1, rows_ = 9:(length(table3_df[, 1])+9), cols_ = 1:5);
    trimws("dsmb_ccru_tables/category ByEvent EXAMPLE_STUDY Cohort D 30OCT2020.xlsx");
    if (fileNameUnderscore == TRUE) {
      table3_fn <- chartr(" ", "_", table3_fn);
    }
    saveWorkbook(wb, paste(setwd, table3_fn, sep=""), overwrite = TRUE);
    
    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#;
  }      
}