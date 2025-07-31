#' Combines exported REDCap raw and label .csv files together with data dictionary. 
#'  Tranforms the exported data into Excel sheets by survey instrument with one 
#'  row per participant
#' 
#' @param protocol study protocol name (i.e. Example_Study)
#' @param pullDate date of data pull. For example, 2024_01_02 (if provided) 
#' @param subjID key identifier field(s) for participant ID in data sets
#' @param subjID_ineligText character text that denotes participant IDs to exclude
#'    using first key identifier field. For example, c("New Subject") (if provided)
#' @param subjID_eligPattern character text that denotes pattern for participant 
#'    IDs to include using first key identifier field. For example, c("^Site_A") 
#'    (if provided)
#' @param varFilter field to use for filtering data (if provided)
#' @param varFilter_eligPattern character text that denotes pattern for filter 
#'    variable to include. For example, c("^Arm_A") (if provided)
#' @param setWD_files directory where the both raw and label REDCap export .csv
#'    files are stored, following the convention for file names of 
#'    1_DATA.csv, 1_DATA_LABELs.csv, 2_DATA.csv, 2_DATA_LABELs.csv, etc
#' @param setWD_dataDict directory where the REDCap .csv data dictionary is stored. 
#'    Make sure that file is saved as CSV UTF-8 comma delimited.
#'    Must contain "ictionary" in file name (if provided)
#' @param outDir output directory where the Excel files are saved 
#' @keywords dataframe
#' @returns two Excel files, one containing variable names and labels and the 
#'    other containing REDCap survey instrument data by sheet
#' @importFrom plyr rbind.fill
#' @importFrom dplyr contains select select_if filter if_any if_all any_of everything left_join distinct_all ungroup
#' @importFrom stringr str_trunc str_replace str_detect
#' @importFrom stringi stri_trans_general
#' @importFrom openxlsx write.xlsx 
#' @importFrom rlang syms 
#' @importFrom utils read.csv 
#' @export
#' @examples
#' \dontrun{
#' redcap_data_out(protocol="Example_Study",pullDate="2024_01_03",
#     subjID=c("registry_id_no"),
#     subjID_ineligText=c("test"),
#     subjID_eligPattern=c("^Site_A"),
#     varFilter=c("redcap_event_name"),
#     varFilter_eligPattern=c("Arm A"),
#     setWD_files="./man/tables/",
#     setWD_dataDict="./man/tables/",
#     outDir="./man/tables/")
#' }
#' 
redcap_data_out <- function(protocol,pullDate=NULL,
                            subjID,subjID_ineligText=NULL,subjID_eligPattern=NULL,
                            varFilter=NULL,varFilter_eligPattern=NULL,
                            setWD_files,setWD_dataDict=NULL,outDir) {
  
  if (is.null(pullDate)) {
    pullDate <- chartr("-", "_", Sys.Date());
  }
  if (is.null(subjID_eligPattern)) {
    subjID_eligPattern <- "^";
  }
  if (is.null(varFilter)) {
    varFilter <- subjID[1];
  }
  if (is.null(varFilter_eligPattern)) {
    varFilter_eligPattern <- "^";
  }
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  first_subjID <- subjID[1];

  fileList1 <- list.files(path=setWD_files, pattern="LABEL", all.files=FALSE, full.names=FALSE, 
                          recursive=FALSE, ignore.case=FALSE, include.dirs=FALSE, no..=FALSE);
  fileList2 <- grep(list.files(path=setWD_files), pattern='LABEL', invert=TRUE, value=TRUE);
  fileList2 <- grep(fileList2, pattern='.csv', invert=FALSE, value=TRUE);
  
  data <- NA;
  data <- as.data.frame(data);
  dataLN <- NA;
  dataLN <- as.data.frame(dataLN);
  dataVN <- NA;
  dataVN <- as.data.frame(dataVN);
  #j <- 1;
  for(j in 1:length(fileList1)){
    tmpC <- read.csv(paste(setWD_files, "\\", fileList2[j], sep=""), header = T);
    if (!is.null(tmpC$redcap_repeat_instrument)) {
      tmp1 <- read.csv(paste(setWD_files, "\\", fileList1[j], sep=""), header = F);
    }
    if (is.null(tmpC$redcap_repeat_instrument)) {
      tmp1 <- read.csv(paste(setWD_files, "\\", fileList1[j], sep=""), header = T);
    }
    tmp1 <- as.data.frame(tmp1);
    tmp2 <- read.csv(paste(setWD_files, "\\", fileList2[j], sep=""), header = T);
    tmp2 <- as.data.frame(tmp2);
    tmp3 <- read.csv(paste(setWD_files, "\\", fileList1[j], sep=""), header = F, nrows = 1);
    tmp3 <- as.data.frame(tmp3);
    tmp4 <- read.csv(paste(setWD_files, "\\", fileList2[j], sep=""), header = F, nrows = 1);
    tmp4 <- as.data.frame(tmp4);
    if (is.null(tmp2$redcap_repeat_instrument)) {
      tmp1$redcap_repeat_instrument <- "Extra Sheet";
    }
    if (is.null(tmp2$redcap_repeat_instrument)) {
      tmp2$redcap_repeat_instrument <- "Extra Sheet";
    }
    if (is.null(tmp3$redcap_repeat_instrument)) {
      tmp3$redcap_repeat_instrument <- "Extra Sheet";
      tmp3$redcap_repeat_instrument[1] <- "Repeat Instrument";
    }
    if (is.null(tmp4$redcap_repeat_instrument)) {
      tmp4$redcap_repeat_instrument <- "redcap_repeat_instrument";
    }
    colnames(tmp1) <- colnames(tmp2);
    data <- plyr::rbind.fill(data, tmp1);
    dataLN <- c(dataLN, tmp3);
    dataVN <- c(dataVN, tmp4);
  };
  
  
  data_column_labels <- as.vector(unlist(dataLN[c(-1)])); 
  data_column_variables <- as.vector(unlist(dataVN[c(-1)])); 
  key <- cbind(data_column_variables, data_column_labels);
  key <- as.data.frame(key);
  key[nrow(key) + 1,] <- c("redcap_repeat_instrument", "REDCap Repeat Instrument");
  key[nrow(key) + 1,] <- c("redcap_repeat_instance", "REDCap Repeat Instance");
  colnames(key) <- c("variable_name", "variable_label"); 
  openxlsx::write.xlsx(key, paste(outDir, "\\", "key_", protocol, "_variable_names_labels_", 
                                  pullDate, ".xlsx", sep=""), sheetName="key");
  

  ###If a REDCap repeat instrument exists, then do below;
  if (!is.null(data$redcap_repeat_instrument) && 
      !all(is.na(data[which(data$redcap_repeat_instrument %!in% c("Extra Sheet")),]$redcap_repeat_instrument))) {
    data$redcap_repeat_instrument <- stringr::str_trunc(as.character(data$redcap_repeat_instrument), 28, 
                                    ellipsis=""); #sheet name has to be 28 characters or less (append rn_ for 31 max);
    data$redcap_repeat_instrument <- gsub(" ", "_", gsub("[[:punct:]]", "", 
                                    tolower(data$redcap_repeat_instrument)));
    data$redcap_repeat_instrument <- stringi::stri_trans_general(data$redcap_repeat_instrument, 
                                      "latin-ascii");
    data$redcap_repeat_instrument <- stringr::str_replace(data$redcap_repeat_instrument, "__", "_"); 
    
    if (length(which(data$redcap_repeat_instrument %in% c(""))) > 0) {
      data[which(data$redcap_repeat_instrument %in% c("")), ]$redcap_repeat_instrument <- "non_repeat_instrument";
    }
    tables <- unique(data$redcap_repeat_instrument);
    tables <- tables[which(!is.na(tables))];
    data$redcap_repeat_instrument <- as.factor(data$redcap_repeat_instrument);
    
    joinNames <- NULL;
    #i <- 10;
    for (i in 1:length(tables) ) {
      tmpTN <- paste(tables[i], sep=""); 
      tmp <- data[which(data$redcap_repeat_instrument %in% c(tables[i])), ];
      tmp <- as.data.frame(tmp);
      tmp <- tmp[, colSums(is.na(tmp)) != nrow(tmp)]; #remove columns that are all NA;
      if (length(tmp$redcap_repeat_instrument) > 0) {
        if (!tmp$redcap_repeat_instrument[1] %in% c("Extra Sheet")) {
          tmp <- tmp |> dplyr::select_if(function(x) !(all(x=="")))
          tmp <- as.data.frame(tmp)
        }
      }
      #Sometimes repeat instrument name is not set for all event instances. Have to do it this way;
      #Watch for blank and column header rows;
      tryCatch({
        tmp <- data[, which(colnames(data) %in% colnames(tmp))];
        #tmp <- data[-c(1:2), which(colnames(data) %in% colnames(tmp))];
        '%!in%' <- function(x,y)!('%in%'(x,y)) ;
        tmp$redcap_repeat_instrument <- tables[i];
        colKeep <- c(subjID, "redcap_event_name", "redcap_repeat_instrument", 
                    "redcap_repeat_instance", "redcap_data_access_group");
        tmp <- tmp |> dplyr::filter(dplyr::if_any(dplyr::any_of(subjID), ~ !is.na(.))) #remove rows missing subjID;
        #tmp <- tmp |> dplyr::filter(!is.na(!!!(rlang::syms(subjID)))) #remove rows missing subjID;
        #tmp <- tmp |> dplyr::filter(!(!!!(rlang::syms(subjID))) == "") #remove rows where subjID is "";
        tmp <- tmp |> dplyr::filter(dplyr::if_any(dplyr::any_of(subjID), ~ . != "")) #remove rows missing subjID;
        tmp <- as.data.frame(tmp)
        tmp <- tmp[, !apply(tmp, 2, function(x) all(is.na(x)))] #remove NA columns; 
        tmp <- tmp[rowSums(tmp[, which(colnames(tmp) %!in% c(colKeep))] == "") 
                   != ncol(tmp[, which(colnames(tmp) %!in% c(colKeep))]), ]; #remove rows that are all blank;
        tmp <- tmp[rowSums(is.na(tmp)) != ncol(tmp), ]; #remove rows that are all NA;
        #tmp <- tmp |> dplyr::filter(dplyr::if_any(dplyr::any_of(subjID), ~ . %!in% c("Study ID", "Record ID", "Patient ID")))
        tmp <- tmp[tmp[,1] %!in% c("Study ID", "Record ID", "Patient ID"), ]; #clean header rows;
        
        if (length(tmp$redcap_repeat_instrument) > 0) {
          if (!tmp$redcap_repeat_instrument[1] %in% c("Extra Sheet")) {
            tmp <- tmp |> dplyr::select_if(function(x) !(all(x=="")))
            tmp <- as.data.frame(tmp)
          }
        }
      }, error=function(e){})  
      tmp <- tmp |> dplyr::select(-contains(".factor")); 
      tmp <- as.data.frame(tmp);
      assign(tmpTN, tmp);
      joinNames[i] <- tmpTN; 
    }
  }

  
  ###If a REDCap repeat instrument does not exist, then do below;
  if (is.null(data$redcap_repeat_instrument)) {
    data$redcap_repeat_instrument <- "non_repeat_instrument";
    tables <- unique(data$redcap_repeat_instrument);
    
    joinNames <- NULL;
    #i <- 2;
    for (i in 1:length(tables) ) {
      tmpTN <- paste(tables[i], sep=""); 
      tmp <- data[which(data$redcap_repeat_instrument %in% c(tables[i])), ]; 
      tmp[tmp == ""] <- NA;
      tmp <- tmp[, colSums(is.na(tmp)) != nrow(tmp)]; 
      tmp <- tmp[which(!is.na(tmp[, 1])), ]; 
      tmp$redcap_repeat_instrument <- tmpTN;
      tmp$redcap_repeat_instance <- 1;
      tmp <- as.data.frame(tmp);
      tmp <- tmp[, c(1, length(tmp)-1, length(tmp), 2:(length(tmp)-2))]; 
      assign(tmpTN, tmp);
      joinNames[i] <- tmpTN; 
    }
  }

  
  ###Get REDCap instrument from data dictionary, if provided for non-repeat instruments;
  tryCatch({
    if (length(data[which(data$redcap_repeat_instrument %in% 
                          c("non_repeat_instrument", "Extra Sheet")),]) > 0) {
      fileList3 <- file.info(list.files(pattern = c("ictionary"), path = setWD_dataDict, 
                                        full.names = TRUE, include.dirs = FALSE));
      newestFiles <- rownames(fileList3);
      fileList3 <- grep(newestFiles, pattern='.csv', invert=FALSE, value=TRUE);
      data_dictionary <- NA;
      data_dictionary <- as.data.frame(data_dictionary);
      #j <- 1;
      for(j in 1:length(fileList3)){
        data_dictionaryTMP <- read.csv(newestFiles[j], header=TRUE);
        data_dictionary <- plyr::rbind.fill(data_dictionary, data_dictionaryTMP);
      }
      data_dictionary <- data_dictionary[-1,-1]; #remove first empty row and column;
      data_dictionary <- data_dictionary[!duplicated(data_dictionary[,1]), ]; #remove duplicate field names (take first);
      # fileList3 <- file.info(list.files(pattern = c("ictionary"), path = setWD_dataDict,
      #                                   full.names = TRUE));
      # newestFile <- rownames(fileList3)[which.max(fileList3$mtime)];
      # data_dictionary <- read.csv(newestFile, header=TRUE);
      data_dictionary[,2] <- stringr::str_trunc(as.character(data_dictionary[,2]), 28, ellipsis=""); 
      #sheet name has to be 28 characters or less (append rn_ for 31 max);
      tables <- unique(data$redcap_repeat_instrument);
      
      if (!exists("joinNames")) {
        joinNames <- NULL
      }
      joinNamesNRI <- NULL;
      #i <- 9;
      for (i in 1:length(unique(data_dictionary[,2]))) {
        if (!unique(data_dictionary[,2])[i] %in% tables) {
          tmpTN <- paste(unique(data_dictionary[,2])[i], sep="");
          varKeep <- data_dictionary[which(data_dictionary[,2] %in% c(unique(data_dictionary[,2])[i])), 1];
          varKeep <- c(varKeep, data_dictionary[1,1], subjID, "redcap_event_name", "redcap_repeat_instrument", 
                       "redcap_repeat_instance", "redcap_data_access_group");
          dataNRI <- data[which(data$redcap_repeat_instrument %in% c("non_repeat_instrument", "extra_sheet", "Extra Sheet")), ];
          tryCatch({
            checkboxVars <- colnames(dataNRI[, grep("___", names(dataNRI))]); #these are REDCap checkbox variables;
            checkboxVar_dd <- sub("___.*", "", checkboxVars);
            cb_dd <- checkboxVar_dd %in% varKeep;
            cbKeep <- checkboxVars[cb_dd];
            varKeep <- c(varKeep, cbKeep);
          }, error=function(e){})
          dataNRI$redcap_repeat_instrument <- NA;
          tmp <- dataNRI[, which(colnames(dataNRI) %in% c(varKeep))];
          
          tryCatch({
            non_repeat_instrument <- non_repeat_instrument[, which(colnames(non_repeat_instrument) %in% c(varKeep))];
          }, error=function(e){})
          tryCatch({
            extra_sheet <- extra_sheet[, which(colnames(extra_sheet) %in% c(varKeep))];
            #ddVars <- data_dictionary[which(data_dictionary[,2] %in% c(unique(data_dictionary[,2]))), 1];
            #ddVars <- c(ddVars, "data");
            #ddVars <- ddVars[ddVars %!in% c(data_dictionary[1,1], subjID, "redcap_event_name", "redcap_repeat_instrument", 
            #                         "redcap_repeat_instance", "redcap_data_access_group")];
            #extra_sheet <- dataNRI[, which(colnames(dataNRI) %!in% c(ddVars))];
          }, error=function(e){})
          tryCatch({
            tmp[tmp == ""] <- NA;
            tmp <- tmp[rowSums(is.na(tmp[, which(!colnames(tmp) %in% c(subjID,"redcap_event_name",
                      "redcap_repeat_instance", "redcap_data_access_group"))])) != ncol(tmp[, which(!colnames(tmp) %in% 
                      c(subjID,"redcap_event_name","redcap_repeat_instance", "redcap_data_access_group"))]), ]; 
            tmp$redcap_repeat_instrument <- tmpTN;
            tryCatch({
              tmp$redcap_repeat_instance[is.na(tmp$redcap_repeat_instance)] <- 1;
            }, error=function(e){})
            if (is.null(tmp$redcap_repeat_instance)) {
              tmp$redcap_repeat_instance <- 1
            }
            tmp <- as.data.frame(tmp);
            assign(tmpTN, tmp);
            joinNamesNRI[i] <- tmpTN;
          }, error=function(e){})
        }
      }
    }
    joinNames <- c(joinNamesNRI, joinNames);
    tryCatch({
      joinNames <- joinNames[which(!joinNames %in% c(NA))];
    }, error=function(e){})
  }, error=function(e){})

  
  #--#--#--#--#--#--#--#--#--#--#--#--#--#--#--# this writes tables as Excel sheets for participants;

  
  if (exists("non_repeat_instrument") && nrow((non_repeat_instrument)) == 0) {
    joinNames <- joinNames[which(!joinNames %in% c("NA", "repeat_instrument", "non_repeat_instrument"))];
  } else {
    joinNames <- joinNames[which(!joinNames %in% c("NA", "repeat_instrument"))];
  }
  joinNames <- sort(joinNames, decreasing = FALSE); #sort table name alphabetically;
  
  #list_of_datasets <- lapply(joinNames, function(x) get(x, mode="list"), envir=sys.frame(sys.parent(0)));
  list_of_datasets <- lapply(joinNames, function(x) get(x, mode="list"));
  names(list_of_datasets) <- c(joinNames);


  #### Below only keep instrument dataset if subjID field exists in it;
  #-#-#-#-#-#
  #if(!is.null(dplyr::filter(list_of_datasets[[1]],
  #            any(subjID %in% colnames(list_of_datasets[[1]]))))){list_of_datasets[[1]]}
  ##Below also works for condition;
  #if(!is.null(dplyr::select(list_of_datasets[[1]], dplyr::any_of(subjID)))){list_of_datasets[[1]]}
  #-#-#-#-#-#
  
  #list_of_datasets <- lapply(list_of_datasets, function(df){if(subjID %in% colnames(df)){df}});
  list_of_datasets <- lapply(list_of_datasets, function(df){if(!is.null(dplyr::filter(df,
                                                        any(subjID %in% colnames(df))))){df}});
  list_of_datasets <- Filter(function(x) nrow(x) > 0, list_of_datasets); 
  list_of_datasets <- Filter(Negate(is.null), list_of_datasets);
  
  ##Find distinct and complete subjID key identifiers from longest dataset;
  #str(list_of_datasets)
  keyIdentifiers <- data |> dplyr::select(dplyr::any_of(subjID));
  #keyIdentifiers <- lapply(list_of_datasets, function(df) df |>
  #                          dplyr::select(dplyr::any_of(subjID)));
  #keyIdentifiers <- Filter(function(x) ncol(x) == length(subjID), keyIdentifiers); 
  #lengths <- lapply(keyIdentifiers, nrow);
  #longest <- which.max(lengths);
  #keyIdentifiers <- keyIdentifiers[[longest]]; 
  keyIdentifiers <- keyIdentifiers |> dplyr::distinct_all()
  keyIdentifiers <- keyIdentifiers |> 
    dplyr::filter(dplyr::if_all(everything(), ~ !is.na(.) & . != ""))
  
  
  
  ##Below filters instrument datasets by participant characteristics;
  list_of_datasets2 <- lapply(list_of_datasets, function(df) df |>
            dplyr::left_join(keyIdentifiers) |>
            dplyr::select(subjID, dplyr::everything()) |>
            dplyr::ungroup() |>
            dplyr::filter(!get(first_subjID) %in% subjID_ineligText) |>
            dplyr::filter(stringr::str_detect(get(varFilter), varFilter_eligPattern)) |>
            dplyr::filter(stringr::str_detect(get(first_subjID), subjID_eligPattern)));
  list_of_datasets2 <- Filter(function(x) nrow(x) > 0, list_of_datasets2); 
  list_of_datasets2 <- Filter(Negate(is.null), list_of_datasets2);
  names(list_of_datasets2) <- make.unique(names(list_of_datasets2), sep = '_')
  openxlsx::write.xlsx(list_of_datasets2, paste(outDir, "\\", protocol, "_participants_data_pull_", 
                                           pullDate, ".xlsx", sep=""));
}