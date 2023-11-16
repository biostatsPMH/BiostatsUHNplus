#' Outputs AE timeline plots including just system organ class (AE category), or 
#' system organ class and lower level term (AE detail). This function can fit up
#' to 5 different attributions. Modify width, height and scale parameters in 
#' ggsave() to customize fit for large plot.
#'
#' @param subjID key identifier field for participant ID in data sets
#' @param subjID_ineligText character text that denotes participant IDs to exclude,
#'    for example, c("New Subject") (if provided)
#' @param baseline_datasets list of data frames that contain baseline participant characteristics,
#'    for example, list(enrollment_DF,demography_DF,ineligibility_DF)
#' @param ae_dataset data frame that contains subject AEs
#' @param ae_attribVars field(s) that denotes attribution to intervention under study,
#'    for example, c("CTC_AE_ATTR_SCALE","CTC_AE_ATTR_SCALE_1") (if provided)
#' @param ae_attribVarsName character text that denotes name of interventions under study,
#'    for example, c("Drug 1", "Drug 2") (if provided)
#' @param ae_attribVarText character text that denotes related attribution, for example
#'    c("Definite", "Probable", "Possible") (if provided)
#' @param startDtVar field that denotes participant start date (i.e. 10MAY2021). For example,
#'    it could be enrollment date or screening date
#' @param ae_detailVar field that denotes participant AE detail (lower-level term)
#' @param ae_categoryVar field that denotes participant AE category (system organ class)
#' @param ae_severityVar field that denotes participant AE severity grade (numeric)
#' @param ae_onsetDtVar field that denotes participant AE onset date
#' @param time_unit character text that denotes time unit for desired timeline, 
#'   for example, could be one of c("day","week","month","year") (if provided)
#' @param include_ae_detail boolean that denotes if AE detail should be included
#'    in timeline plot. Default is True
#' @param legendPerSpace parameter at denotes proportion of vertical image space 
#'    dedicated to legend at bottom. Default is 0.05 for AE detail and 0.1 for AE Category
#' @keywords plot
#' @return ggplot object of AE timeline plot
#' @importFrom plyr join_all rbind.fill
#' @importFrom purrr modify_if
#' @importFrom dplyr select distinct mutate arrange summarise group_by filter across row_number n_distinct all_of right_join count ungroup
#' @importFrom stringr str_detect str_wrap str_split
#' @importFrom ggh4x strip_nested facet_nested elem_list_text elem_list_rect force_panelsizes
#' @importFrom forcats fct_rev
#' @importFrom cowplot get_legend
#' @importFrom ggstance geom_pointrangeh
#' @import ggplot2
#' @export
#' @examples
#' data("enrollment", "ae");
#' plotOut <- ae_timeline_plot(subjID="Subject",subjID_ineligText=c("New Subject","Test"),
#'   baseline_datasets=list(enrollment),
#'   ae_dataset=ae,
#'   ae_attribVars=c("CTC_AE_ATTR_SCALE","CTC_AE_ATTR_SCALE_1",
#'                "CTC_AE_ATTR_SCALE","CTC_AE_ATTR_SCALE_1",
#'                "CTC_AE_ATTR_SCALE"),
#'   ae_attribVarsName=c("Drug 1","Drug 2", "Drug 3","Drug 4", "Drug 5"),
#'   ae_attribVarText=c("Definite", "Probable", "Possible"),
#'   startDtVar="ENROL_DATE_INT",ae_detailVar="ae_detail",
#'   ae_categoryVar="ae_category",ae_severityVar="AE_SEV_GD",
#'   ae_onsetDtVar="AE_ONSET_DT_INT",time_unit="week")
#'   ggsave(paste("ae_detail_timeline_plot_example_2", ".png", sep=""), plotOut, width=6.4, height=18, device="png", scale = 1);

ae_timeline_plot <- function(subjID,subjID_ineligText=NULL,baseline_datasets,ae_dataset,
                      ae_attribVars,ae_attribVarsName=NULL,ae_attribVarText=NULL,
                      startDtVar,ae_detailVar,ae_categoryVar,
                      ae_severityVar,ae_onsetDtVar,time_unit=c("day","week","month","year"),
                      include_ae_detail=T,legendPerSpace=NULL,...){
  
  options(dplyr.summarise.inform = FALSE)
  if (is.null(ae_attribVarText)) {
    ae_attribVarText <- c("Definite", "Probable", "Possible");
  }
  if (time_unit == "day") {
    divisionUnit = 1;
    plotTimeText = "Days"
  } else if (time_unit == "week") {
    divisionUnit = 7;
    plotTimeText = "Weeks"
  } else if (time_unit == "month") {
    divisionUnit = 30;
    plotTimeText = "Months"
  } else if (time_unit == "year") {
    divisionUnit = 365.25;
    plotTimeText = "Years"
  } else {
    divisionUnit = 30;
    plotTimeText = "Months"
  }
  
  mydata <- plyr::join_all(baseline_datasets, by = subjID, type = "full") |>
    dplyr::right_join(ae, by = subjID) |>
    dplyr::mutate(Subject = eval(parse(text=subjID)), ae_detail = eval(parse(text=ae_detailVar)), ae_category = eval(parse(text=ae_categoryVar)), AE_SEV_GD = eval(parse(text=ae_severityVar)), AE_ONSET_DT_INT = eval(parse(text=ae_onsetDtVar)), ENROL_DATE_INT = eval(parse(text=startDtVar))) |>
    dplyr::select(Subject, ae_detail, ae_category, AE_SEV_GD, dplyr::all_of(ae_attribVars), AE_ONSET_DT_INT, ENROL_DATE_INT) |>
    dplyr::group_by(across(c(Subject, ae_detail, ae_category, dplyr::all_of(ae_attribVars), AE_SEV_GD, AE_ONSET_DT_INT))) |>
    dplyr::summarise(ENROL_DATE_INT = ENROL_DATE_INT[which(!is.na(ENROL_DATE_INT))[1]]) |>
    dplyr::mutate(AE_ONSET_DT_INT = as.Date(AE_ONSET_DT_INT, tz = "UTC"), ENROL_DATE_INT = as.Date(ENROL_DATE_INT, tz = "UTC"), AE_SEV_GD = as.numeric(AE_SEV_GD)) |>
    dplyr::filter(!Subject %in% subjID_ineligText) |>
    dplyr::arrange(Subject)
  
  
  if (include_ae_detail == T) {
    
    if (is.null(legendPerSpace)) {
      legendPerSpaceDetail = 0.05
    } else {
      legendPerSpaceDetail = legendPerSpace
    }
    
    #i <- 1;
    mydataPlot <- NA;
    mydataPlot <- as.data.frame(mydataPlot);
    for (i in 1:length(ae_attribVars)) {
      
      if (is.null(ae_attribVarsName)) {
        drugName <- paste("Attribution ", i, sep="");
      } else {
        drugName <- ae_attribVarsName[i];
      }
      
      selectedAttribVar <- ae_attribVars[i];
  
      mydata_drug112 <- mydata |> 
        dplyr::ungroup() |>
        dplyr::select(Subject, ae_detail, ae_category, AE_SEV_GD, dplyr::all_of(selectedAttribVar), AE_ONSET_DT_INT, ENROL_DATE_INT) |> 
        dplyr::filter(get(selectedAttribVar) %in% ae_attribVarText & AE_SEV_GD %in% c(1:2) & AE_ONSET_DT_INT >= ENROL_DATE_INT) |>
        dplyr::group_by(Subject, ae_detail, ae_category, AE_ONSET_DT_INT, ENROL_DATE_INT) |>
        dplyr::summarise(drug1_ae = AE_ONSET_DT_INT - ENROL_DATE_INT) |>
        dplyr::ungroup() |>
        dplyr::group_by(Subject, ae_detail, ae_category) |>
        dplyr::arrange(AE_ONSET_DT_INT) 
      #dplyr::filter(dplyr::row_number()==1) #takes the first grade 1-2 AE per subject by type
      mydata_drug1_sum12 <- mydata_drug112 |>
        dplyr::ungroup() |>
        dplyr::select(ae_detail, ae_category, drug1_ae) |>
        dplyr::group_by(ae_detail, ae_category) |>
        dplyr::summarise(time_median = median(drug1_ae), time_min = min(drug1_ae), time_max = max(drug1_ae), group = paste(drugName, ": AE 1-2", sep="")) 
      mydata_drug112 <- as.data.frame(mydata_drug112); 
      mydata_drug1_sum12 <- as.data.frame(mydata_drug1_sum12); 
      
      mydata_drug13p <- mydata |> 
        dplyr::ungroup() |>
        dplyr::select(Subject, ae_detail, ae_category, AE_SEV_GD, dplyr::all_of(selectedAttribVar), AE_ONSET_DT_INT, ENROL_DATE_INT) |> 
        dplyr::filter(get(selectedAttribVar) %in% ae_attribVarText & AE_SEV_GD %in% c(3:5) & AE_ONSET_DT_INT >= ENROL_DATE_INT) |>
        dplyr::group_by(Subject, ae_detail, ae_category, AE_ONSET_DT_INT, ENROL_DATE_INT) |>
        dplyr::summarise(drug1_ae = AE_ONSET_DT_INT - ENROL_DATE_INT) |>
        dplyr::ungroup() |>
        dplyr::group_by(Subject, ae_detail, ae_category) |>
        dplyr::arrange(AE_ONSET_DT_INT)
      #dplyr::filter(dplyr::row_number()==1) #takes the first grade 3+ AE per subject by type
      
      mydata_drug1_sum3p <- mydata_drug13p |>
        dplyr::ungroup() |>
        dplyr::select(ae_detail, ae_category, drug1_ae) |>
        dplyr::group_by(ae_detail, ae_category) |>
        dplyr::summarise(time_median = median(drug1_ae), time_min = min(drug1_ae), time_max = max(drug1_ae), group = paste(drugName, ": AE 3+", sep="")) 
      mydata_drug13p <- as.data.frame(mydata_drug13p); 
      mydata_drug1_sum3p <- as.data.frame(mydata_drug1_sum3p); 
      
      mydataPlot <- plyr::rbind.fill(mydataPlot, mydata_drug1_sum12, mydata_drug1_sum3p);
    }
    mydataPlot <- mydataPlot[-1,-1];
    ### Convert to months from days;
    mydataPlot$time_median <- round((as.numeric(mydataPlot$time_median/divisionUnit)), 1);
    mydataPlot$time_max <- round((as.numeric(mydataPlot$time_max/divisionUnit)), 1);
    mydataPlot$time_min <- round((as.numeric(mydataPlot$time_min/divisionUnit)), 1);
    
    #-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#;
    #-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#;
    ### Make AE timeline plot including both SOC and LLT;
    plotData <- mydataPlot;
    plotData <- as.data.frame(plotData);
    plotData$label <- paste(plotData$time_median, " (", plotData$time_min, "-", plotData$time_max, ")", sep="");
    plotData[which(plotData$time_min == plotData$time_max), ]$label <- paste("  ", plotData[which(plotData$time_min == plotData$time_max), ]$time_median, sep="");
    plotData$ae_category <- stringr::str_wrap(plotData$ae_category, width = 15);
    plotData$ae_detail <- stringr::str_wrap(plotData$ae_detail, width = 25);
    
    ### Make correct order for AEs to be plotted;
    plotData <- plotData |>
      dplyr::arrange(ae_detail)
    plotData <- plotData |> purrr::modify_if(is.character, as.factor);
    plotData$ae_detail <- factor(plotData$ae_detail, levels=rev(levels(plotData$ae_detail)));
    #str(plotData);
    
    ### Below part sets correct facet_nested() panel spacing adjusting for LLT, SOC label size;
    plotPanelSpacing <- plotData |>
      dplyr::select("ae_category", "ae_detail") |>
      dplyr::mutate(span_soc = lengths(stringr::str_split(ae_category, '\n'))) |>
      dplyr::mutate(span_llt = lengths(stringr::str_split(ae_detail, '\n'))) 
    span_txt <- plotData|>
      dplyr::group_by(ae_detail) |>  
      dplyr::count(ae_detail, name="span_txt")
    plotPanelSpacing <- as.data.frame(plotPanelSpacing);
    span_txt <- as.data.frame(span_txt);
    plotData <- plyr::join_all(list(plotData, span_txt, plotPanelSpacing[, -which(colnames(plotPanelSpacing) %in% c("ae_category"))]), by=c("ae_detail"), type='left', match = "first");
    #str(plotData); #92 obs. of  10 variables;
    
    #-#-#-# Control part for setting span size - this is complex, best leave alone;
    plotData$span <- plotData$span_txt;
    ### Below is how many treatment lines display okay per one line of LLT, manually tweak below;
    plotPerfectRowTxtAmt <- 2;
    plotData$span <- plotData$span_txt / plotPerfectRowTxtAmt;
    plotData$span[which(plotData$span_llt >= plotData$span_txt)] <- (plotData$span_llt[which(plotData$span_llt >= plotData$span_txt)] / plotData$span_txt[which(plotData$span_llt >= plotData$span_txt)]) * plotPerfectRowTxtAmt;
    plotData$span[which(plotData$span_txt == 1 & plotData$span_llt >= plotPerfectRowTxtAmt)] <- plotData$span_llt[which(plotData$span_txt == 1 & plotData$span_llt >= plotPerfectRowTxtAmt)];
    plotData$span[which(plotData$span_txt == 1 & plotData$span_llt < plotPerfectRowTxtAmt)] <- 1;
    span_soc <- plotData|>
      dplyr::group_by(ae_category) |>  
      dplyr::count(ae_category, name="span_soc_tot")
    span_soc <- as.data.frame(span_soc);
    plotData <- plyr::join_all(list(plotData, span_soc), by=c("ae_category"), type='left', match = "first");
    #str(plotData); #194 obs. of  12 variables;
    ### Span part below for SOC may need future adjusting and error checking;
    plotData$span[which(plotData$span_soc_tot == plotPerfectRowTxtAmt)] <- (plotData$span_soc[which(plotData$span_soc_tot == plotPerfectRowTxtAmt)] / plotData$span_soc_tot[which(plotData$span_soc_tot == plotPerfectRowTxtAmt)]) * plotPerfectRowTxtAmt;
    plotData$span[which(plotData$span_soc_tot > plotData$span_soc & plotData$span_soc*plotPerfectRowTxtAmt > plotData$span_soc_tot)] <- plotData$span_soc[which(plotData$span_soc_tot > plotData$span_soc & plotData$span_soc*plotPerfectRowTxtAmt > plotData$span_soc_tot)] / plotPerfectRowTxtAmt;
    plotData$span[which(plotData$span_soc_tot < plotPerfectRowTxtAmt)] <- plotData$span_soc[which(plotData$span_soc_tot < plotPerfectRowTxtAmt)];
    plotData$span[which(plotData$span_soc_tot < plotData$span_soc & plotData$span_soc_tot > plotPerfectRowTxtAmt)] <- plotData$span_soc[which(plotData$span_soc_tot < plotData$span_soc & plotData$span_soc_tot > plotPerfectRowTxtAmt)] / plotData$span_soc_tot[which(plotData$span_soc_tot < plotData$span_soc & plotData$span_soc_tot > plotPerfectRowTxtAmt)];
    #-#-#-#;
    
    plotPanelSpacingCheck <- plotData |> 
      dplyr::select("ae_category", "ae_detail", "span_soc", "span_llt", "span_txt", "span_soc_tot", "span") 
    #write.xlsx(plotPanelSpacingCheck, file=paste("plotPanelSpacingCheck", ".xlsx", sep=""), sheetName="GGplot facet span", col.names=TRUE, row.names=FALSE, append=F, showNA=FALSE);
    
    ### This part sets order correct for ggplot facets;  
    plotSpan <- plotData |> 
      dplyr::select("ae_category", "ae_detail", "span") |> 
      dplyr::group_by(ae_category, ae_detail) |>  
      dplyr::arrange(ae_category, desc(ae_detail)) |>
      dplyr::filter(dplyr::row_number()==1) 
    plotSpan <- as.data.frame(plotSpan);
    
    SOC_LLT_strips <- ggh4x::strip_nested(
      # Vertical strips
      size = "variable",
      background_y = ggh4x::elem_list_rect(fill = c("#FFB347", "#C19A6B")),
      text_y = ggh4x::elem_list_text(colour = c("black", "white"),
                              family=c("Bahnschrift", "Bauhaus 93"),
                              hjust = c(1,0), 
                              vjust = c(1,0.5)),
      by_layer_y = TRUE
    )
    
    #plotData;
    plotData$group <- as.factor(plotData$group);
    
    p1_with_legend <- ggplot(plotData, aes(xmin=time_min, xmax=time_max, y=forcats::fct_rev(group))) + 
      ggstance::geom_pointrangeh(aes(x=time_median, shape=group, color=group), position=position_dodge2(width = 0, preserve = "single", padding = -1.5), fatten=2) +
      geom_text(aes(x=time_max, color=group, label=label), position=position_dodge2(width = 0, preserve = "single", padding = -1.5), size=2.6, hjust=-0.2, show.legend = FALSE) +
      scale_color_manual(name=NULL, values=c("#FF2800", "#AE0C00", "#08E8DE", "#1DACD6", "#BF94E4", "#702963", "#FFBF00", "#FF7E00", "#8DB600", "#008000" )) +
      scale_shape_manual(name=NULL, values=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
      xlab(paste(plotTimeText, " [median onset time (range)]", sep="")) +
      theme(legend.position="bottom") + 
      theme(legend.title = element_blank()) +
      guides(color=guide_legend(nrow=2,byrow=F)) +
      theme(strip.text.y.left = element_text(angle = 0), strip.text = element_text(family="Bahnschrift", hjust = 1, vjust = 1, margin = margin(5, 5, 5, 5, "pt")), strip.background = element_rect(fill="#FFB347", color="white")) +
      theme(panel.spacing=unit(0, "cm")) +
      theme(axis.text.y = element_text(hjust = 1)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)), limits=c(0,max(plotData$time_max)*1.30), minor_breaks=NULL) +
      theme(panel.grid.minor.x=element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank()) +
      theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), panel.border=element_blank(), panel.background=element_blank(), plot.title=element_text(hjust = 0.5)) +
      ggh4x::facet_nested(ae_category + forcats::fct_rev(ae_detail) ~ ., scales = "free", space = "free", switch = "y", strip = SOC_LLT_strips) +
      scale_y_discrete(position = "right") +
      theme(legend.key = element_rect(fill = "white"), text=element_text(family="Arial"), plot.margin = margin(0,0,0,0, "cm")) +
      theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.x = element_text(family="Berlin Sans FB")) +
      ggh4x::force_panelsizes(rows = plotSpan$span) +
      theme(legend.title=element_blank(), legend.margin = margin(0, 0, 0, 0), legend.spacing.x = unit(0, "mm"), legend.spacing.y = unit(0, "mm"))
    p1_no_legend <- p1_with_legend + theme(legend.position = "none")
    
    #### This part makes the legend centered below both plot and panel area;
    gt <- ggplot_gtable(ggplot_build(p1_no_legend))
    le1 <- cowplot::get_legend(p1_with_legend)
    
    #---!!!!!! note the second parameter in rel_heights below sets legend space area on plot !!!!!!---# 
    pPlot <- cowplot::plot_grid(gt, le1, nrow = 2, rel_heights = c(1, legendPerSpaceDetail)) +
      theme(plot.background = element_rect(fill = "white", colour = NA))
    
    return(pPlot)
  } else {
    
    if (is.null(legendPerSpace)) {
      legendPerSpaceCategory = 0.1
    } else {
      legendPerSpaceCategory = legendPerSpace
    }
    
    #i <- 1;
    mydataPlot <- NA;
    mydataPlot <- as.data.frame(mydataPlot);
    for (i in 1:length(ae_attribVars)) {
      
      if (is.null(ae_attribVarsName)) {
        drugName <- paste("Attribution ", i, sep="");
      } else {
        drugName <- ae_attribVarsName[i];
      }
      
      selectedAttribVar <- ae_attribVars[i];
    
      mydata_drug112 <- mydata |> 
        dplyr::ungroup() |>
        dplyr::select(Subject, ae_category, AE_SEV_GD, dplyr::all_of(selectedAttribVar), AE_ONSET_DT_INT, ENROL_DATE_INT) |> 
        dplyr::filter(get(selectedAttribVar) %in% ae_attribVarText & AE_SEV_GD %in% c(1:2) & AE_ONSET_DT_INT >= ENROL_DATE_INT) |>
        dplyr::group_by(Subject, ae_category, AE_ONSET_DT_INT, ENROL_DATE_INT) |>
        dplyr::summarise(drug1_ae = AE_ONSET_DT_INT - ENROL_DATE_INT) |>
        dplyr::ungroup() |>
        dplyr::group_by(Subject, ae_category) |>
        dplyr::arrange(AE_ONSET_DT_INT) 
      #dplyr::filter(dplyr::row_number()==1) #takes the first grade 1-2 AE per subject by type
      mydata_drug1_sum12 <- mydata_drug112 |>
        dplyr::ungroup() |>
        dplyr::select(ae_category, drug1_ae) |>
        dplyr::group_by(ae_category) |>
        dplyr::summarise(time_median = median(drug1_ae), time_min = min(drug1_ae), time_max = max(drug1_ae), group = paste(drugName, ": AE 1-2", sep="")) 
      mydata_drug112 <- as.data.frame(mydata_drug112); 
      mydata_drug1_sum12 <- as.data.frame(mydata_drug1_sum12); 
      
      mydata_drug13p <- mydata |> 
        dplyr::ungroup() |>
        dplyr::select(Subject, ae_category, AE_SEV_GD, dplyr::all_of(selectedAttribVar), AE_ONSET_DT_INT, ENROL_DATE_INT) |> 
        dplyr::filter(get(selectedAttribVar) %in% ae_attribVarText & AE_SEV_GD %in% c(3:5) & AE_ONSET_DT_INT >= ENROL_DATE_INT) |>
        dplyr::group_by(Subject, ae_category, AE_ONSET_DT_INT, ENROL_DATE_INT) |>
        dplyr::summarise(drug1_ae = AE_ONSET_DT_INT - ENROL_DATE_INT) |>
        dplyr::ungroup() |>
        dplyr::group_by(Subject, ae_category) |>
        dplyr::arrange(AE_ONSET_DT_INT) 
      #dplyr::filter(dplyr::row_number()==1) #takes the first grade 3+ AE per subject by type
      mydata_drug1_sum3p <- mydata_drug13p |>
        dplyr::ungroup() |>
        dplyr::select(ae_category, drug1_ae) |>
        dplyr::group_by(ae_category) |>
        dplyr::summarise(time_median = median(drug1_ae), time_min = min(drug1_ae), time_max = max(drug1_ae), group = paste(drugName, ": AE 3+", sep="")) 
      mydata_drug13p <- as.data.frame(mydata_drug13p); 
      mydata_drug1_sum3p <- as.data.frame(mydata_drug1_sum3p);
      mydataPlot <- plyr::rbind.fill(mydataPlot, mydata_drug1_sum12, mydata_drug1_sum3p);
    }
    mydataPlot <- mydataPlot[-1,-1];
    ### Convert to months from days;
    mydataPlot$time_median <- round((as.numeric(mydataPlot$time_median/divisionUnit)), 1);
    mydataPlot$time_max <- round((as.numeric(mydataPlot$time_max/divisionUnit)), 1);
    mydataPlot$time_min <- round((as.numeric(mydataPlot$time_min/divisionUnit)), 1);
    
    #-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#;
    #-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#-~-#;
    ### Make AE timeline plot for SOC;
    plotData <- mydataPlot;
    plotData <- as.data.frame(plotData);
    plotData$label <- paste(plotData$time_median, " (", plotData$time_min, "-", plotData$time_max, ")", sep="");
    plotData[which(plotData$time_min == plotData$time_max), ]$label <- paste("  ", plotData[which(plotData$time_min == plotData$time_max), ]$time_median, sep="");
    plotData$ae_category <- stringr::str_wrap(plotData$ae_category, width = 25);
    
    ### Make correct order for AEs to be plotted;
    plotData <- plotData |>
      dplyr::arrange(ae_category)
    plotData <- plotData |> purrr::modify_if(is.character, as.factor);
    plotData$ae_category <- factor(plotData$ae_category, levels=rev(levels(plotData$ae_category)));
    
    ### Below part sets correct facet_nested() panel spacing adjusting for SOC label size;
    plotPanelSpacing <- plotData |>
      dplyr::select("ae_category") |>
      dplyr::mutate(span_soc = lengths(stringr::str_split(ae_category, '\n'))) 
    span_txt <- plotData |>
      dplyr::group_by(ae_category) |>  
      dplyr::count(ae_category, name="span_txt")
    plotPanelSpacing <- as.data.frame(plotPanelSpacing);
    span_txt <- as.data.frame(span_txt);
    plotData <- plyr::join_all(list(plotData, span_txt, plotPanelSpacing), by=c("ae_category"), type='left', match = "first");
    #str(plotData); #49 obs. of  8 variables;
    
    #-#-#-# Control part for setting span size;
    plotData$span <- plotData$span_txt;
    ### Below is how many treatment lines display okay per one line of SOC, manually tweak below;
    plotPerfectRowTxtAmt <- 2;
    plotData$span <- plotData$span_txt / plotPerfectRowTxtAmt;
    plotData$span[which(plotData$span_soc >= plotData$span_txt)] <- (plotData$span_soc[which(plotData$span_soc >= plotData$span_txt)] / plotData$span_txt[which(plotData$span_soc >= plotData$span_txt)]) * plotPerfectRowTxtAmt;
    plotData$span[which(plotData$span_txt == 1 & plotData$span_soc >= plotPerfectRowTxtAmt)] <- plotData$span_soc[which(plotData$span_txt == 1 & plotData$span_soc >= plotPerfectRowTxtAmt)];
    plotData$span[which(plotData$span_txt == 1 & plotData$span_soc < plotPerfectRowTxtAmt)] <- 1;
    #-#-#-#;
    
    plotPanelSpacingCheck <- plotData |> 
      dplyr::select("ae_category", "span_soc", "span_txt", "span") 
    #write.xlsx(plotPanelSpacingCheck, file=paste("plotPanelSpacingCheck", ".xlsx", sep=""), sheetName="GGplot facet span", col.names=TRUE, row.names=FALSE, append=F, showNA=FALSE);
    
    ### This part sets order correct for ggplot facets;  
    plotSpan <- plotData |> 
      dplyr::select("ae_category", "span") |> 
      dplyr::group_by(ae_category) |>  
      dplyr::arrange(desc(ae_category)) |>
      dplyr::filter(dplyr::row_number()==1) 
    plotSpan <- as.data.frame(plotSpan);
    
    SOC_LLT_strips <- ggh4x::strip_nested(
      # Vertical strips
      size = "variable",
      background_y = ggh4x::elem_list_rect(fill = c("#FFB347", "#C19A6B")),
      text_y = ggh4x::elem_list_text(colour = c("black", "white"),
                                     family=c("Bahnschrift", "Bauhaus 93"),
                                     hjust = c(1,0), 
                                     vjust = c(1,0.5)),
      by_layer_y = TRUE
    )
    
    #plotData;
    plotData$group <- as.factor(plotData$group);
    
    p1_with_legend <- ggplot(plotData, aes(xmin=time_min, xmax=time_max, y=forcats::fct_rev(group))) + 
      ggstance::geom_pointrangeh(aes(x=time_median, shape=group, color=group), position=position_dodge2(width = 0, preserve = "single", padding = -1.5), fatten=2) +
      geom_text(aes(x=time_max, color=group, label=label), position=position_dodge2(width = 0, preserve = "single", padding = -1.5), size=2.6, hjust=-0.2, show.legend = FALSE) +
      scale_color_manual(name=NULL, values=c("#FF2800", "#AE0C00", "#08E8DE", "#1DACD6", "#BF94E4", "#702963", "#FFBF00", "#FF7E00", "#8DB600", "#008000" )) +
      scale_shape_manual(name=NULL, values=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
      xlab(paste(plotTimeText, " [median onset time (range)]", sep="")) +
      theme(legend.position="bottom") + 
      theme(legend.title = element_blank()) +
      guides(color=guide_legend(nrow=2,byrow=F)) +
      theme(strip.text.y.left = element_text(angle = 0), strip.text = element_text(family="Bahnschrift", hjust = 1, margin = margin(5, 5, 5, 5, "pt")), strip.background = element_rect(fill="#FFB347", color="white")) +
      theme(panel.spacing=unit(0, "cm")) +
      theme(axis.text.y = element_text(hjust = 1)) +
      scale_x_continuous(expand = expansion(add = c(0,30)), limits=c(0,max(plotData$time_max)*1.30), minor_breaks=NULL) +
      theme(panel.grid.minor.x=element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank()) +
      theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), panel.border=element_blank(), panel.background=element_blank(), plot.title=element_text(hjust = 0.5)) +
      ggh4x::facet_nested(forcats::fct_rev(ae_category) ~ ., scales = "free", space = "free", switch = "y", strip = SOC_LLT_strips) +
      scale_y_discrete(position = "right") +
      theme(legend.key = element_rect(fill = "white"), text=element_text(family="Arial"), plot.margin = margin(0,0,0,0, "cm")) +
      theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.x = element_text(family="Berlin Sans FB")) +
      ggh4x::force_panelsizes(rows = plotSpan$span) +
      theme(legend.title=element_blank(), legend.margin = margin(0, 0, 0, 0), legend.spacing.x = unit(0, "mm"), legend.spacing.y = unit(0, "mm"))
    p1_no_legend <- p1_with_legend + theme(legend.position = "none")
    
    #### This part makes the legend centered below both plot and panel area;
    gt <- ggplot_gtable(ggplot_build(p1_no_legend))
    le1 <- cowplot::get_legend(p1_with_legend)
    pPlot <- cowplot::plot_grid(gt, le1, nrow = 2, rel_heights = c(1, legendPerSpaceCategory)) +
      theme(plot.background = element_rect(fill = "white", colour = NA))
    
    return(pPlot)
  }
}