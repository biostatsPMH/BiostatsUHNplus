#' Caterpillar plot. Useful for plotting random effects from hierarchical models,
#' such as MCMCglmm::MCMCglmm() object, that have binary outcome.
#'
#' @param subjID key identifier field for participant ID in data sets
#' @param remove.text.subjID boolean indiciating if non-numeric text should be 
#'    removed from subjID in plot label. Note that this can only be used if there
#'    are non-duplicate participant IDs when non-numeric text is removed. Default 
#'    is FALSE (if provided)
#' @param mcmcglmm_object MCMCglmm model output
#' @param orig_dataset data frame supplied to MCMCglmm function
#' @param ae_attribVars field(s) that denotes attribution to intervention under study,
#'    for example, c("CTC_AE_ATTR_SCALE","CTC_AE_ATTR_SCALE_1") (if provided)
#' @param binaryOutcomeVar name of binary variable (0,1) that denotes outcome 
#'    in MCMCglmm model
#' @param prob probability for highest posterior density interval, similar to a 
#'    confidence interval. Defaul is 0.95 (if provided)
#' @param title title of the plot. Overrides default title (if provided)
#' @param subtitle subtitle of the plot. Overrides default subtitle (if provided)
#' @param ncol number of columns in plot. Default is 2 (if provided)
#' @param no.title boolean that denotes if title should be outputted in plot. Default 
#'    is TRUE (if provided)
#' @param fonts character text that denotes font for title, subtitle, category labels,
#'    x-axis plot labels (if provided)
#' @param columnTextWidth numeric that denotes character width for label text before
#'    breaking to start new line. Default is 20 characters (if provided)
#' @param break.label.summary boolean to indicate if new line should start in label
#'    before (n, event) summary. Default is FALSE
#' @keywords plot
#' @return ggplot object of caterpillar plot
#' @importFrom plyr join_all rbind.fill
#' @importFrom MCMCglmm posterior.mode 
#' @importFrom coda HPDinterval
#' @importFrom purrr modify_if
#' @importFrom dplyr select rename group_by arrange filter row_number n
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @import lifecycle
#' @export
#' @examples
#' data("ae");
#' 
#' ae$G3Plus <- 0;
#' ae$G3Plus[ae$AE_SEV_GD %in% c("3", "4", "5")] <- 1;
#' ae$Drug_1_Attribution <- 0;
#' ae$Drug_1_Attribution[ae$CTC_AE_ATTR_SCALE %in% c("Definite", "Probable", "Possible")] <- 1;
#' ae$Drug_2_Attribution <- 0;
#' ae$Drug_2_Attribution[ae$CTC_AE_ATTR_SCALE_1 %in% c("Definite", "Probable", "Possible")] <- 1;
#' 
#' prior2RE <- list(R = list(V = diag(1), fix = 1),
#'   G=list(G1=list(V=1, nu=0.02), G2=list(V=1, nu=0.02)));
#'   
#' model1 <- MCMCglmm(G3Plus ~ Drug_1_Attribution + Drug_2_Attribution, 
#'   random=~Subject + ae_category, family="categorical", data=ae, saveX=TRUE, 
#'   verbose=F, burnin=2000, nitt=10000, thin=10, pr=TRUE, prior=prior2RE);
#'   
#' p <- caterpillar_plot(subjID = "Subject",
#'   mcmcglmm_object = model1,
#'   prob = 0.99,
#'   orig_dataset = ae,
#'   binaryOutcomeVar = "G3Plus")
#'   
#' p <- caterpillar_plot(subjID = "ae_category",
#'   mcmcglmm_object = model1,
#'   prob = 0.95,
#'   orig_dataset = ae,
#'   remove.text.subjID = FALSE,
#'   ncol = 4,
#'   binaryOutcomeVar = "G3Plus",
#'   subtitle = "System organ class (n, event)",
#'   title = "Odds Ratio for G3+ Severity with 95% Highest Posterior Density Interval",
#'   fonts = c("Arial", "Arial", "Arial", "Arial"),
#'   break.label.summary = TRUE)

caterpillar_plot <- function(subjID,
                             remove.text.subjID=FALSE,
                             mcmcglmm_object,orig_dataset,
                             binaryOutcomeVar,
                             prob=NULL,title=NULL,no.title=FALSE,
                             subtitle=NULL,ncol=NULL,
                             fonts=NULL,columnTextWidth=NULL,
                             break.label.summary=FALSE
                             ){
  
  
  font.title <- "Bahnschrift";
  font.subtitle <- "Gungsuh";
  font.labels <- "Berlin Sans FB";
  font.axis <- "Gadugi";
  if (!is.null(fonts)) {
    tryCatch({
      font.title <- na.fail(fonts[1]);
    }, error=function(e){})
    tryCatch({
      font.subtitle <- na.fail(fonts[2]);
    }, error=function(e){})
    tryCatch({
      font.labels <- na.fail(fonts[3]);
    }, error=function(e){})
    tryCatch({
      font.axis <- na.fail(fonts[4]);
    }, error=function(e){})
  }
  
  if (is.null(ncol)) {
      ncol <- 2;
  } 
  if (is.null(columnTextWidth)) {
    columnTextWidth <- 20;
  }
  if (is.null(title) && no.title == FALSE) {
    tryCatch({
      title <- paste("Odds Ratio with ", round(prob*100, 2), "% Highest Posterior Density Interval", sep="");
    }, error=function(e){})
  } 
  if (no.title == TRUE) {
    title <- NULL;
  }
  if (is.null(subtitle)) {
    tryCatch({
      subtitle <- paste(subjID, " (n, events)", sep="");
    }, error=function(e){})
  }
  
  intSubjs <- mcmcglmm_object$Sol[, which(grepl(paste(subjID, '.*?', sep=""), colnames(mcmcglmm_object$Sol)))];
  
  ranefSubjs <- cbind(est = MCMCglmm::posterior.mode(intSubjs), CI = coda::HPDinterval(intSubjs, prob=prob)); 
  rownames(ranefSubjs) <- sub(paste(subjID, ".", sep=""), '', rownames(t(intSubjs)));
  ranefSubjs <- as.data.frame(ranefSubjs);
  ranefSubjs$est <- exp(ranefSubjs$est);
  ranefSubjs$lower <- exp(ranefSubjs$lower);
  ranefSubjs$upper <- exp(ranefSubjs$upper);
  
  if (remove.text.subjID == TRUE) {
    rownames(ranefSubjs) <- gsub("[^0-9]", "", rownames(ranefSubjs)); #This removes non-numerical text from cow identifier, may not want to do if there are duplicate numbers when removing text; 
  }
  
  ranefSubjs$ID <- rownames(ranefSubjs);
  ranefSubjs$term <- reorder(factor(rownames(ranefSubjs)), ranefSubjs$est);
  
  instSubj <- orig_dataset |> 
    dplyr::rename(ID = subjID) |>
    dplyr::select(ID) |>
    dplyr::group_by(ID, .drop = FALSE) |>  
    dplyr::count(name="instances", .drop=FALSE) 
  hp_instSubj <- orig_dataset |> 
    dplyr::rename(ID = subjID) |>
    dplyr::select("ID", binaryOutcomeVar) |>
    dplyr::group_by(ID, .drop = FALSE) |>  
    dplyr::filter(get(binaryOutcomeVar) == 1) |>
    dplyr::count(get(binaryOutcomeVar), name="hp_instances") |>
    dplyr::select(-"get(binaryOutcomeVar)") 
  
  ranefSubjs <- plyr::join_all(list(ranefSubjs, instSubj[, c("ID", "instances")], hp_instSubj[, c("ID", "hp_instances")]), by=c("ID"), type='left', match = "first");
  ranefSubjs$instances[which(is.na(ranefSubjs$instances))] <- 0;
  ranefSubjs$hp_instances[which(is.na(ranefSubjs$hp_instances))] <- 0;
  
  if (break.label.summary == TRUE) {
    ranefSubjs$term <- stringr::str_wrap(ranefSubjs$term, width = columnTextWidth);
    ranefSubjs$term <- paste(ranefSubjs$term, "\n(", ranefSubjs$instances, ", ", ranefSubjs$hp_instances, ")", sep="");
  } else {
    ranefSubjs$term <- paste(ranefSubjs$term, " (", ranefSubjs$instances, ", ", ranefSubjs$hp_instances, ")", sep="");
    ranefSubjs$term <- stringr::str_wrap(ranefSubjs$term, width = columnTextWidth);
  }
  
  ranefSubjs$term <- reorder(factor(ranefSubjs$term), ranefSubjs$est);
  num_groups <- ncol;
  ranefSubjs <- ranefSubjs |>
    dplyr::arrange(est) |>
    dplyr::group_by(facet=(row_number()-1) %/% (n()/num_groups)+1)
  ranefSubjs$facet <- as.factor(ranefSubjs$facet);
  ranefSubjs$facet <- ordered(ranefSubjs$facet, levels = c(as.character(rev(seq(1:ncol)))));
  ranefSubjs$significance <- "normal";
  ranefSubjs$est <- as.numeric(format(round(ranefSubjs$est, 2), nsmall=2));
  ranefSubjs$lower <- as.numeric(format(round(ranefSubjs$lower, 2), nsmall=2));
  ranefSubjs$upper <- as.numeric(format(round(ranefSubjs$upper, 2), nsmall=2));
  ranefSubjs$significance[(ranefSubjs$lower >= 1.00 & ranefSubjs$upper >= 1.00) | (ranefSubjs$lower <= 1.00 & ranefSubjs$upper <= 1.00)] <- "different";
  
  options(warn=-1); #suppress warning messages;
  pPlot <- 
    ggplot(ranefSubjs,aes(term,est)) +
    geom_hline(yintercept=1, color="grey") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    geom_pointrange(aes(ymin=lower, ymax=upper, color=significance)) +
    guides(color=FALSE) +
    scale_color_manual(values=c("normal"="darkgrey", "different"="black")) +
    facet_wrap(~facet, dir="v", scales="free", ncol=ncol) +
    scale_y_continuous(limits = ~ c(0, ceiling(max(.x))), breaks = ~ c(0, floor(max(.x))), expand = c(0.10, 0.05)) +
    coord_flip() + 
    theme(plot.title=element_text(family=font.title, size=14, hjust=0.5), plot.subtitle=element_text(family=font.subtitle, size=12), axis.text.y=element_text(family=font.labels, size=8), axis.text.x=element_text(family=font.axis), axis.title.x=element_blank(), axis.title.y=element_blank()) + 
    theme(plot.title.position = "plot", plot.subtitle = element_text(hjust = 0.5), strip.background = element_blank(), strip.text.x = element_blank()) +
    theme(strip.text = element_blank(), panel.margin.y = unit(-0, "lines"), axis.ticks.y=element_blank()) +
    labs(title=paste(title), subtitle=paste(subtitle), caption="");
  
  return(pPlot)
}