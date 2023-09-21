#' Nice model output from MCMCglmm::MCMCglmm()
#' 
#' @param mcmcglmm_object returned output from MCMCglmm()
#' @param data dataframe containing data
#' @keywords dataframe
#' @importFrom tibble rownames_to_column 
#' @importFrom plyr join_all 
#' @importFrom purrr modify_if 
#' @importFrom dplyr select mutate arrange group_by 
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' mcmcglmm_mva <- nice_mcmcglmm(model1, Milk);
#' options(knitr.kable.NA = '');
#' knitr::kable(mcmcglmm_mva);
nice_mcmcglmm <- function(mcmcglmm_object, dataset) {
  cc <- summary(mcmcglmm_object)$solutions
  citab <- with(as.data.frame(cc),
                cbind(RR_HPDI_95 = paste0(format(round(exp(cc[,1]), 2), nsmall=2), 
                                          " (", format(round(exp(cc[,2]), 2), nsmall=2), ", ", 
                                          format(round(exp(cc[,3]), 2), nsmall=2), ")", sep=""),
                      MCMCp = format(round(cc[,5], 3), nsmall=3), eff_sample = format(round(cc[,4], 2), nsmall=2) ))
  rownames(citab) <- rownames(cc)
  mcmcglmm_ci <- citab;
  mcmcglmm_ci <- as.data.frame(mcmcglmm_ci);
  mcmcglmm_ci <- tibble::rownames_to_column(mcmcglmm_ci, "Variable");
  mcmcglmm_ci <- mcmcglmm_ci[-1,];
  colnames(mcmcglmm_ci) <- c("Variable", "RR (95% HPDI)", "MCMCp", "eff.samp");
  mcmcglmm_ci$join <- mcmcglmm_ci$Variable;
  
  tryCatch({
    varLevels <- do.call(rbind, lapply(sapply(dataset[, c(all.vars(mcmcglmm_object$Fixed$formula)[-1])], levels), data.frame))
    varLevels <- tibble::rownames_to_column(varLevels, "Variable");
    colnames(varLevels) <- c("Variable", "Levels");
    varLevels$Variable <- gsub("(.*)\\.(.*)", "\\1", varLevels$Variable);
    varLevels$join <- paste(varLevels$Variable, varLevels$Levels, sep="");
  }, error=function(e){return(printErr <- NA)})
  
  if (is.null(varLevels)) {
    varLevels <- as.data.frame(cbind(mcmcglmm_ci$Variable, NA, mcmcglmm_ci$join))
  }
  colnames(varLevels) <- c("Variable", "Levels", "join");
  
  opd_mcmcglmm <- plyr::join_all(list(varLevels, mcmcglmm_ci), by=c("join"), type='full');
  opd_mcmcglmm <- opd_mcmcglmm |> modify_if(is.factor, as.character);
  
  origVar <- as.data.frame(all.vars(mcmcglmm_object$Fixed$formula)[-1]);
  colnames(origVar) <- "Variable";
  origVar <- origVar |>
    dplyr::mutate(OrigOrder = 1:n())
  
  opd_mcmcglmm <- plyr::join_all(list(opd_mcmcglmm, origVar), by=c("Variable"), type='full');
  
  tryCatch({
    opd_mcmcglmm[which(is.na(opd_mcmcglmm$"RR (95% HPDI)")), ]$"RR (95% HPDI)" <- "reference";
  }, error=function(e){return(printErr <- NA)})
  opd_mcmcglmm <- opd_mcmcglmm 
    dplyr::select(-join) 
    dplyr::mutate(Ovar = match(Variable, unique(Variable)))  
    dplyr::group_by(Variable)  
    dplyr::mutate(instance = 1:n())
  tryCatch({
    opd_mcmcglmm[which(opd_mcmcglmm$"RR (95% HPDI)" == "reference"), ]$instance <- 0;
  }, error=function(e){return(printErr <- NA)})
  opd_mcmcglmm <- opd_mcmcglmm 
    dplyr::arrange(Variable, instance) 
    dplyr::arrange(OrigOrder,instance) 
    dplyr::select(-instance, -Ovar, -OrigOrder)
  opd_mcmcglmm$Variable <- gsub("_", " ", opd_mcmcglmm$Variable);
  opd_mcmcglmm$Variable[duplicated(opd_mcmcglmm$Variable )] <- NA;
  opd_mcmcglmm$MCMCp[opd_mcmcglmm$MCMCp == "0.000"] <- "<0.001";
  opd_mcmcglmm;
}