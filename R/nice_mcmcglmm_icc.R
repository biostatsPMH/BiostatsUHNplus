#' Nice table of intraclass correlation coefficients from MCMCglmm::MCMCglmm()
#' model output
#' 
#' @param mcmcglmm_object returned output from MCMCglmm()
#' @param prob probability for highest posterior density interval, similar to a 
#'    confidence interval. Defaul is 0.95 (if provided)
#' @param decimals number of decimal places to use in estimates
#' @keywords dataframe
#' @returns grouped_df
#' @importFrom MCMCglmm posterior.mode 
#' @importFrom coda HPDinterval
#' @export
#' @examples
#' data(ae)
#' ae$AE_SEV_GD <- as.numeric(ae$AE_SEV_GD);
#' ae$Drug_1_Attribution <- 0;
#' ae$Drug_1_Attribution[ae$CTC_AE_ATTR_SCALE %in% c("Definite", "Probable", "Possible")] <- 1;
#' ae$Drug_2_Attribution <- 0;
#' ae$Drug_2_Attribution[ae$CTC_AE_ATTR_SCALE_1 %in% c("Definite", "Probable", "Possible")] <- 1;
#' prior2RE <- list(R = list(V = diag(1), fix = 1), G=list(G1=list(V=1, nu=0.02), 
#'   G2=list(V=1, nu=0.02)));
#' model1 <- MCMCglmm::MCMCglmm(Drug_1_Attribution ~ AE_SEV_GD + Drug_2_Attribution, 
#'   random=~ae_detail + Subject, family="categorical", data=ae, saveX=TRUE, 
#'   verbose=FALSE, burnin=2000, nitt=10000, thin=10, pr=TRUE, prior=prior2RE);
#' mcmcglmm_icc <- nice_mcmcglmm_icc(model1, ae);
nice_mcmcglmm_icc <- function(mcmcglmm_object, prob=NULL, decimals=NULL) {
  
  if (is.null(prob)) {
    prob <- 0.95;
  } 
  if (is.null(decimals)) {
    decimals <- 4;
  } 
  re <- colnames(model1$VCV);
  ICC <- data.frame();
  for (i in 1:length(re)) {
    tmp <- mcmcglmm_object$VCV[, i]/(rowSums(mcmcglmm_object$VCV)); 
    df_tmp <- cbind(ICC = MCMCglmm::posterior.mode(tmp), CI = coda::HPDinterval(tmp, prob=prob));
    ICC <- rbind(ICC, df_tmp); 
    ICC <- round(ICC, 4);
  }
  row.names(ICC) <- re;
  return(ICC)
}