covsum <- utils::getFromNamespace("covsum", "reportRmd")
csep <- utils::getFromNamespace("csep", "reportRmd")
formatp <- utils::getFromNamespace("formatp", "reportRmd")
lpvalue <- utils::getFromNamespace("lpvalue", "reportRmd")
niceNum <- utils::getFromNamespace("niceNum", "reportRmd")
nicename <- utils::getFromNamespace("nicename", "reportRmd")

#' Nested version of reportRmd:::covsum()
#'
#' @param data dataframe containing data
#' @param covs character vector with the names of columns to include in table
#' @param maincov covariate to stratify table by
#' @param id covariates to nest summary by
#' @param digits number of digits for summarizing mean data, does not affect
#'   p-values
#' @param numobs named list overriding the number of people you expect to have
#'   the covariate
#' @param markup boolean indicating if you want latex markup
#' @param sanitize boolean indicating if you want to sanitize all strings to not
#'   break LaTeX
#' @param nicenames boolean indicating if you want to replace . and _ in strings
#'   with a space
#' @param IQR boolean indicating if you want to display the inter quantile range
#'   (Q1,Q3) as opposed to (min,max) in the summary for continuous variables
#' @param all.stats boolean indicating if all summary statistics (Q1,Q3 +
#'   min,max on a separate line) should be displayed. Overrides IQR.
#' @param pvalue boolean indicating if you want p-values included in the table
#' @param effSize boolean indicating if you want effect sizes included in the 
#'   table. Can only be obtained if pvalue is also requested.
#' @param show.tests boolean indicating if the type of statistical used should
#'   be shown in a column beside the pvalues. Ignored if pvalue=FALSE.
#' @param dropLevels logical, indicating if empty factor levels be dropped from
#'   the output, default is TRUE.
#' @param excludeLevels a named list of covariate levels to exclude from
#'   statistical tests in the form list(varname =c('level1','level2')). These
#'   levels will be excluded from association tests, but not the table. This can
#'   be useful for levels where there is a logical skip (ie not missing, but not
#'   presented). Ignored if pvalue=FALSE.
#' @param full boolean indicating if you want the full sample included in the
#'   table, ignored if maincov is NULL
#' @param digits.cat number of digits for the proportions when summarizing
#'   categorical data (default: 0)
#' @param testcont test of choice for continuous variables,one of
#'   \emph{rank-sum} (default) or \emph{ANOVA}
#' @param testcat test of choice for categorical variables,one of
#'   \emph{Chi-squared} (default) or \emph{Fisher}
#' @param include_missing Option to include NA values of maincov. NAs will not
#'   be included in statistical tests
#' @param percentage choice of how percentages are presented ,one of
#'   \emph{column} (default) or \emph{row}
#' @keywords dataframe
#' @importFrom stats lm sd
#' @importFrom rstatix cramer_v eta_squared
#' @importFrom dplyr select summarise group_by filter across row_number
#' @importFrom purrr modify_if 
#' @importFrom rlang syms 
#' @importFrom modeest mlv 
#' @importFrom parallel detectCores makeCluster
#' @importFrom afex mixed
#' @importFrom utils getFromNamespace
#' @importFrom magrittr "%>%"
#' @seealso \code{\link{fisher.test}},\code{\link{chisq.test}},
#'   \code{\link{wilcox.test}},\code{\link{kruskal.test}},and
#'   \code{\link{anova}}
covsum_nested <- function (data, covs, maincov = NULL, id = NULL, digits = 1, numobs = NULL, 
                           markup = TRUE, sanitize = TRUE, nicenames = TRUE, IQR = FALSE, 
                           all.stats = FALSE, pvalue = TRUE, effSize = TRUE, show.tests = TRUE, excludeLevels = NULL, 
                           full = TRUE, digits.cat = 0, testcont = c("rank-sum test", 
                                                                     "ANOVA"), testcat = c("Chi-squared", "Fisher"), 
                           include_missing = FALSE, percentage = c("column", "row")) 
{
  #-#-#-#-#-#-#-#-#-#-#-#-#
  if (missing(id)) 
    stop("id is a required argument")
  warning("Use this function at your own risk. Please check output.\nOrder of nested ids matter. For example, in c('id1','id2') id1 should be nested within id2, etc.\n")
  nested.pvalue=FALSE
  if (pvalue){ 
    nested.pvalue=TRUE
    nc <- parallel::detectCores() # number of cores
    warning(paste("Unnested p-value and statistical test is incorrect for nested data, but is kept for comparison to nested p-value.\nNested p-value derived from anova(afex::mixed(maincov ~ cov + (1|id1:id2:...idn), family=binomial, data, method='LRT')).\n", "\nUsing ", nc, " processor(s) for parallel processing.\n", sep=""))
  }
  options(dplyr.summarise.inform = FALSE)
  is.date <- function(x) inherits(x, 'Date')
  covsIdData1 <- function(covs = covs, id = id, data = data, excludeLevels = excludeLevels){
    id <- c(id, NULL)
    tto <- data |>
      purrr::modify_if(is.character, as.factor) |>
      dplyr::select(!!!(rlang::syms(covs)), !!!(rlang::syms(id))) |>
      dplyr::group_by(!!!(rlang::syms(id))) |>
      dplyr::reframe(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.date), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.factor), ~ modeest::mlv(.x, method = mfv))) |>
      dplyr::group_by(!!!(rlang::syms(id))) |>
      dplyr::filter(dplyr::row_number() == ceiling(n()/2))
    tto <- as.data.frame(tto)
    tto
  }
  covsIdData2 <- function(covs = covs, id = id, data = data, excludeLevels = excludeLevels){
    tto <- data |>
      purrr::modify_if(is.character, as.factor) |>
      dplyr::select(!!!(rlang::syms(covs)), !!!(rlang::syms(id))) |>
      dplyr::group_by(!!!(rlang::syms(id))) |>
      dplyr::reframe(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.date), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.factor), ~ modeest::mlv(.x, method = mfv))) |>
      dplyr::group_by(!!!(rlang::syms(id))) |>
      dplyr::filter(dplyr::row_number() == ceiling(n()/2))
    tto <- as.data.frame(tto)
    tto
  }
  maincovCovsIdData1 <- function(maincov = maincov, covs = covs, id = id, data = data, excludeLevels = excludeLevels){
    id <- c(id, maincov)
    tto <- data |>
      purrr::modify_if(is.character, as.factor) |>
      dplyr::select(!!!(rlang::syms(maincov)), !!!(rlang::syms(covs)), !!!(rlang::syms(id))) |>
      dplyr::group_by(!!!(rlang::syms(id))) |>
      dplyr::reframe(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.date), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.factor), ~ modeest::mlv(.x, method = mfv))) |>
      dplyr::group_by(!!!(rlang::syms(id))) |>
      dplyr::filter(dplyr::row_number() == ceiling(n()/2))
    tto <- as.data.frame(tto)
    tto
  }
  maincovCovsIdData2 <- function(maincov = maincov, covs = covs, id = id, data = data, excludeLevels = excludeLevels){
    tto <- data |>
      purrr::modify_if(is.character, as.factor) |>
      dplyr::select(!!!(rlang::syms(maincov)), !!!(rlang::syms(covs)), !!!(rlang::syms(id))) |>
      dplyr::group_by(!!!(rlang::syms(id))) |>
      dplyr::reframe(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.date), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.factor), ~ modeest::mlv(.x, method = mfv))) |>
      dplyr::group_by(!!!(rlang::syms(id)), .drop=FALSE) |>
      dplyr::filter(dplyr::row_number() == ceiling(n()/2))
    tto <- as.data.frame(tto)
    tto
  }
  if (is.null(maincov) & !is.null(id)){ 
    data1 <- covsIdData1(covs, id, data)
    data2 <- covsIdData2(covs, id, data)
  }
  else if (!is.null(maincov) & !is.null(id)) {
    data1 <- maincovCovsIdData1(maincov, covs, id, data)
    data2 <- maincovCovsIdData2(maincov, covs, id, data)
    dataWithoutMaincov1 <- covsIdData1(covs, id, data) 
    dataWithoutMaincov2 <- covsIdData2(covs, id, data)
  }
  else {
    data <- data
  }
  #-#-#-#-#-#-#-#-#-#-#-#-#
  #obj1 <- reportRmd:::covsum(data = data1, covs = covs, maincov = maincov)
  #obj2 <- reportRmd:::covsum(data = data2, covs = covs, maincov = NULL, dropLevels = FALSE)
  obj1 <- reportRmd:::covsum(data = data1, covs = covs, maincov = maincov, digits=digits, numobs=numobs, markup=markup, sanitize=sanitize, nicenames=nicenames, IQR=IQR, all.stats=all.stats, pvalue=pvalue, effSize=effSize, show.tests=show.tests, excludeLevels=excludeLevels, full=full, digits.cat=digits.cat, testcont=testcont, testcat=testcat, include_missing=include_missing, percentage=percentage)
  obj2 <- reportRmd:::covsum(data = data2, covs = covs, maincov = NULL, dropLevels = FALSE, digits=digits, numobs=numobs, markup=markup, sanitize=sanitize, nicenames=nicenames, IQR=IQR, all.stats=all.stats, pvalue=pvalue, effSize=effSize, show.tests=show.tests, excludeLevels=excludeLevels, full=full, digits.cat=digits.cat, testcont=testcont, testcat=testcat, include_missing=include_missing, percentage=percentage)
  objComb <- cbind(obj2, obj1[, -c(1:2)]);
  colnames(objComb)[2] <- paste("Full Sample (", colnames(objComb)[2], ")", sep="");
  
  #------------# LRT glmer nested pvalues #------------#;
  ###https://search.r-project.org/CRAN/refmans/afex/html/mixed.html
  if (nested.pvalue & !is.null(maincov) & !is.null(id)) {
    objComb$cov <- "";
    objComb$cov[which(objComb[2] == "")] <- covs;
    objComb$'Nested p-value' <- "";
    suppressWarnings({
      tryCatch({
        cl <- parallel::makeCluster(rep("localhost", nc)) # make cluster
        suppressWarnings({tryCatch({
          out_glmer <- lapply(objComb$cov[which(objComb$cov != "")], function(x) try(as.numeric(anova(afex::mixed(as.formula(paste(maincov, '~', x, '+(', 1, '|', paste(id, collapse=':'), ')', sep='')), family=binomial, data=data, expand_re=TRUE, cl=cl, method="LRT"))[4]), silent=TRUE))
          #out_glmer <- lapply(objComb$cov[which(objComb$cov != "")], function(x) try(as.numeric(anova(afex::mixed(as.formula(paste(maincov, '~', x, '+(', x, '|', paste(id, collapse=':'), ')', sep='')), family=binomial, data=data, cl=cl, method="LRT"))[4]), silent=TRUE))
        }, error=function(e){})})
        try(stopCluster(cl), silent=TRUE)
      }, error=function(e){})
      suppressWarnings({
        tryCatch({
          try(stopCluster(cl), silent=TRUE)
        }, error=function(e){})
      })
      out_glmer <- as.numeric(unlist(out_glmer));
      objComb$'Nested p-value'[which(objComb$cov != "")] <- unlist(out_glmer);
      objComb <- objComb[, which(names(objComb) != "cov")];
    })
    objComb;
  }
  else {
    objComb;
  }
}


#' Outputs a nested version of reportRmd:::rm_covsum()
#'
#' @param data dataframe containing data
#' @param covs character vector with the names of columns to include in table
#' @param maincov covariate to stratify table by
#' @param id covariates to nest summary by
#' @param caption character containing table caption (default is no caption)
#' @param tableOnly Logical, if TRUE then a dataframe is returned, otherwise a
#'   formatted printed object is returned (default).
#' @param covTitle character with the names of the covariate (predictor) column.
#'   The default is to leave this empty for output or, for table only output to
#'   use the column name 'Covariate'.
#' @param digits number of digits for summarizing mean data
#' @param digits.cat number of digits for the proportions when summarizing
#'   categorical data (default: 0)
#' @param nicenames boolean indicating if you want to replace . and _ in strings
#'   with a space
#' @param IQR boolean indicating if you want to display the inter quantile range
#'   (Q1,Q3) as opposed to (min,max) in the summary for continuous variables
#' @param all.stats boolean indicating if all summary statistics (Q1,Q3 +
#'   min,max on a separate line) should be displayed. Overrides IQR.
#' @param pvalue boolean indicating if you want p-values included in the table
#' @param effSize boolean indicating if you want effect sizes included in the
#'   table. Can only be obtained if pvalue is also requested.
#' @param p.adjust p-adjustments to be performed
#' @param unformattedp boolean indicating if you would like the p-value to be
#'   returned unformatted (ie not rounded or prefixed with '<'). Best used with
#'   tableOnly = T and outTable function. 
#' @param show.tests boolean indicating if the type of statistical used should
#'   be shown in a column beside the pvalues. Ignored if pvalue=FALSE.
#' @param testcont test of choice for continuous variables,one of
#'   \emph{rank-sum} (default) or \emph{ANOVA}
#' @param testcat test of choice for categorical variables,one of
#'   \emph{Chi-squared} (default) or \emph{Fisher}
#' @param full boolean indicating if you want the full sample included in the
#'   table, ignored if maincov is NULL
#' @param include_missing Option to include NA values of maincov. NAs will not
#'   be included in statistical tests
#' @param percentage choice of how percentages are presented, one of
#'   \emph{column} (default) or \emph{row}
#' @param dropLevels logical, indicating if empty factor levels be dropped from
#'   the output, default is TRUE.
#' @param excludeLevels a named list of covariate levels to exclude from
#'   statistical tests in the form list(varname =c('level1','level2')). These
#'   levels will be excluded from association tests, but not the table. This can
#'   be useful for levels where there is a logical skip (ie not missing, but not
#'   presented). Ignored if pvalue=FALSE.
#' @param numobs named list overriding the number of people you expect to have
#'   the covariate
#' @param chunk_label only used if output is to Word to allow cross-referencing
#' @keywords dataframe
#' @return A character vector of the table source code, unless tableOnly=TRUE in
#'   which case a data frame is returned
#' @importFrom stats lm sd
#' @importFrom rstatix cramer_v eta_squared
#' @importFrom dplyr select summarise group_by filter across row_number
#' @importFrom purrr modify_if 
#' @importFrom rlang syms 
#' @importFrom modeest mlv 
#' @importFrom parallel detectCores makeCluster
#' @importFrom afex mixed
#' @export
#' @seealso \code{\link{covsum}},\code{\link{fisher.test}},
#'   \code{\link{chisq.test}}, \code{\link{wilcox.test}},
#'   \code{\link{kruskal.test}}, \code{\link{anova}}, and \code{\link{outTable}}
#' @examples
#' rm_covsum_nested(data = Milk, id = c("Cow"), covs = c("protein", "Time", 
#' "Diet", "Yard"), maincov = "High_Protein")
rm_covsum_nested <- function(data,covs,maincov=NULL,caption=NULL,tableOnly=FALSE,covTitle='',
                             digits=1,digits.cat = 0,nicenames=TRUE,IQR = FALSE,all.stats=FALSE,
                             pvalue=TRUE,effSize=TRUE,p.adjust='none',unformattedp = FALSE,show.tests=TRUE,
                             testcont = c('rank-sum test','ANOVA'),testcat = c('Chi-squared','Fisher'),
                             full=TRUE,include_missing=FALSE,percentage=c('column','row'),
                             excludeLevels=NULL,numobs=NULL,markup=TRUE, sanitize= TRUE,chunk_label,...){
  
  if (unformattedp |p.adjust !='none')
    formatp <- function(x) {
      as.numeric(x)
    }
  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argsToPass <- intersect(names(formals(covsum_nested)), names(argList))
  covsumArgs <- argList[names(argList) %in% argsToPass]
  covsumArgs[["markup"]] <- FALSE
  covsumArgs[["sanitize"]] <- FALSE
  covsumArgs[["nicenames"]] <- FALSE
  tab <- do.call(covsum_nested, covsumArgs)
  colnames(tab)[1] <- "Covariate"
  output_var_names <- covs
  Sys.sleep(1)
  to_indent <- which(!tab$Covariate %in% output_var_names)
  to_bold_name <- which(tab$Covariate %in% output_var_names)
  bold_cells <- arrayInd(to_bold_name, dim(tab))

  if (nicenames) tab$Covariate <- reportRmd:::replaceLbl(argList$data, tab$Covariate)
  names(tab)[1] <- covTitle
  if ("p-value" %in% names(tab)) {
    if (p.adjust!='none'){
      tab[["p (unadjusted)"]] <- tab[["p-value"]]
      tab[["p-value"]] <- sapply(tab[["p-value"]],function(x) p.adjust(x,method=p.adjust))
    }
    to_bold_p <- which(as.numeric(tab[["p-value"]]) < 0.05)
    p_vals <- tab[["p-value"]]
    new_p <- sapply(p_vals, reportRmd:::formatp)
    tab[["p-value"]] <- new_p
    if (length(to_bold_p) > 0)
      bold_cells <- rbind(bold_cells, matrix(cbind(to_bold_p,
                                                   which(names(tab) == "p-value")), ncol = 2))
  }
  if ("Effect Size" %in% names(tab)) {
    e_vals <- tab[["Effect Size"]]
    new_e <- sapply(e_vals,reportRmd:::formatp)
    tab[["Effect Size"]] <- new_e
  }
  if ('Nested p-value' %in% names(tab)) {
    # format p-values nicely
    to_bold_p <- which(tab[["Nested p-value"]]<.05 & !tab[["Nested p-value"]]=="")
    p_vals <- tab[['Nested p-value']]
    new_p <- sapply(p_vals,reportRmd:::formatp)
    tab[['Nested p-value']] <- new_p
    if (length(to_bold_p)>0)    bold_cells <- rbind(bold_cells,
                                                    matrix(cbind(to_bold_p, which(names(tab)=='Nested p-value')),ncol=2))
  }
  tryCatch({
    if (length(which(tab$'p-value' != '' & is.na(tab$'Nested p-value'))) > 0) {
      tab$'Nested p-value'[tab$'p-value' != '' & is.na(tab$'Nested p-value')] <- 'Did not converge;<br>quasi or complete<br>category separation';
    }
  }, error=function(e){})
  
  if (tableOnly){
    if (names(tab)[1]=='') names(tab)[1]<- 'Covariate'
    attr(tab, "to_indent") <- to_indent
    attr(tab, "bold_cells") <- bold_cells
    attr(tab, "dimchk") <- dim(tab)
    return(tab)
  }
  
  if ('p-value' %in% names(tab)) 
    colnames(tab)[colnames(tab) == 'p-value'] <- 'Unnested p-value'
  if ('StatTest' %in% names(tab)) 
    colnames(tab)[colnames(tab) == 'StatTest'] <- 'Unnested StatTest'
  suppressWarnings({
    tryCatch({
      try(stopCluster(cl), silent=TRUE)
    }, error=function(e){})
  })
  
  
  outTable(tab=tab,to_indent=to_indent,bold_cells = bold_cells,
           caption=caption,
           chunk_label=ifelse(missing(chunk_label),'NOLABELTOADD',chunk_label))
  
}