covsum_nested <- function (data, covs, maincov = NULL, id = NULL, digits = 1, numobs = NULL, 
                           markup = TRUE, sanitize = TRUE, nicenames = TRUE, IQR = FALSE, 
                           all.stats = FALSE, pvalue = TRUE, show.tests = FALSE, excludeLevels = NULL, 
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
  covsIdData1 <- function(covs = covs, id = id, data = data, excludeLevels = excludeLevels){
    id <- c(id, NULL)
    tto <- data %>%
      purrr::modify_if(is.character, as.factor) %>%
      dplyr::select(!!!(rlang::syms(covs)), !!!(rlang::syms(id))) %>%
      dplyr::group_by(!!!(rlang::syms(id))) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.factor), ~ modeest::mlv(.x, method = mfv))) %>%
      dplyr::group_by(!!!(rlang::syms(id))) %>%
      dplyr::filter(dplyr::row_number() == 1)
    tto <- as.data.frame(tto)
    tto
  }
  covsIdData2 <- function(covs = covs, id = id, data = data, excludeLevels = excludeLevels){
    tto <- data %>%
      purrr::modify_if(is.character, as.factor) %>%
      dplyr::select(!!!(rlang::syms(covs)), !!!(rlang::syms(id))) %>%
      dplyr::group_by(!!!(rlang::syms(id))) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.factor), ~ modeest::mlv(.x, method = mfv))) %>%
      dplyr::group_by(!!!(rlang::syms(id))) %>%
      dplyr::filter(dplyr::row_number() == 1)
    tto <- as.data.frame(tto)
    tto
  }
  maincovCovsIdData1 <- function(maincov = maincov, covs = covs, id = id, data = data, excludeLevels = excludeLevels){
    id <- c(id, maincov)
    tto <- data %>%
      purrr::modify_if(is.character, as.factor) %>%
      dplyr::select(!!!(rlang::syms(maincov)), !!!(rlang::syms(covs)), !!!(rlang::syms(id))) %>%
      dplyr::group_by(!!!(rlang::syms(id))) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.factor), ~ modeest::mlv(.x, method = mfv))) %>%
      dplyr::group_by(!!!(rlang::syms(id))) %>%
      dplyr::filter(dplyr::row_number() == 1)
    tto <- as.data.frame(tto)
    tto
  }
  maincovCovsIdData2 <- function(maincov = maincov, covs = covs, id = id, data = data, excludeLevels = excludeLevels){
    tto <- data %>%
      purrr::modify_if(is.character, as.factor) %>%
      dplyr::select(!!!(rlang::syms(maincov)), !!!(rlang::syms(covs)), !!!(rlang::syms(id))) %>%
      dplyr::group_by(!!!(rlang::syms(id))) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), dplyr::across(where(is.factor), ~ modeest::mlv(.x, method = mfv))) %>%
      dplyr::group_by(!!!(rlang::syms(id))) %>%
      dplyr::filter(dplyr::row_number() == 1)
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
  #obj2 <- reportRmd:::covsum(data = data2, covs = covs, maincov = NULL)
  obj1 <- reportRmd:::covsum(data = data1, covs = covs, maincov = maincov, digits=digits, numobs=numobs, markup=markup, sanitize=sanitize, nicenames=nicenames, IQR=IQR, all.stats=all.stats, pvalue=pvalue, show.tests=show.tests, excludeLevels= excludeLevels, full=full, digits.cat=digits.cat, testcont=testcont, testcat=testcat, include_missing=include_missing, percentage=percentage)
  obj2 <- reportRmd:::covsum(data = data2, covs = covs, maincov = NULL, digits=digits, numobs=numobs, markup=markup, sanitize=sanitize, nicenames=nicenames, IQR=IQR, all.stats=all.stats, pvalue=pvalue, show.tests=show.tests, excludeLevels= excludeLevels, full=full, digits.cat=digits.cat, testcont=testcont, testcat=testcat, include_missing=include_missing, percentage=percentage)
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

#tab <- objComb;
rm_covsum_nested <- function(data,covs,maincov=NULL,caption=NULL,tableOnly=FALSE,covTitle='',
                             digits=1,digits.cat = 0,nicenames=TRUE,IQR = FALSE,all.stats=FALSE,pvalue=TRUE, show.tests=FALSE,
                             testcont = c('rank-sum test','ANOVA'),testcat = c('Chi-squared','Fisher'),
                             full=TRUE,include_missing=FALSE,percentage=c('column','row'),
                             excludeLevels=NULL,numobs=NULL,markup=TRUE, sanitize= TRUE,chunk_label,...){
  
  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argsToPass <- intersect(names(formals(covsum_nested)),names(argList))
  covsumArgs <- argList[names(argList) %in% argsToPass]
  covsumArgs[["markup"]] <- FALSE; covsumArgs[["sanitize"]] <- FALSE
  tab <- do.call(covsum_nested,covsumArgs)
  if (nicenames) output_var_names <- gsub('[_.]',' ',covs) else output_var_names <- covs
  to_indent <- which(!tab$Covariate %in% output_var_names)
  to_bold_name <- which(tab$Covariate %in% output_var_names)
  if (nicenames) tab$Covariate <- gsub('[_.]',' ',tab$Covariate)
  names(tab)[1] <-covTitle
  bold_cells <- arrayInd(to_bold_name, dim(tab))
  
  if ('p-value' %in% names(tab)) {
    # format p-values nicely
    to_bold_p <- which(tab[["p-value"]]<.05 & !tab[["p-value"]]=="")
    p_vals <- tab[['p-value']]
    new_p <- sapply(p_vals,reportRmd:::formatp)
    tab[['p-value']] <- new_p
    if (length(to_bold_p)>0)    bold_cells <- rbind(bold_cells,
                                                    matrix(cbind(to_bold_p, which(names(tab)=='p-value')),ncol=2))
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