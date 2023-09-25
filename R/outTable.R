# Rmarkdown Reporting --------------------------------------------------------------

#' Print tables to PDF/Latex HTML or Word
#'
#' Output the table nicely to whatever format is appropriate. This is the output
#' function used by the rm_* printing functions.
#'
#' Entire rows can be bolded, or specific cells. Currently indentation refers to
#' the first column only. By default, underscores in column names are converted
#' to spaces. To disable this set rm_ to FALSE
#'
#' @param tab a table to format
#' @param row.names a string specifying the column name to assign to the
#'   rownames. If NULL (the default) then rownames are removed.
#' @param to_indent numeric vector indicating which rows to indent in the first
#'   column.
#' @param  bold_headers boolean indicating if the column headers should be
#'   bolded
#' @param rows_bold numeric vector indicating which rows to bold
#' @param bold_cells array indices indicating which cells to bold. These will be
#'   in addition to rows bolded by rows_bold.
#' @param caption table caption
#' @param digits number of digits to round numeric columns to, wither a single
#'   number or a vector corresponding to the number of numeric columns in tab
#' @param align string specifying column alignment, defaults to left alignment
#'   of the first column and right alignment of all other columns. The align
#'   argument accepts a single string with 'l' for left, 'c' for centre and 'r'
#'   for right, with no separations. For example, to set the left column to be
#'   centred, the middle column right-aligned and the right column left aligned
#'   use: align='crl'
#' @param applyAttributes boolean indicating if the function should use
#'   to_indent and bold_cells formatting attributes. This will only work
#'   properly if the dimensions of the table output from rm_covsum, rm_uvsum etc
#'   haven't changed.
#' @param keep.rownames should the row names be included in the output
#' @param nicenames boolean indicating if you want to replace . and _ in strings
#'   with a space
#' @param fontsize PDF/HTML output only, manually set the table fontsize
#' @param chunk_label only used knitting to Word docs to allow cross-referencing
#' @param format if specified ('html','latex') will override the 
#'   global pandoc setting
#' @return A character vector of the table source code, unless tableOnly=TRUE in
#'   which case a data frame is returned
#' @export
#' @examples
#' # To make custom changes or change the fontsize in PDF/HTML
#' tab <- rm_covsum(data=pembrolizumab,maincov = 'change_ctdna_group',
#' covs=c('age','sex','pdl1','tmb','l_size'),show.tests=TRUE,tableOnly = TRUE)
#' outTable(tab, fontsize=7)
#'
#' # To bold columns with the variable names
#'  rows_bold <- c(1,4,7,10,13)
#'  outTable(tab,rows_bold = rows_bold)
#'
#'  # To bold the estimates for male/female
#'  bold_cells <- as.matrix(expand.grid(5:6,1:ncol(tab)))
#'  outTable(tab,bold_cells= bold_cells)
outTable <- function(tab,row.names=NULL,to_indent=numeric(0),bold_headers=TRUE,
                     rows_bold=numeric(0),bold_cells=NULL,caption=NULL,digits=getOption("reportRmd.digits",2),align,
                     applyAttributes=TRUE,keep.rownames=FALSE, nicenames=TRUE,fontsize,chunk_label,format=NULL){
  
  # strip tibble aspects
  tab=as.data.frame(tab)
  if (!is.null(row.names)) {
    tab <- cbind(rownames(tab),tab)
    names(tab)[1] <- row.names
  }
  rownames(tab) <- NULL
  
  # define column alignment
  if (missing(align)){
    alignSpec = paste(c('l',rep('r',ncol(tab)-1)),collapse = '',sep='')
  } else{
    alignSpec = gsub('[^lrc]+','',paste(align,collapse=''))
    alignSpec = substr(alignSpec,1,ncol(tab))
    if (nchar(alignSpec)<ncol(tab)) {
      lastchar = substr(alignSpec,nchar(alignSpec),nchar(alignSpec))
      alignSpec <- paste0(alignSpec,paste(rep(lastchar,ncol(tab)-nchar(alignSpec)),collapse=''))
    }
    if (!identical(alignSpec, align)){ warning(paste0('Argument align did not conform to expectations, align="',alignSpec,'" used instead'))}
  }
  # round and format numeric columns if digits is specified
  if (!missing(digits)){
    coltypes <- unlist(lapply(tab, class))
    if (any(coltypes=='numeric')){
      numCols <- names(coltypes)[coltypes=='numeric']
      colRound <- cbind(numCols,digits)
      colDigits <- as.numeric(colRound[,2])
      names(colDigits) <- colRound[,1]
      for (v in numCols) tab[[v]] <- sapply(tab[[v]],function(x) niceNum(x,digits=colDigits[v]))
    }
  }
  
  out_fmt = ifelse(is.null(knitr::pandoc_to()),'html',
                   ifelse(knitr::pandoc_to(c('doc','docx')),'doc',
                          ifelse(knitr::is_latex_output(),'latex','html')))
  
  if (format %in% c('html','latex')){
    out_fmt = format
  }
  
  chunk_label = ifelse(missing(chunk_label),'NOLABELTOADD',chunk_label)
  
  if (applyAttributes){
    if (!is.null(attr(tab,'dimchk'))){
      if (all(attr(tab,'dimchk')==dim(tab))){
        if (!is.null(attr(tab,'to_indent'))) to_indent <- attr(tab,'to_indent')
        if (!is.null(attr(tab,'bold_cells'))) bold_cells <- attr(tab,'bold_cells')
      }
    }
  }
  if (is.null(to_indent)) to_indent = numeric(0)
  to_indent = as.vector(to_indent)
  if (nicenames) names(tab) <- reportRmd:::nicename(names(tab))
  if (length(rows_bold)>0){
    arrInd <- as.matrix(expand.grid(rows_bold,1:ncol(tab)))
    bold_cells <- rbind(bold_cells,arrInd)
    dimnames(bold_cells) <- NULL
    bold_cells <- bold_cells[!duplicated(bold_cells),]
  }
  if (!is.null(bold_cells)){
    bold_cells <- bold_cells[!duplicated(bold_cells),,drop=FALSE]
    bold_cells <- bold_cells[!is.na(tab[bold_cells]),,drop=FALSE]
  }
  if (out_fmt=='doc'){
    caption = if (!is.null(caption)) {ifelse(chunk_label=='NOLABELTOADD',caption,paste0('(\\#tab:',chunk_label,')',caption))}
    tab[is.na(tab)] <-'&nbsp;' # This is necessary to assign the 'Compact' style to empty cells
    tab[tab==''] <-'&nbsp;'
    
    tab[[1]][to_indent] <- sapply(tab[[1]][to_indent],function(x) paste('&nbsp;&nbsp;',x))
    pander::pander(tab,
                   caption=caption,
                   emphasize.strong.cells=bold_cells,
                   split.table=Inf, split.cells=15,
                   justify = alignSpec)
    
    
  } else {  # For PDF, HTML
    # set NA to empty in kable
    oldop <- options()
    on.exit(options(oldop))
    options(knitr.kable.NA = '')
    if (out_fmt=='latex') {
      names(tab) <- reportRmd:::sanitize(names(tab))
      if(!is.null(caption)) caption <- reportRmd:::sanitize(caption)
      for (v in 1:ncol(tab)) tab[[v]] <- reportRmd:::sanitize(tab[[v]])
      if (!is.null(bold_cells)) tab[bold_cells] <- sapply(tab[bold_cells],function(x) reportRmd:::lbld(x))
    }
    if (out_fmt=='html') {
      if (!is.null(bold_cells)) tab[bold_cells] <- sapply(tab[bold_cells],function(x) reportRmd:::hbld(x))
      for (v in 1:ncol(tab)) tab[[v]] <- reportRmd:::rmds(tab[[v]])
    }
    if (nrow(tab)>30){
      kout <- knitr::kable(tab, format = out_fmt,
                           escape = FALSE,
                           booktabs=TRUE,
                           longtable=TRUE,
                           linesep='',
                           caption=caption,
                           align =alignSpec)
      kout <- kableExtra::kable_styling(kout,latex_options = c('repeat_header'))
    } else {
      kout <- knitr::kable(tab, format = out_fmt,
                           escape = FALSE,
                           booktabs=TRUE,
                           longtable=FALSE,
                           linesep='',
                           caption=caption,
                           align = alignSpec)
    }
    kout <- kableExtra::add_indent(kout,positions = to_indent)
    if (!missing(fontsize)){
      kout <- kableExtra::kable_styling(kout,font_size = fontsize)
    }
    if (out_fmt=='html'){
      kout <- kableExtra::kable_styling(kout,full_width = TRUE)
    }
    kout
  }
}