#' Single data summary table for RTTE data
#'
#' @description Derive a single data summary table with up to 2 levels of stratification, counting the number of events in each group.
#' It is similar to `PhRame_makeSummaryTable()`, the same arguments should apply the same to both functions. There is a possibility to
#' lump counts of event (e.g., if `lumpCount = 3`, a column "3 or more" will be shown). It is possible to remove
#' counts if there are no events with `dropCount0 = TRUE`.
#'
#' @param df is the data frame (e.g. anaDataF) or data object created using
#'   \code{pmxdata::derived_data} (e.g. DATobject)
#' @param addFactors if \strong{TRUE}, factors are added to the input data.
#'   Default is \strong{FALSE}. This option is used iff \code{df} is a data
#'   object
#' @param filterExpr is an expression used to filter the data. Either a formula
#'   using non standard evaluation (e.g. \code{quo(AMT==0 & TYPE==1)}) or a
#'   character string with filter expression (e.g. "(AMT==0 & TYPE==1)") is
#'   acceptable to this argument.
#' @param myID is the name of the ID column, default is \strong{"ID"}
#' @param myDV is the name of the DV column, default is \strong{"DV"}
#' @param myEVCOUNT is the name of the EVCOUNT column, default is \strong{"EVCOUNT"}
#' @param outerLevel is the outer level (1st level) of stratification variable,
#'   eg. "STUDYIDN"
#' @param innerLevel is the inner level (2nd level) of stratification variable,
#'   eg. "TRTF"
#' @param outerLabel is the label for the outer level (1st level) of
#'   stratification variable, eg. "Study")
#' @param innerLabel is the label for the inner level (2nd level) of
#'   stratification variable, eg. "Treatment"
#' @param digits is the number of significant digits
#' @param lumpCount is the maximum number of event to display, e.g. if `lumpcount=3`,
#'   the category "3 or more" will be display. Only works for Latex code.
#'   Default is \strong{Inf} (no lumping)
#' @param dropCount0 is a logical, should categories with zero event be dropped.
#'   Only works for Latex code. Default is \strong{FALSE} (don't drop)
#' @param nIdColNm is the character string to be printed as the column name
#'   with the number of subjects, default is
#'   \strong{"Total"}
#' @param nEventColNm is the character string to be printed as the column names
#'   with the number of subjects with this number of events. Default is `NULL`,
#'   because this is dynamically generated depending on the data.
#' @param caption is the table caption. Assign \strong{NULL} (the default) to this argument
#'   produce table without caption.
#' @param label is the label for the table used for cross-reference, default is
#'   \strong{"tab:anaSummary"}
#' @param footnote is the text for footnote.
#' @param footnoteSize is the footnote text size, default is
#'   \strong{"footnotesize"}
#' @param textSize is the text size, default is \strong{"small"}
#' @param here is the logical argument determining the placement of the LaTex
#'   table. If set to \strong{TRUE} with optional argument \code{table.env=TRUE}
#'   with \code{longtable=FALSE}, will cause the LaTeX table environment to be
#'   set up with option H to guarantee that the table will appear exactly where
#'   you think it will in the text. Default is \strong{TRUE}
#' @param numeric.dollar is the logical argument determining if the numbers will
#'   be placed within $ symbols, default is \strong{FALSE}
#' @param asList if \strong{TRUE} the data summary is returned as a list where
#'   different elements in the list contain summary of different stratification
#'   levels. If \strong{FALSE} the data summary is returned as a latex table. If
#'   no stratification variable is provided, this function always returns a list
#' @param saveTex is the logical argument determining if the LaTex code for the
#'   table will be saved on disk. Default is \strong{FALSE}
#' @param fileName is the name of the Tex file (without extension) with the
#'   LaTex code for the table saved on disk, eg. if \code{fileName="mytable"} is
#'   provided, a file called \code{"mytable.tex"} will be saved on disk. Default
#'   is \strong{NULL}.
#' @param filePath is the path to the directory where the Tex file is saved.
#'   Default is \strong{"/"}.
#' @param ... is the additional optional arguments compatible with latex
#' @param myFun internal function for the calculation of summarized data. If
#'   NULL, the default, an internal function specific to R(TTE) data is used.
#'   This should not be changed for a standard use.
#' @param cgroup is the names of the column groups, to be display as the first
#'   row of column name.
#' @param n.cgroup is the number of columns for which each element of cgroup is
#'   a heading. Default is `NULL`, because this is dynamically generated
#'    depending on the data.
#'
#' @return By default, the table at Latex format. Alternatively, if
#'   `asList = TRUE`, a list of data.frame tables.
#'
#' @export
#'
#' @examples
#' rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
#' rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)
#' rttedata$TRTF <- factor(paste(rttedata$TRTN, "mg"), levels = paste(sort(unique(rttedata$TRTN)), "mg"))
#'
#' # create summary as a list
#' summaryCountRTTE(rttedata,
#'                     outerLevel   ="STUDYIDN" ,
#'                     outerLabel   = "Study",
#'                     innerLevel   = "TRTF",
#'                     innerLabel   = "Treatment",
#'                     asList = TRUE)
#'
#'
#' # output as latex table
#' summaryCountRTTE(rttedata,
#'                     outerLevel   ="STUDYIDN" ,
#'                     outerLabel   = "Study",
#'                     innerLevel   = "TRTF",
#'                     innerLabel   = "Treatment")
#' summaryCountRTTE(rttedata,
#'                     outerLevel   ="STUDYIDN" ,
#'                     outerLabel   = "Study",
#'                     innerLevel   = "TRTF",
#'                     innerLabel   = "Treatment",
#'                     lumpCount    = 3
#'                     )
summaryCountRTTE <- function(
    df,
    addFactors = FALSE,
    filterExpr = NULL,
    myID = "ID",
    myDV = "DV",
    myEVCOUNT = "EVCOUNT",
    outerLevel = NULL,
    innerLevel = NULL,
    outerLabel = NULL,
    innerLabel = NULL,
    digits = 3,
    lumpCount = Inf,
    dropCount0 = FALSE,
    nIdColNm = "\\textbf{nID\\textsuperscript{a}}",
    nEventTotColNm =  "\\textbf{nEventTot\\textsuperscript{b}}",
    nEventColNm = NULL,
    caption = NULL,
    label = "tab:anaSummary",
    footnote = "\\textsuperscript{a}Total number of subjects. \\textsuperscript{b}Total number of events.\\newline\\textsuperscript{c}Number of subjects with the indicated cumulative number of events.",
    footnoteSize = "footnotesize",
    textSize = "small",
    here = TRUE,
    numeric.dollar = FALSE,
    asList = FALSE,
    saveTex = FALSE,
    fileName = NULL,
    filePath = "./",
    myFun = NULL,
  #  cgroup = c("\\textbf{Total}", "\\textbf{nID\\textsuperscript{a} with the indicated cumulative nEvent\\textsuperscript{b}}"),
  cgroup = c("\\textbf{Total}", "\\textbf{nID per nEvent\\textsuperscript{c}}"),
  n.cgroup = NULL,
    ...
){
  if ((!is.null(rlang::peek_option("save.script")) && rlang::peek_option("save.script") ==
       TRUE) && !str_detect(as.character(match.call()[1]),
                            "_script")) {
    argsList <- c(list(funcName = match.call()[1] %>% as.character() %>%
                         str_remove(".+:+") %>% str_c("PhRame:::", .) %>%
                         str_remove("\\(\\)")), as.list(environment()), list(...))
    invisible(do.call("save_script", argsList))
  }
  if (is.data.frame(df)) {
    df <- as.data.frame(df) %>% droplevels()
  }
  else if (all(class(df) == c("data_file", "derived_data"))) {
    if(!requireNamespace("pmxdata", quietly = TRUE)) {
      stop(
        "Package \"pmxdata\" must be installed to use derived_data ",
        call. = FALSE
      )
    }
    df <- pmxdata::analysis_data(deriveddatafile = df, addFactors = addFactors)
    if (!is.null(filterExpr)) {
      if (is_formula(filterExpr)) {
        df <- df %>% filter(!!filterExpr) %>% as.data.frame() %>%
          droplevels()
      }
      if (!is_formula(filterExpr)) {
        if (!is_formula(eval(parse(text = paste0("quo(",
                                                 filterExpr, ")"))))) {
          stop("If filterExpr is not in non standard evaluation format, this must be a character string with R-compatible expression used to filter data.\n")
        }
        else {
          df <- df %>% filter(eval(parse(text = paste0("quo(",
                                                       filterExpr, ")")))) %>% as.data.frame() %>%
            droplevels()
        }
      }
    }
  }
  else {
    stop("df must be either a data frame or a data object.\n")
  }
  if (saveTex && is.null(fileName)) {
    stop("Provide a name for the file where LaTex table code will be saved.\n")
  }
  if (!myID %in% names(df)) {
    stop("Value provided for myID, used for the name of ID column, is not present in the data.\n")
  }
  if (!myDV %in% names(df)) {
    stop("Value provided for myDV, used for the name of DV column, is not present in the data.\n")
  }
  if (!myEVCOUNT %in% names(df)) {
    stop("Value provided for myEVCOUNT, used for the name of EVCOUNT column, is not present in the data.\n")
  }
  if (!is.null(outerLevel) && !(outerLevel %in% names(df))) {
    stop("Value provided for outerLevel is not present in the data.\n")
  }
  if (!is.null(innerLevel) && !(innerLevel %in% names(df))) {
    stop("Value provided for innerLevel is not present in the data.\n")
  }
  if (is.null(outerLevel) & !is.null(outerLabel)) {
    outerLabel <- NULL
  }
  if (is.null(innerLevel) & !is.null(innerLabel)) {
    innerLabel <- NULL
  }
  if (!is.null(outerLevel) & !is.factor(outerLevel)) {
    df[, outerLevel] <- factor(df[, outerLevel])
  }
  if (!is.null(innerLevel) & !is.factor(innerLevel)) {
    df[, innerLevel] <- factor(df[, innerLevel])
  }
  maxcountOverall <- max(ungroup(df)[[myEVCOUNT]])
  if (is.null(myFun)) {
    myFun <- function(x) {
      ans1 <- x %>%
        # EVCOUNT as factor so that groups with n=0 subjects do not drop.
        mutate(EVCOUNT = factor(!!rlang::sym(myEVCOUNT), levels = seq(0,maxcountOverall))) %>%
        group_by(!!rlang::sym(myID), .add = TRUE) %>%
        slice_tail(n = 1) %>%
        ungroup(!!rlang::sym(myID)) %>%
        count(EVCOUNT, .drop = FALSE) %>%
        filter(!all(n == 0)) %>%
        tidyr::pivot_wider(
          names_from = !!rlang::sym(myEVCOUNT),
          values_from = n
          )
      ans2 <- x %>%
        summarise(
          # use summarise() because `x` can be a grouped data frame.
          subjects = length(unique(!!rlang::sym(myID))),
          nEventTot = sum(.data[[myDV]])
        )
      ans1$subjects <- ans2$subjects # no mutate() we dont want a groupwise operation
      ans1$nEventTot <- ans2$nEventTot # no mutate() we dont want a groupwise operation
      ans1 %>%
        as.data.frame()
    }
  }
  if (!is.null(outerLevel) | !is.null(innerLevel)) {
    if (!asList) {
      res_tab <- PhRame_dataSummaryBy_comb(df, fn = myFun,
                                           grp_var = c(outerLevel, innerLevel))
    }
    if (asList) {
      res_tab <- PhRame_dataSummaryBy_all(df, fn = myFun,
                                          grp_var = c(outerLevel, innerLevel))
    }
  }
  else {
    tab_comb <- PhRame_dataSummaryBy_comb(df, fn = myFun)
    res_tab <- as.list(tab_comb)
  }
  if (asList | (is.null(outerLevel) & is.null(innerLevel))) {
    return(res_tab)
  }
  else {
    res_tab <- res_tab %>% droplevels() %>% as.data.frame()
    Ntable <- res_tab %>%
      dplyr::select(any_of(c("subjects", "nEventTot")), matches("^\\d+$")) %>%
      mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x))) %>%
      lump_drop_columns(
        lump = lumpCount,
        dropCount0 = dropCount0
      )
    if (is.null(outerLabel)) {
      outerLabel <- outerLevel
    }
    if (is.null(innerLabel)) {
      innerLabel <- innerLevel
    }
    if (!is.null(outerLabel) & !is.null(innerLabel)) {
      rowLabel <- paste0("\\textbf{", outerLabel, " / ", innerLabel, "}")
    }
    if (!is.null(outerLabel) & is.null(innerLabel)) {
      rowLabel <- paste0("\\textbf{", outerLabel, "}")
    }
    if (is.null(outerLabel) & !is.null(innerLabel)) {
      rowLabel <- paste0("\\textbf{", innerLabel, "}")
    }
    if (!is.null(footnote)) {
      footnote <- paste0("\\", footnoteSize, " ", footnote)
    }

    if(is.null(nEventColNm)){
      nEventColNm <- setdiff(colnames(Ntable), c("subjects", "nEventTot"))
      nEventColNm <- paste0("\\textbf{", nEventColNm, "}")
    }

    coljust <- rep("S", ncol(Ntable))
    if(is.null(n.cgroup)){
      n.cgroup <- c(2, ncol(Ntable)-2)
    }

    if (!is.null(outerLevel) & !is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = unique(as.character(res_tab[, outerLevel])),
                   rowname = as.character(res_tab[, innerLevel]),
                   rowlabel = rowLabel, n.rgroup = tapply((res_tab[,
                                                                   outerLevel]), (res_tab[, outerLevel]), length),
                   colheads = c(nIdColNm, nEventTotColNm, nEventColNm),
                   col.just = coljust, size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar,
                   cgroup = cgroup, n.cgroup = n.cgroup,
                   ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = unique(as.character(res_tab[, outerLevel])),
                            rowname = as.character(res_tab[, innerLevel]),
                            rowlabel = rowLabel, n.rgroup = tapply((res_tab[,
                                                                            outerLevel]), (res_tab[, outerLevel]), length),
                            colheads = c(nIdColNm, nEventTotColNm, nEventColNm),
                            col.just = coljust, size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar,
                            cgroup = cgroup, n.cgroup = n.cgroup, ...)
      }
    }
    else if (!is.null(outerLevel) & is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = NULL, rowname = as.character(res_tab[,
                                                                 outerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                   colheads = c(nIdColNm, nEventTotColNm, nEventColNm),
                   col.just = coljust, size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar,
                   cgroup = cgroup, n.cgroup = n.cgroup, ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = NULL, rowname = as.character(res_tab[,
                                                                          outerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                            colheads = c(nIdColNm, nEventTotColNm, nEventColNm),
                            col.just = coljust, size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar,
                            cgroup = cgroup, n.cgroup = n.cgroup, ...)
      }
    }
    else if (is.null(outerLevel) & !is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = NULL, rowname = as.character(res_tab[,
                                                                 innerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                   colheads = c(nIdColNm, nEventTotColNm, nEventColNm),
                   col.just = coljust, size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar,
                   cgroup = cgroup, n.cgroup = n.cgroup, ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = NULL, rowname = as.character(res_tab[,
                                                                          innerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                            colheads = c(nIdColNm, nEventTotColNm, nEventColNm),
                            col.just = coljust, size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar,
                            cgroup = cgroup, n.cgroup = n.cgroup, ...)
      }
    }
  }
}


lump_drop_columns <- function(x, lump = Inf, dropCount0 = FALSE){

  ans <- x

  counts <- as.double(names(x)[grepl("^\\d+$", names(x))])

  if(lump <= 0){
    stop("Cannot lump counts <= 0")
  }
  if(max(counts) > lump){
    lumpname <- paste(lump, "or more")
    ans[[lumpname]] <- rowSums(ans %>% select(any_of(as.character(lump:max(counts)))))
    ans[as.character(lump:max(counts))] <- NULL
  }

  if(dropCount0){
    ans[sapply(ans, function(.x) all(.x == 0))] <- NULL
  }

  ans

}
