#' Single data summary table for (R)TTE data
#'
#' @param df
#' @param addFactors
#' @param filterExpr
#' @param myID
#' @param myDV
#' @param myEVCOUNT
#' @param outerLevel
#' @param innerLevel
#' @param outerLabel
#' @param innerLabel
#' @param digits
#' @param lumpCount
#' @param dropCount0
#' @param nIdColNm
#' @param nObsColNm
#' @param caption
#' @param label
#' @param footnote
#' @param footnoteSize
#' @param textSize
#' @param here
#' @param numeric.dollar
#' @param asList
#' @param saveTex
#' @param fileName
#' @param filePath
#' @param myFun
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
    nObsColNm = NULL,
    caption = "PK analysis data set: Number of subjects with observations and number of events",
    label = "tab:anaSummary",
    footnote = "\\textsuperscript{a}Number of subjects",
    footnoteSize = "footnotesize",
    textSize = "small",
    here = TRUE,
    numeric.dollar = FALSE,
    asList = FALSE,
    saveTex = FALSE,
    fileName = NULL,
    filePath = "./",
    myFun = NULL,
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
  if (is.null(myFun)) {
    myFun <- function(x) {
      ans1 <- x %>%
        mutate(EVCOUNT = factor_and_lump(!!rlang::sym(myEVCOUNT), lump = lumpCount)) %>%
        count(EVCOUNT, .drop = dropCount0) %>%
        tidyr::pivot_wider(
          names_from = !!rlang::sym(myEVCOUNT),
          values_from = n
          )
      ans2 <- x %>%
        summarise(
          # use summarise() because `x` can be a grouped data frame.
          subjects = length(unique(!!rlang::sym(myID)))
          )
      ans1$subjects <- ans2$subjects # no mutate() we dont want a groupwise operation
      ans1
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
    res_tab <- lapply(tab_comb, as.numeric)
  }
  if (asList | (is.null(outerLevel) & is.null(innerLevel))) {
    return(res_tab)
  }
  else {
    res_tab <- res_tab %>% droplevels() %>% as.data.frame()
    Ntable <- res_tab %>% dplyr::select(subjects, matches("^\\d+$"))
    if (is.null(outerLabel)) {
      outerLabel <- outerLevel
    }
    if (is.null(innerLabel)) {
      innerLabel <- innerLevel
    }
    if (!is.null(outerLabel) & !is.null(innerLabel)) {
      rowLabel <- paste0("\\textbf{", outerLabel, " / ",
                         innerLabel, "}")
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

    coljust <- rep("S", ncol(Ntable))
    if(is.null(nObsColNm)){
      nObsColNm <- seq_len(ncol(Ntable)-1) # 0, 1, 2 etc..
    }

    if (!is.null(outerLevel) & !is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = unique(as.character(res_tab[, outerLevel])),
                   rowname = as.character(res_tab[, innerLevel]),
                   rowlabel = rowLabel, n.rgroup = tapply((res_tab[,
                                                                   outerLevel]), (res_tab[, outerLevel]), length),
                   colheads = c(nIdColNm, nObsColNm),
                   col.just = coljust, size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar, ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = unique(as.character(res_tab[, outerLevel])),
                            rowname = as.character(res_tab[, innerLevel]),
                            rowlabel = rowLabel, n.rgroup = tapply((res_tab[,
                                                                            outerLevel]), (res_tab[, outerLevel]), length),
                            colheads = c(nIdColNm, nObsColNm),
                            col.just = coljust, size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar, ...)
      }
    }
    else if (!is.null(outerLevel) & is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = NULL, rowname = as.character(res_tab[,
                                                                 outerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                   colheads = c(nIdColNm, nObsColNm),
                   col.just = coljust, size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar, ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = NULL, rowname = as.character(res_tab[,
                                                                          outerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                            colheads = c(nIdColNm, nObsColNm),
                            col.just = coljust, size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar, ...)
      }
    }
    else if (is.null(outerLevel) & !is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = NULL, rowname = as.character(res_tab[,
                                                                 innerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                   colheads = c(nIdColNm, nObsColNm),
                   col.just = coljust, size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar, ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = NULL, rowname = as.character(res_tab[,
                                                                          innerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                            colheads = c(nIdColNm, nObsColNm),
                            col.just = coljust, size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar, ...)
      }
    }
  }
}

factor_and_lump <- function(x, lump = Inf){
  lastval  <- max(x)
  lastname <- max(x)
  if(lump <= 0){
    stop("Cannot lump counts <= 0")
  }
  if(lastval > lump){
    x[x>=lump] <- lump
    lastval <- lump
    lastname <- paste0(lump, " or more")
  }
  factor(
    x,
    levels = c(seq(0, max(x)-1), lastval),
    labels = c(seq(0, max(x)-1), lastname)
  )
}

