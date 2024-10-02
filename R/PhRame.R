PhRame_makeSummaryTable <- function (df, addFactors = FALSE, filterExpr = NULL, myID = "ID",
                                     myDV = "DV", outerLevel = NULL, innerLevel = NULL, outerLabel = NULL,
                                     innerLabel = NULL, digits = 3, nIdColNm = "\\textbf{nID\\textsuperscript{a}}",
                                     nObsColNm = "\\textbf{nObs\\textsuperscript{b}}", avnObsColNm = "\\textbf{nObs/nID\\textsuperscript{c}}",
                                     caption = "PK analysis data set: Number of subjects with observations and number of observations",
                                     label = "tab:anaSummary", footnote = "\\textsuperscript{a}Number of subjects\\newline\\textsuperscript{b}Number of observations\\newline\\textsuperscript{c}Average number of observations per subject",
                                     footnoteSize = "footnotesize", textSize = "small", here = TRUE,
                                     numeric.dollar = FALSE, asList = FALSE, saveTex = FALSE,
                                     fileName = NULL, filePath = "./", myFun = NULL, ...)
{
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
      x %>% dplyr::summarise(
        subjects = length(unique(!!rlang::sym(myID))),
        nObs = length(!!rlang::sym(myDV)),
        avnObs = PhRame_out.digits(
          length(!!rlang::sym(myDV))/length(unique(!!rlang::sym(myID))),
          dig = digits, numeric = TRUE)
        )
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
    res_tab <- list(nID = as.numeric(tab_comb$subjects),
                    nObs = as.numeric(tab_comb$nObs), avnObs = as.numeric(tab_comb$avnObs))
  }
  if (asList | (is.null(outerLevel) & is.null(innerLevel))) {
    return(res_tab)
  }
  else {
    res_tab <- res_tab %>% droplevels() %>% as.data.frame()
    Ntable <- res_tab %>% dplyr::select(subjects, nObs,
                                        avnObs)
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
    if (!is.null(outerLevel) & !is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = unique(as.character(res_tab[, outerLevel])),
                   rowname = as.character(res_tab[, innerLevel]),
                   rowlabel = rowLabel, n.rgroup = tapply((res_tab[,
                                                                   outerLevel]), (res_tab[, outerLevel]), length),
                   colheads = c(nIdColNm, nObsColNm, avnObsColNm),
                   col.just = rep("S", 3), size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar, ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = unique(as.character(res_tab[, outerLevel])),
                            rowname = as.character(res_tab[, innerLevel]),
                            rowlabel = rowLabel, n.rgroup = tapply((res_tab[,
                                                                            outerLevel]), (res_tab[, outerLevel]), length),
                            colheads = c(nIdColNm, nObsColNm, avnObsColNm),
                            col.just = rep("S", 3), size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar, ...)
      }
    }
    else if (!is.null(outerLevel) & is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = NULL, rowname = as.character(res_tab[,
                                                                 outerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                   colheads = c(nIdColNm, nObsColNm, avnObsColNm),
                   col.just = rep("S", 3), size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar, ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = NULL, rowname = as.character(res_tab[,
                                                                          outerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                            colheads = c(nIdColNm, nObsColNm, avnObsColNm),
                            col.just = rep("S", 3), size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar, ...)
      }
    }
    else if (is.null(outerLevel) & !is.null(innerLevel)) {
      Hmisc::latex(Ntable, file = "", first.hline.double = FALSE,
                   rgroup = NULL, rowname = as.character(res_tab[,
                                                                 innerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                   colheads = c(nIdColNm, nObsColNm, avnObsColNm),
                   col.just = rep("S", 3), size = textSize, insert.bottom = footnote,
                   caption = caption, label = label, here = here,
                   numeric.dollar = numeric.dollar, ...)
      if (saveTex) {
        tmp <- Hmisc::latex(Ntable, file = paste0(filePath,
                                                  fileName, ".tex"), first.hline.double = FALSE,
                            rgroup = NULL, rowname = as.character(res_tab[,
                                                                          innerLevel]), rowlabel = rowLabel, n.rgroup = NULL,
                            colheads = c(nIdColNm, nObsColNm, avnObsColNm),
                            col.just = rep("S", 3), size = textSize, insert.bottom = footnote,
                            caption = caption, label = label, here = here,
                            numeric.dollar = numeric.dollar, ...)
      }
    }
  }
}

PhRame_dataSummaryBy_all <- function (x, addFactors = FALSE, filterExpr = NULL, fn, grp_var = NULL)
{
  if ((!is.null(rlang::peek_option("save.script")) && rlang::peek_option("save.script") ==
       TRUE) && !str_detect(as.character(match.call()[1]),
                            "_script")) {
    argsList <- c(list(funcName = match.call()[1] %>% as.character() %>%
                         str_remove(".+:+") %>% str_c("PhRame:::", .) %>%
                         str_remove("\\(\\)")), as.list(environment()))
    return(do.call("save_script", argsList))
  }
  if (is.data.frame(x)) {
    x <- as.data.frame(x) %>% droplevels()
  }
  else if (all(class(x) == c("data_file", "derived_data"))) {
    x <- pmxdata::analysis_data(deriveddatafile = x, addFactors = addFactors)
    if (!is.null(filterExpr)) {
      if (is_formula(filterExpr)) {
        x <- x %>% filter(!!filterExpr) %>% as.data.frame() %>%
          droplevels()
      }
      if (!is_formula(filterExpr)) {
        if (!is_formula(eval(parse(text = paste0("quo(",
                                                 filterExpr, ")"))))) {
          stop("If filterExpr is not in non standard evaluation format, this must be a character string with R-compatible expression used to filter data.\n")
        }
        else {
          x <- x %>% filter(eval(parse(text = paste0("quo(",
                                                     filterExpr, ")")))) %>% as.data.frame() %>%
            droplevels()
        }
      }
    }
  }
  else {
    stop("x must be either a data frame or a data object.\n")
  }
  tab_all <- x %>% droplevels() %>% fn() %>% mutate(stratlvl = 0)
  tab <- grp_var %>% purrr::map(~rep("All", nrow(tab_all))) %>% set_names(grp_var) %>%
    bind_cols(tab_all, .) %>% list()
  tmp <- c()
  for (i in seq_along(grp_var)) {
    tmp <- c(tmp, grp_var[i])
    tab_str <- x %>% droplevels() %>% group_by_at(vars(tmp)) %>%
      fn() %>% ungroup() %>% mutate(stratlvl = i)
    left_arg <- grp_var[!grp_var %in% grp_var[1:i]]
    tab_str <- left_arg %>% purrr::map(~rep("All", nrow(tab_str))) %>%
      set_names(left_arg) %>% bind_cols(tab_str, .) %>%
      list()
    tab <- c(tab, tab_str)
  }
  return(tab)
}

PhRame_dataSummaryBy_comb <- function (x, addFactors = FALSE, filterExpr = NULL, fn, grp_var = NULL)
{
  if ((!is.null(rlang::peek_option("save.script")) && rlang::peek_option("save.script") ==
       TRUE) && !str_detect(as.character(match.call()[1]),
                            "_script")) {
    argsList <- c(list(funcName = match.call()[1] %>% as.character() %>%
                         str_remove(".+:+") %>% str_c("PhRame:::", .) %>%
                         str_remove("\\(\\)")), as.list(environment()))
    return(do.call("save_script", argsList))
  }
  if (is.data.frame(x)) {
    x <- as.data.frame(x) %>% droplevels()
  }
  else if (all(class(x) == c("data_file", "derived_data"))) {
    x <- pmxdata::analysis_data(deriveddatafile = x, addFactors = addFactors)
    if (!is.null(filterExpr)) {
      if (is_formula(filterExpr)) {
        x <- x %>% filter(!!filterExpr) %>% as.data.frame() %>%
          droplevels()
      }
      if (!is_formula(filterExpr)) {
        if (!is_formula(eval(parse(text = paste0("quo(",
                                                 filterExpr, ")"))))) {
          stop("If filterExpr is not in non standard evaluation format, this must be a character string with R-compatible expression used to filter data.\n")
        }
        else {
          x <- x %>% filter(eval(parse(text = paste0("quo(",
                                                     filterExpr, ")")))) %>% as.data.frame() %>%
            droplevels()
        }
      }
    }
  }
  else {
    stop("x must be either a data frame or a data object.\n")
  }
  tab <- PhRame_dataSummaryBy_all(x, fn = fn, grp_var = grp_var)
  if (is.null(grp_var)) {
    tab <- tab[[1]] %>% as.data.frame()
    return(tab)
  }
  else {
    tab_comb <- suppressWarnings(tab %>% invoke(bind_rows,
                                                .))
    for (i in seq_along(grp_var)) {
      tab_comb[, grp_var[i]] <- factor(tab_comb[, grp_var[i]],
                                       levels = c(levels(x[, grp_var[i]]), "All"))
    }
    if (length(grp_var) == 1) {
      tab_comb <- tab_comb %>% dplyr::arrange(tab_comb[,
                                                       grp_var[1]], desc(stratlvl)) %>% mutate(interactstrat = length(unique(.data[[grp_var[1]]]))) %>%
        filter(c(stratlvl != 1 | interactstrat != 2)) %>%
        droplevels() %>% as.data.frame()
    }
    else if (length(grp_var) == 2) {
      tab_comb <- tab_comb %>% dplyr::arrange(tab_comb[,
                                                       grp_var[1]], desc(stratlvl), tab_comb[, grp_var[2]]) %>%
        group_by_at(grp_var[1]) %>% mutate(interactstrat = length(unique(interaction(.data[[grp_var[1]]],
                                                                                     .data[[grp_var[2]]])))) %>% ungroup() %>% filter(c(stratlvl !=
                                                                                                                                          1 | interactstrat != 2)) %>% droplevels() %>%
        as.data.frame()
    }
    else {
      stop("This function currently supports upto 2 leves of stratification.\n")
    }
    tab_comb <- tab_comb %>% dplyr::select(-one_of("stratlvl",
                                                   "interactstrat"))
    return(tab_comb)
  }
}

PhRame_out.digits <- function (x, dig = 3, no99 = TRUE, no9900 = FALSE, noSmall = FALSE,
          scientific = FALSE, numeric = FALSE, warning = TRUE)
{
  if ((!is.null(rlang::peek_option("save.script")) && rlang::peek_option("save.script") ==
       TRUE) && !str_detect(as.character(match.call()[1]),
                            "_script")) {
    argsList <- c(list(funcName = as.character(match.call()[1]) %>%
                         as.character() %>% str_remove("()")), as.list(environment()))
    return(do.call("save_script", argsList))
  }
  options(warn = -1)
  if (length(x) == 0) {
    return("")
  }
  if (is.character(x)) {
    return(x)
  }
  y <- c()
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      y <- c(y, NA)
    }
    else if (x[i] == -99 & no99) {
      y <- c(y, "")
    }
    else if (x[i] == -9900 & no9900) {
      y <- c(y, "")
    }
    else if (abs(x[i]) <= 1e-06 & noSmall) {
      y <- c(y, 0)
    }
    else {
      x[i] <- as.numeric(as.character(x[i]))
      tmp <- case_when(x[i] == 0 ~ "0", abs(round(x[i],
                                                  4)) <= 1 * 10^-(dig + 1) ~ as.character(formatC(x[i],
                                                                                                  digits = dig - 1, format = "fg")), abs(round(x[i],
                                                                                                                                               1)) < 1 * 10^(dig - 1) ~ as.character(formatC(signif(x[i],
                                                                                                                                                                                                    dig), digits = dig, format = "fg", flag = "#")),
                       abs(round(x[i], 1)) < 1 * 10^(dig + 1) ~ as.character(signif(x[i],
                                                                                    dig)), (abs(round(x[i], 1)) >= 1 * 10^(dig +
                                                                                                                             1)) & (dig > 1) ~ as.character(formatC(signif(x[i],
                                                                                                                                                                           dig), digits = dig - 1, format = "fg")), (abs(round(x[i],
                                                                                                                                                                                                                               1)) >= 1 * 10^(dig + 1)) & (dig <= 1) ~ as.character(formatC(signif(x[i],
                                                                                                                                                                                                                                                                                                   dig), digits = 1, format = "fg")), warning ~
                         "check out.digits function", TRUE ~ "")
      if (scientific) {
        tmp <- as.character(formatC(x[i], digits = dig -
                                      1, format = "E"))
      }
      if (numeric) {
        y <- c(y, as.numeric(as.character(tmp)))
      }
      else {
        y <- c(y, tmp)
      }
    }
  }
  return(y)
  options(warn = 0)
}


