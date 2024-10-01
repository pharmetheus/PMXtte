#' Title
#'
#' @param nEventColNm the charactrer string to be printed as the column name with number of events
#' @param avnEventColNm the charactrer string to be printed as the column name with average number of events
#' @inheritDotParams PhRame::makeSummaryTable
#' @inheritParams PhRame::makeSummaryTable
#'
#' @return
#' @export
#'
#' @importFrom PhRame makeSummaryTable
#'
#' @examples
# ttedata  <- readr::read_csv(system.file('extdata/anaDATAf.csv', package= 'PMXtte'))
#
# #create summary as a list
# makeSummaryTableTTE(ttedata,
#                     outerLevel   = "DOSEN",
#                     outerLabel   = "Dose",
#                     innerLevel   = "STUDYIDN",
#                     innerLabel   = "Study",
#                     asList = TRUE)
#
# #output as latex table
# makeSummaryTableTTE(ttedata,
#                     outerLevel   = "DOSEN",
#                     outerLabel   = "Dose",
#                     innerLevel   = "STUDYIDN",
#                     innerLabel   = "Study")
makeSummaryTableTTE <- function(df,
                                myID = "ID",
                                myDV = "DV",
                                digits = 3,
                                nIdColNm = "\\textbf{nID\\textsuperscript{a}}",
                                nEventColNm = "\\textbf{nEvent\\textsuperscript{b}}",
                                avnEventColNm = "\\textbf{pEvent/nID\\textsuperscript{c}}",
                                caption = "Number of patients and number of events",
                                label = "tab:anaSummary",
                                footnote = "\\textsuperscript{a}Number of patients\\newline\\textsuperscript{b}Number of events\\newline\\textsuperscript{c}Proportion of number of events",
                                ...){



tteFun <- function(df){
  df %>%
    summarise(subjects = length(unique(!!rlang::sym(myID))),
              nObs     = length(c(!!rlang::sym(myDV))[!!rlang::sym(myDV)==1]),
              avnObs   = out.digits(length(c(!!rlang::sym(myDV))[!!rlang::sym(myDV)==1])/length(unique(!!rlang::sym(myID))), dig = digits, numeric = F))
}
makeSummaryTable(df = df,
                 myID = myID,
                 myDV = myDV,
                 digits = digits,
                 nIdColNm = nIdColNm,
                 nObsColNm = nEventColNm,
                 avnObsColNm = avnEventColNm,
                 caption = caption,
                 label = label,
                 footnote = footnote,
                 myFun = tteFun,
                 ...
)
}
