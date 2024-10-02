#' Title
#'
#' @param nEventColNm the charactrer string to be printed as the column name with number of events
#' @param avnEventColNm the charactrer string to be printed as the column name with average number of events
#'
#' @return
#' @export
#'
#' @examples
#' ttedata  <- readr::read_csv(system.file('extdata/anaDATAf.csv', package= 'PMXtte'), show_col_types = FALSE)
#'
#create summary as a list
#' makeSummaryTableTTE(ttedata,
#'                     outerLevel   = "DOSEN",
#'                     outerLabel   = "Dose",
#'                     innerLevel   = "STUDYIDN",
#'                     innerLabel   = "Study",
#'                     asList = TRUE)
#'
#' #output as latex table
#' makeSummaryTableTTE(ttedata,
#'                     outerLevel   = "DOSEN",
#'                     outerLabel   = "Dose",
#'                     innerLevel   = "STUDYIDN",
#'                     innerLabel   = "Study")
makeSummaryTableTTE <- function(df,
                                myID = "ID",
                                myDV = "DV",
                                digits = 3,
                                nObsColNm = "\\textbf{nEvent\\textsuperscript{b}}",
                                avnObsColNm = "\\textbf{pEvent/nID\\textsuperscript{c}}",
                                caption = "Number of patients and number of events",
                                footnote = "\\textsuperscript{a}Number of patients\\newline\\textsuperscript{b}Number of events\\newline\\textsuperscript{c}Proportion of number of events",
                                myFun = NULL,
                                ...){
  if(is.null(myFun)){
    myFun <- function(df){
      df %>%
        summarise(
          subjects = length(unique(!!rlang::sym(myID))),
          nObs     = length(c(!!rlang::sym(myDV))[!!rlang::sym(myDV)==1]),
          avnObs   = PhRame_out.digits(
            length(c(!!rlang::sym(myDV))[!!rlang::sym(myDV)==1])/length(unique(!!rlang::sym(myID))),
            dig = digits,
            numeric = F)
        )
    }
  }

  PhRame_makeSummaryTable(
    df = df,
    myID = "ID",
    myDV = "DV",
    digits = 3,
    nObsColNm = nObsColNm,
    avnObsColNm = avnObsColNm,
    caption = caption,
    footnote = footnote,
    myFun = myFun,
    ...
  )
}
