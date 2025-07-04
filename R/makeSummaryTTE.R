#' Single data summary table for (R)TTE data
#'
#' @description Derive a single data summary table with up to 2 levels of stratification, specifically tailored for (R)TTE data.
#' It is a wrapper around `PhRame_makeSummaryTable()`, which means that it is the same function but defaulted with arguments
#' that use "event" instead of "observation". Any argument available in the documentation of `PhRame_makeSummaryTable()` can
#' be used (except for `myFun`, see the details in `Arguments` below). By default, the average number of event will be displayed
#' only for single event data, not for RTTE data.
#'
#' @param df is the data frame
#' @param myID is the name of the ID column, default is "ID"
#' @param myDV is the name of the DV column, default is "DV"
#' @param digits is the number of significant digits
#' @param showAvnObs a logical, show a column with the average number of events? Default will be `FALSE` if the data is recognized as RTTE data, `TRUE` if not.
#' @param nObsColNm is the character string to be printed as the column name with the number of observations, default is "\\\\textbf\{nEvent\\\\textsuperscript\{b\}\}"
#' @param avnObsColNm is the character string to be printed as the column name with the average number of observations per subject in a given strata, default is "\\\\textbf\{pEvent\\\\textsuperscript\{c\}\}"
#' @param avnObsPercent is a logical, should "pEvent" be printed as a percentage? TRUE
#' @param caption is the table caption. Assign NULL to this argument produce table without caption. Default is "Number of patients and number of events"
#' @param footnote is the text for footnote, default is "\\\\textsuperscript\{a\}Number of subjects\\\\newline\\\\textsuperscript\{b\}Number of events\\\\newline\\\\textsuperscript\{c\}Proportion of number of events" for TTE data.
#' @param myFun internal function for the calculation of summarized data. If NULL, the default, an internal function specific to R(TTE) data is used. This should not be changed for a standard use.
#' @param ...  passed to `PhRame_makeSummaryTable()`, possibly for the additional optional arguments compatible with latex
#'
#' @return By default, the table at Latex format. Alternatively, if `asList = TRUE`, a list of data.frame tables
#' @export
#'
#' @examples
#' rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
#' rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)
#' # create summary as a list
#' makeSummaryTableTTE(rttedata,
#'                     outerLevel   ="STUDYIDN" ,
#'                     outerLabel   = "Study",
#'                     innerLevel   = "DOSEN",
#'                     innerLabel   = "Dose",
#'                     asList = TRUE)
#'
#' # output as latex table
#' makeSummaryTableTTE(rttedata,
#'                     outerLevel   ="STUDYIDN" ,
#'                     outerLabel   = "Study",
#'                     innerLevel   = "DOSEN",
#'                     innerLabel   = "Dose")
#'
#' # Time to first event only
#' tte1data <- filter_xth_event(rttedata, event = 1)
#' makeSummaryTableTTE(tte1data,
#'                     outerLevel   ="STUDYIDN" ,
#'                     outerLabel   = "Study",
#'                     innerLevel   = "DOSEN",
#'                     innerLabel   = "Dose",
#'                     asList = TRUE)

makeSummaryTableTTE <- function(df,
                                myID = "ID",
                                myDV = "DV",
                                digits = 3,
                                showAvnObs = !isRTTE(df, myID = myID),
                                nObsColNm = "\\textbf{nEvent\\textsuperscript{b}}",
                                avnObsColNm = "\\textbf{pEvent\\textsuperscript{c}}",
                                avnObsPercent = TRUE,
                                caption = "Number of patients and number of events",
                                footnote = paste0(
                                  "\\textsuperscript{a}Number of subjects\\newline",
                                  "\\textsuperscript{b}Number of events\\newline",
                                  if(showAvnObs){"\\textsuperscript{c}Proportion of events"}
                                  ),
                                myFun = NULL,
                                ...){

  if(is.null(myFun)){
    myFun <- function(df){
      ans <- df %>%
        summarise(
          subjects = length(unique(!!rlang::sym(myID))),
          nObs     = length(c(!!rlang::sym(myDV))[!!rlang::sym(myDV)==1]),
          avnObs   = length(c(!!rlang::sym(myDV))[!!rlang::sym(myDV)==1])/length(unique(!!rlang::sym(myID)))
          )
      if(showAvnObs){
        if(avnObsPercent){
          ans$avnObs <- scales::percent(ans$avnObs, accuracy = (10^2 * 10^-digits))
        } else {
          ans$avnObs <- PhRame_out.digits(ans$avnObs, dig = digits, numeric = FALSE)
        }
      } else {
        ans$avnObs <- ""
      }
      ans
    }
  }

  if(!showAvnObs) avnObsColNm <- ""

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
