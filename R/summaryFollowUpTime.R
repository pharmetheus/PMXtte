#' Single data summary table for follow-up times with RTTE data
#'
#' @description Derive a single data summary table with up to 2 levels of stratification, in order to display the number
#' of events, the total follow-up time and the annualized event rates. It is a wrapper around `PhRame_makeSummaryTable()`,
#' which means that it is the same function but defaulted with arguments that use "event" instead of "observation".
#' Any argument available in the documentation of `PhRame_makeSummaryTable()` can be used (except for `myFun`,
#' see the details in `Arguments` below). New arguments include the possibility to handle the Time and Event Counts
#' variables, and possibly conversion in the relevant units.
#'
#' @param myTIME Column used to calculate annual time, default is `"TSFDH"`
#' @param myEVCOUNT Column that contains count for events, default is `"EVCOUNT"`
#' @param timeConversion factor to multiply the time column by to convert it to another format (e.g. annual). Default is `1/(24*365.25)` because default myTIME is `"TSFDH"`
#' @param nEventColNm the character string to be printed as the column name with the number of events
#' @param fTimeColNm the character string to be printed as the column name with follow up time
#' @param EventRateColNm the character string to be printed as the column name with Event rate
#' @param digits_rate the number of significant digits for the Event rate column
#' @param digits the number of significant digits for the follow up time column
#' @param caption is the table caption. Assign NULL to this argument produce table without caption.
#' @param footnote is the text for footnote, default is "\\textsuperscript{a}Number of events\\newline\\textsuperscript{b}Total
#' observation period calculated as sum of individual observation period per row.
#' \\newline\\textsuperscript{c}Annual event rate calculated, for each row, as number of events per
#' group divided by total observation period per group."#' @param label is the label for the table used for cross-reference.
#' @param asList if \strong{TRUE} the data summary is returned as a list where
#'   different elements in the list contain summary of different stratification
#'   levels. If \strong{FALSE} the data summary is returned as a latex table. If
#'   no stratification variable is provided, this function always returns a list
#'   of two items (Number of event, Observation period, Event rate)
#' @inheritParams PhRame_makeSummaryTable
#' @return By default, the table at Latex format. Alternatively, if `asList = TRUE`, a list of data.frame tables
#'
#' @export
#' @examples
#' rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
#' rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)
#' rttedata$TRTF <- factor(paste(rttedata$TRTN, "mg"), levels = paste(sort(unique(rttedata$TRTN)), "mg"))
#'
#' #create summary as a list
#' summaryFollowUpTime(rttedata,
#'                     outerLevel   ="STUDYIDN" ,
#'                     outerLabel   = "Study",
#'                     innerLevel   = "TRTF",
#'                     innerLabel   = "Treatment",
#'                     asList = TRUE,
#'                     timeConversion = 1/24/365.25)
#'
#' #output as latex table
#' summaryFollowUpTime(rttedata,
#'                     outerLevel   ="STUDYIDN" ,
#'                     outerLabel   = "Study",
#'                     innerLevel   = "TRTF",
#'                     innerLabel   = "Treatment",
#'                     timeConversion = 1/24/365.25)

summaryFollowUpTime <- function (df,
                                 myID = "ID",
                                 myDV = "DV",
                                 myTIME = 'TSFDH',
                                 timeConversion = 1/(24*365.25),
                                 myEVCOUNT = 'EVCOUNT',
                                 outerLevel = NULL,
                                 innerLevel = NULL,
                                 outerLabel = NULL,
                                 innerLabel = NULL,
                                 digits = 0,
                                 digits_rate = 2,
                                 nEventColNm = "\\textbf{nEvent\\textsuperscript{a}}",
                                 fTimeColNm = "\\textbf{Observation period (year)\\textsuperscript{b}}",
                                 EventRateColNm = "\\textbf{Event rate (1/year)\\textsuperscript{c}}",
                                 caption = "Summary of observation periods and annual event rates in the analysis data set.",
                                 label = "tab:anaSummaryFollowupTime",
                                 footnote = "\\textsuperscript{a}Total number of events\\newline\\textsuperscript{b}
                                 The observation period (total) is the sum of the individual observation periods per group.
                                 \\newline\\textsuperscript{c}Annual event rate is calculated, per group, as the total number of events divided by the observation period (total).",
                                 asList = FALSE,
                                 ...) {

  sum_data <- df %>% group_by(!!rlang::sym(myID)) %>%
    mutate(
      lastTSFDH = dplyr::last(!!rlang::sym(myTIME)),             # Last time per subject
      finalCount = dplyr::last(!!rlang::sym(myEVCOUNT))          # How many events the subject had
    ) %>%
    slice(1)


  #Convert time to annual

  sum_data <- sum_data %>% mutate(annualT = lastTSFDH*timeConversion)
  followUpFun <- function(df){
    df %>%
      summarise(nEvent=sum(finalCount),
                TotalFollowUpTime=sum(annualT[!duplicated(!!rlang::sym(myID))]),
                AnnualizedEventRate=PhRame_out.digits(nEvent/TotalFollowUpTime, dig = digits_rate, numeric = TRUE))
  }
  if(asList & (!is.null(outerLevel) | !is.null(innerLevel))){
    res_tab <- PhRame_dataSummaryBy_all(sum_data, fn = followUpFun, grp_var = c(outerLevel, innerLevel))
  }
  if(is.null(outerLevel) & is.null(innerLevel)){
    tab_comb <- PhRame_dataSummaryBy_comb(sum_data, fn = followUpFun)
    res_tab <- list(nEvent    = as.numeric(tab_comb$nEvent),
                    TotalFollowUpTime   = as.numeric(tab_comb$TotalFollowUpTime),
                    AnnualizedEventRate = as.numeric(tab_comb$AnnualizedEventRate))
  }
  if(asList | (is.null(outerLevel) & is.null(innerLevel))) {
    return(res_tab)
  }else{
    followUpFunTable <- function(df) {
      df %>% summarise(
        subjects = sum(finalCount),
        nObs = sum(annualT[!duplicated(!!rlang::sym(myID))]),
        avnObs = subjects/nObs
      ) %>%
        mutate(
          nObs = round(nObs, digits = digits),
          avnObs = PhRame_out.digits(avnObs, dig = digits_rate)
        )
    }
    PhRame_makeSummaryTable(df = sum_data,
                            myID = myID,
                            myDV = myDV,
                            outerLevel = outerLevel,
                            innerLevel = innerLevel,
                            outerLabel = outerLabel,
                            innerLabel = innerLabel,
                            digits = digits,
                            nIdColNm = nEventColNm,
                            nObsColNm = fTimeColNm,
                            avnObsColNm = EventRateColNm,
                            caption = caption,
                            label = label,
                            footnote = footnote,
                            myFun = followUpFunTable,
                            ...
    )

  }
}
