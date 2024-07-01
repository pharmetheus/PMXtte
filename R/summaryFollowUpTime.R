#' Title
#'
#' @param time Column used to calculate annual time
#' @param count Column that contains count for events
#' @param timeConversion factor to multiply the time column by to convert it to another format(e.g. annual)
#' @param nEventColNm the charactrer string to be printed as the column name with the number of events
#' @param fTimeColNm the charactrer string to be printed as the column name with follow up time
#' @param EventRateColNm the charactrer string to be printed as the column name with Event rate
#' @inheritDotParams PhRame::makeSummaryTable
#' @inheritParams PhRame::makeSummaryTable
#' @return
#' @export
#' @import PhRame
#' @examples
#' ttedata  <- read_csv(system.file('extdata/anaDATAf.csv', package= 'PMXtte'))
#'
#' #create summary as a list
#' summaryFollowUpTime(ttedata,
#'                     outerLevel   = "DOSEN",
#'                     outerLabel   = "Dose",
#'                     innerLevel   = "STUDYIDN",
#'                     innerLabel   = "Study",
#'                     asList = TRUE,
#'                     timeConversion = 1/24/365.25)
#'
#' #output as latex table
#' summaryFollowUpTime(ttedata,
#'                     outerLevel   = "DOSEN",
#'                     outerLabel   = "Dose",
#'                     innerLevel   = "STUDYIDN",
#'                     innerLabel   = "Study",
#'                     timeConversion = 1/24/365.25)

summaryFollowUpTime <- function (df,
                                 myID = "ID",
                                 myDV = "DV",
                                 time = 'TSFDH',
                                 timeConversion = 1,
                                 count = 'EVCOUNT',
                                 outerLevel = NULL,
                                 innerLevel = NULL,
                                 outerLabel = NULL,
                                 innerLabel = NULL,
                                 digits = 3,
                                 digits_rate = 2,
                                 nEventColNm = "\\textbf{nEvent\\textsuperscript{a}}",
                                 fTimeColNm = "\\textbf{TotalFollowUpTime\\textsuperscript{b}}",
                                 EventRateColNm = "\\textbf{AnnualizedEventRate/nID\\textsuperscript{c}}",
                                 caption = "Annual Follow Up time",
                                 label = "tab:anaSummary",
                                 footnote = "\\textsuperscript{a}Number of events\\newline\\textsuperscript{b}Sum of individual follow-up times per group.\\newline\\textsuperscript{c}Annual event rate",
                                 asList = FALSE,
                                 ...) {

  sum_data <- df %>% group_by(!!rlang::sym(myID)) %>%
    mutate(lastTSFDH = dplyr::last(!!rlang::sym(time)),             # Last time per subject
           finalCount = dplyr::last(!!rlang::sym(count))          # How many events the subject had
           )


  #Convert time to annual

   sum_data <- sum_data %>% mutate(annualT = lastTSFDH*timeConversion)
  followUpFun <- function(df){
    df %>%
      summarise(nEvent=sum(finalCount),
                TotalFollowUpTime=sum(annualT[!duplicated(!!rlang::sym(myID))]),
                AnnualizedEventRate=out.digits(nEvent/TotalFollowUpTime, dig = digits, numeric = TRUE))
  }
  if(asList & (!is.null(outerLevel) | !is.null(innerLevel))){
      res_tab <- PhRame:::dataSummaryBy_all(sum_data, fn = followUpFun, grp_var = c(outerLevel, innerLevel))
    }
  if(is.null(outerLevel) & is.null(innerLevel)){
      tab_comb <- PhRame:::dataSummaryBy_comb(sum_data, fn = followUpFun)
      res_tab <- list(nEvent    = as.numeric(tab_comb$nEvent),
                      TotalFollowUpTime   = as.numeric(tab_comb$TotalFollowUpTime),
                      AnnualizedEventRate = as.numeric(tab_comb$AnnualizedEventRate))
  }
  if(asList | (is.null(outerLevel) & is.null(innerLevel))) {
    return(res_tab)
  }else{
    followUpFunTable <- function(df){
      df %>%
        summarise(subjects=sum(finalCount),
                  nObs=round(sum(annualT[!duplicated(!!rlang::sym(myID))]), digits = digits),
                  avnObs=out.digits(subjects/nObs, dig = digits_rate))
    }
  PhRame::makeSummaryTable(df = sum_data,
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
