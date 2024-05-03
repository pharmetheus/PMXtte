#' Title
#'
#' @param RTTEdata dataframe for to be prepared for plots
#' @param plottype string selecting type of plot
#' @return modified dataframe ready for plots
#' @import dplyr
#' @import rlang
#' @export
#'
#' @examples
prep_dataframe <- function(RTTEdata, plottype = "time_to_first_event",exposure_col = CMINSS){
  RTTEdata <- RTTEdata %>% dplyr::filter(EVID==0&TYPE==0)

  tert.bounds  <- RTTEdata %>% distinct(ID,.keep_all = TRUE) %>%
    filter(DOSE>0) %>%
    summarise(low.tertile = quantile({{exposure_col}},1/3), mid.tertile = quantile({{exposure_col}},2/3))

  # Add tertile information to the data and arrange
  RTTEdata <- RTTEdata %>%
    mutate(exp.tertile=ifelse({{exposure_col}} <= as.numeric(tert.bounds$mid.tertile),2,3),
           exp.tertile=ifelse({{exposure_col}} <= as.numeric(tert.bounds$low.tertile),1,exp.tertile),
           exp.tertile=ifelse(DOSE == 0,0,exp.tertile),
           # Will list placebo at the bottom of the legend,
           # which aligns with the expected survival across treatment groups.
           exp.tertileF=factor(exp.tertile,
                               levels=rev(0:3),
                               labels = rev(c("Placebo","Low","Mid","High"))))

  if (plottype == 'time_to_first_event'){
  RTTEdata <- RTTEdata %>% dplyr::distinct(ID, .keep_all = TRUE)}
  # Add dose factors to the data
  DOSEL <- sort(unique(RTTEdata$DOSE),decreasing = TRUE)
  placebo <- ifelse(min(DOSEL)==0, "Placebo", paste(DOSEL, " mg"))
  RTTEdata <- RTTEdata %>%
    dplyr::mutate(DOSEF = factor(DOSE,levels = DOSEL,
                          labels = c(paste(DOSEL[1:length(DOSEL)-1]," mg"), placebo
                          )))
  return(RTTEdata)
}
