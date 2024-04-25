#' Title
#'
#' @param RTTEdata dataframe for to be prepared for plots
#' @param plottype string selecting type of plot
#' @return modified dataframe ready for plots
#' @import dplyr
#' @export
#'
#' @examples
prep_dataframe <- function(RTTEdata, plottype = "time_to_first_event"){
  RTTEdata <- RTTEdata %>% dplyr::filter(EVID==0&TYPE==0)
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
