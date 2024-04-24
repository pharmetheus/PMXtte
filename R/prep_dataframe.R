#' Title
#'
#' @param RTTEdata
#'
#' @return modified dataframe ready for plots
#' @import dplyr
#' @export
#'
#' @examples
prep_dataframe <- function(RTTEdata){
  RTTEdata <- RTTEdata %>% dplyr::filter(EVID==0&TYPE==0)
  RTTEdata <- RTTEdata %>% dplyr::distinct(ID, .keep_all = TRUE)
  # Add dose factors to the data (we arrange it to make the plots look nice and in order)
  RTTEdata <- RTTEdata %>%
    dplyr::mutate(DOSEF = factor(DOSE,levels = sort(unique(RTTEdata$DOSE),decreasing = TRUE),
                                 labels = c("400 mg","200 mg", "100 mg", "Placebo")))
  return(RTTEdata)
}
