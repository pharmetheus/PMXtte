#' Factor the RTTE variable
#'
#' @param x a vector of numeric
#'
#' @return a vector of factor
#' @export
#'
#' @examples
#' ex <- data.frame(
#'   ID = c(1,1,2,2,2),
#'   RTTE = c(1,2,1,2,3),
#'   DV = c(1,0,1,1,0),
#'   TIME = c(100, 200, 50, 150, 250)
#' )
#' dplyr::mutate(ex, RTTEF = factor_rtte(RTTE))
#' # Useful to facet Kaplan-Meier with RTTE data
#' ggKAP(
#'  data = dplyr::mutate(ex, RTTEF = factor_rtte(RTTE)),
#'  facet_var = "RTTEF"
#' )
factor_rtte <- function(x){
  if(is.null(x)||length(x)==0) return(factor())
  factor(
    x,
    levels = unique(sort(x)),
    labels = paste0("Event #",unique(sort(x)))
  )
}
