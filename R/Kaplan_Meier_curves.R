#' Title
#'
#' @param RTTEdata dataframe for analysis
#' @param time_col The name of the column containing the survival times.
#' @param event_col The name of the column indicating an event occurrence (1) or censoring (0)
#' @param ... Additional arguments to be passed to ggsurvplot (e.g., xlim, title, etc.).
#' @return Kaplan-Meier (KM) curves for the provided data coloured by treatment/dose
#' @import rlang
#' @importFrom survival Surv
#' @importFrom survminer ggsurvplot surv_fit
#' @export
#'
#' @examples
Kaplan_Meier_curves <- function(data, time_col = "TSFDW", event_col="DV", cov_col= "DOSEF",...){
  # Check if the input is a dataframe
  if (!is.data.frame(data)){
    stop('Input is not a dataframe')
  }
  # convert time_col, event_col, cov_col to strings if they are not already
  time_col <- as.character(substitute(time_col))
  event_col <- as.character(substitute(event_col))
  cov_col <- as.character(substitute(cov_col))

  data <- prep_dataframe(data)

  # Check if the columns exist in the dataframe
  if (!all(c(time_col, event_col, cov_col) %in% names(data))) {
    stop('one or more specified columns do not exist in the dataframe')
  }

  #fit survival curves
  fit.FirstEventByArm <- survminer::surv_fit(survival::Surv(time = data[[time_col]],
                                                            event = data[[event_col]]) ~
                                               data[[cov_col]] , data = data)
  #set default plot values
  default_args <- list(xlab          = "Time since first dose (week)",
                       ylab          = "Fraction without events",
                       #font.x        = 25,
                       #font.y        = 25,
                       #font.tickslab = 16,
                       #font.legend   = 20,
                       #cumevents = TRUE,
                       #cumcensor=TRUE,
                       #linetype = "DOSEF",
                       break.time.by = 4,
                       xlim          = c(0,52.1),
                       ylim          = c(0,1),
                       axes.offset   = TRUE,
                       surv.scale    = "percent",
                       #tables.height = 0.15,
                       #facet.by        = c("PROJF"),
                       #short.panel.labs=TRUE,
                       ggtheme         = ggplot2::theme_bw(),
                       censor           = TRUE,
                       #ncol            = 2,
                       conf.int      = TRUE,
                       conf.inf.alpha = 0.9,
                       #palette       = regCols,
                       risk.table    = TRUE,
                       #risk.table.title  = "Hej Siv",
                       risk.table.y.text = TRUE,
                       risk.table.y.text.col = TRUE,
                       pval         = TRUE,
                       pval.method  = TRUE,
                       surv.median.line ="hv",
                       legend        = c(0.25, 0.25),
                       fit=fit.FirstEventByArm,
                       #data          = data ,
                       #distinct(ID, .keep_all = TRUE),
                       legend.title  = cov_col,
                       legend.labs   = sub(pattern="data\\[\\[cov_col\\]\\]=",
                                           replacement="",
                                           x=names(fit.FirstEventByArm$strata))
  )

  #merge and override default and user arguments
  args <- modifyList(default_args, list(...), keep.null = TRUE)

  #Plot survival curves
  FirstEventByArm <- do.call(survminer::ggsurvplot, args)

  return(FirstEventByArm)
}
