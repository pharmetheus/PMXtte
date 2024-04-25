#' Title
#'
#' @param RTTEdata dataframe for analysis
#'
#' @return Kaplan-Meier (KM) curves for the provided data coloured by treatment/dose
#' @import rlang
#' @importFrom survival Surv
#' @importFrom survminer ggsurvplot surv_fit
#' @export
#'
#' @examples
Kaplan_Meier_curves <- function(data, time_col = "TSFDW", status_col="DV", cov_col= "DOSEF"){
  fit.FirstEventByArm <- survminer::surv_fit(survival::Surv(time = data[[time_col]],
                                                          event = data[[status_col]]) ~
                                             data[[cov_col]], data = data)
  FirstEventByArm <- survminer::ggsurvplot(fit=fit.FirstEventByArm,
                                #data          = data ,
                                #distinct(ID, .keep_all = TRUE),
                                xlab          = "Time since first dose (week)",
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
                                legend.title  = "Dose",
                                legend.labs   = sub(pattern="DOSEF=",
                                                    replacement="",
                                                    x=names(fit.FirstEventByArm$strata))

  )
  return(FirstEventByArm)
}
