#' Create Kaplan Meier plots
#'
#' This function generates Kaplan Meier curves plot using ggsurvplot from a dataframe
#' @param data dataframe used to fit survival curves
#' @param time_col The name of the column containing the survival times.
#' @param event_col The name of the column indicating an event occurrence (1) or censoring (0)
#' @param cov_col The name of the column to stratify on, must be string (e.g. "DOSE") if (1) there will be no stratification
#' @inheritParams survminer::ggsurvplot
#' @inheritParams survminer::ggsurvplot_facet
#' @inheritDotParams survminer::ggsurvplot
#' @return Kaplan-Meier (KM) curves for the provided data coloured by treatment/dose
#' @import rlang
#' @import ggplot2
#' @importFrom survival Surv
#' @importFrom survminer ggsurvplot surv_fit ggsurvplot_facet
#' @export
#'
#' @examples
#' #load example data
#' exampledata <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv', package= 'PMXtte'),na.strings=c(".","-99","NA"))
#' #generate time to first event dataframe
#' RTTEdata <- exampledata %>% dplyr::filter(EVID==0&TYPE==0)
#' RTTEdata <- RTTEdata %>% dplyr::distinct(ID, .keep_all = TRUE)
#' #default plot with one curve
#' plotKaplanMeier(RTTEdata)
#' # Kaplan Meier plot stratified by Dose and faceted by SEX
#' plotKaplanMeier(RTTEdata, cov_col = "DOSE", facet.by = 'SEX')


plotKaplanMeier <- function(data,
                                time_col              = "TSFDW",
                                event_col             ="DV",
                                cov_col               = 1 ,
                                facet.by              = NULL,
                                panel.labs            = NULL,
                                xlab                  = "Time since first dose (week)",
                                ylab                  = "Fraction without events",
                                break.time.by         = 4,
                                xlim                  = c(0,52.1),
                                ylim                  = c(0,1),
                                surv.scale            = "percent",
                                ggtheme               = ggplot2::theme_bw(),
                                conf.int              = TRUE,
                                conf.inf.alpha        = 0.9,
                                risk.table            = TRUE,
                                risk.table.y.text     = TRUE,
                                risk.table.y.text.col = TRUE,
                                risk.table.fontsize   = 4,
                                pval                  = TRUE,
                                pval.method           = TRUE,
                                surv.median.line      ="hv",
                                legend                = "top",
                                legend.labs           = sub(pattern=paste(cov_col,'=', sep = ''),
                                                            replacement="",
                                                            x=names(fit.FirstEventByArm$strata)),
                                legend.title          = cov_col,
                                ...){
  # Check if the input is a dataframe
  if (!is.data.frame(data)){
    stop('Input is not a dataframe')
  }
  # convert time_col, event_col to strings if they are not already
  time_col  <- as.character(substitute(time_col))
  event_col <- as.character(substitute(event_col))

  # Check if the columns exist in the dataframe
  if (!all(c(time_col, event_col) %in% names(data))) {
    stop('one or more specified columns do not exist in the dataframe')
  }
  #change settings based on covariates and facets input
  if (cov_col == 1){
    legend                <- "none"
    legend.labs           <- "all"
    legend.title          <- "all"
    pval                  <- FALSE
    pval.method           <- FALSE
  }
  else {
    cov_col <- as.character(substitute(cov_col))
    if (!cov_col %in% names(data)){
      stop('covariate column does not exist in the dataframe')
    }
  }

  #fit survival curves
  formula_text        <- paste("survival::Surv(",time_col,",",event_col,")~",cov_col)
  fit.FirstEventByArm <- survminer::surv_fit(as.formula(formula_text) , data = data)

  if (!is.null(facet.by)){
    facet_plots <- survminer::ggsurvplot_facet(fit                = fit.FirstEventByArm,
                                               data               = data,
                                               facet.by           = facet.by,
                                               pval               = pval,
                                               pval.method        = pval.method,
                                               panel.labs         = panel.labs,
                                               surv.scale         = surv.scale,
                                               ggtheme            = ggtheme,
                                               conf.int           = conf.int,
                                               conf.inf.alpha     = conf.inf.alpha,
                                               surv.median.line   = surv.median.line,
                                               legend             = legend,
                                               legend.title       = legend.title,
                                               legend.labs        = legend.labs,
                                               xlab               = xlab,
                                               ylab               = ylab,
                                               break.time.by      = break.time.by,
                                               xlim               = xlim,
                                               ylim               = ylim

                                               )
    return(facet_plots)
  }

  #Plot survival curves
  FirstEventByArm <- survminer::ggsurvplot(xlab                  = xlab,
                                           ylab                  = ylab,
                                           break.time.by         = break.time.by,
                                           xlim                  = xlim,
                                           ylim                  = ylim,
                                           surv.scale            = surv.scale,
                                           ggtheme               = ggtheme,
                                           conf.int              = conf.int,
                                           conf.inf.alpha        = conf.inf.alpha,
                                           risk.table            = risk.table,
                                           risk.table.y.text     = risk.table.y.text,
                                           risk.table.y.text.col = risk.table.y.text.col,
                                           pval                  = pval,
                                           pval.method           = pval.method,
                                           surv.median.line      = surv.median.line,
                                           legend                = legend,
                                           fit                   = fit.FirstEventByArm,
                                           legend.title          = legend.title,
                                           legend.labs           = legend.labs,
                                           risk.table.fontsize   = risk.table.fontsize,
                                           #font.x               = 25,
                                           #font.y        = 25,
                                           #font.tickslab = 16,
                                           #font.legend   = 20,
                                           #cumevents = TRUE,
                                           #cumcensor=TRUE,
                                           #linetype = "DOSEF",
                                           #tables.height = 0.15,
                                           #short.panel.labs=TRUE,
                                           #axes.offset   = TRUE, #default is TRUE
                                           #censor           = TRUE, #default is true
                                           #ncol            = 2,
                                           #palette       = regCols,
                                           #risk.table.title  = "Hej Siv",
                                           #data          = data
                                           #distinct(ID, .keep_all = TRUE),

  )


  return(FirstEventByArm)
}
