#' Create Kaplan Meier plots
#'
#' This function generates Kaplan Meier curves plot using ggsurvplot from a dataframe
#' @param data dataframe used to fit survival curves
#' @param time_col The name of the column containing the survival times.
#' @param event_col The name of the column indicating an event occurrence (1) or censoring (0)
#' @param cov_col The name of the column to stratify on, must be string (e.g. "DOSE") if (1) there will be no stratification
#' @param label.parsed if TRUE (default), facet labels can include superscipts and greek letters
#' @param ciWidth confidence intervals to be passed in surv_fit default is 0.95
#' @param ggtheme function, ggplot2 theme name. Default value is theme_pmx. Allowed values include ggplot2 official themes.
#' @param risk.table.fontsize size of the font for survival table in points
#' @param pval.size size of font for the pval in points
#' @param facet.by a character vector containing the name of grouping variables to facet the survival curves into multiple panels. Should be of length <= 2. Alias of the ggsurvplot_facet() function. the function does not facet tables therefor only the plots will be faceted and tables will not be produced.
#' @inheritParams survminer::ggsurvplot
#' @inheritParams survminer::ggsurvplot_facet
#' @inheritDotParams survminer::ggsurvplot
#' @param surv.median.line.legend Text to be used for median survival line in the legend
#' @details The function takes a dataframe and fits the survival curves using surv_fit then plots them using ggsurvplot.
#' The arguments risk.table.fontsize and pval.size convert regular sizes to ggplot sizes. for other font size arguments passed to ggsurvplot you can multiply regular point sizes by 0.36 to convert to ggplot sizes.
#' @return Kaplan-Meier (KM) curves for the provided data coloured by treatment/dose
#' @import rlang
#' @import ggplot2
#' @importFrom PMXColors theme_pmx pmx_palettes
#' @importFrom survival Surv
#' @importFrom survminer ggsurvplot surv_fit ggsurvplot_facet
#'
#' @examples
#' #load example data
#' exampledata <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv', package= 'PMXtte'),na.strings=c(".","-99","NA"))
#'
#' #generate time to first event dataframe
#' RTTEdata <- exampledata %>% dplyr::filter(EVID==0&TYPE==0)
#' RTTEdata <- RTTEdata %>% dplyr::distinct(ID, .keep_all = TRUE)
#'
#' #default plot with one curve
#'  plotKaplanMeier(RTTEdata)
#'
#' # Kaplan Meier plot stratified by Dose and faceted by SEX
#'  plotKaplanMeier(RTTEdata, cov_col = "DOSE", facet.by = 'SEX')
#'
#' # Change the panel labs of the facets
#'  plotKaplanMeier(RTTEdata, cov_col = 'DOSE', facet.by = 'SEX', panel.labs = list('SEX' = c('male','female')))
#'
#' # Change the size of pval text
#'  plotKaplanMeier(RTTEdata, cov_col = "DOSE", pval.size = 12)
#'
#' # Change the starting color in the palette
#'  plotKaplanMeier(RTTEdata, cov_col = "DOSE", palette = PMXColors::pmx_palettes(firstColorNum = 3))
#' @export



plotKaplanMeier <- function(data,
                            time_col                = "TSFDW",
                            event_col               ="DV",
                            cov_col                 = 1 ,
                            facet.by                = NULL,
                            panel.labs              = NULL,
                            short.panel.labs        = TRUE,
                            label.parsed            = TRUE,
                            xlab                    = "Time since first dose (week)",
                            ylab                    = "Fraction without events",
                            break.time.by           = 4,
                            xlim                    = c(0,52.1),
                            ylim                    = c(0,1),
                            surv.scale              = "percent",
                            ggtheme                 = PMXColors::theme_pmx(),
                            palette                 = PMXColors::pmx_palettes(),
                            conf.int                = TRUE,
                            ciWidth                 = 0.95,
                            add.ciWidth.to.legend   = TRUE,
                            conf.inf.alpha          = 0.9,
                            risk.table              = TRUE,
                            risk.table.y.text       = TRUE,
                            risk.table.y.text.col   = TRUE,
                            risk.table.fontsize     = 10,
                            pval                    = TRUE,
                            pval.method             = TRUE,
                            pval.size               = 10,
                            surv.median.line        ="hv",
                            surv.median.line.legend = 'Median Survival',
                            legend                  = "top",
                            legend.title            = cov_col,
                            conf.int.legend         = 'Confidence intervals',
                            surv.curve.legend       = 'Survival Curves',
                            legend.labs             = sub(pattern=paste(cov_col,'=', sep = ''),
                                                          replacement="",
                                                          x=names(fit.FirstEventByArm$strata)),
                            nrow = NULL,
                            ncol = NULL,
                            scales = "fixed",

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
  fit.FirstEventByArm <- survminer::surv_fit(as.formula(formula_text) , data = data , conf.int = ciWidth)

  # title for confidence interval legend
  if (add.ciWidth.to.legend){
    conf.int.legend <- paste0(ciWidth*100, '% ',conf.int.legend)
  }

  if (!is.null(facet.by)){
    facetPlots <- survminer::ggsurvplot_facet(fit                = fit.FirstEventByArm,
                                              data                   = data,
                                              facet.by               = facet.by,
                                              pval                   = pval,
                                              short.panel.labs       = short.panel.labs,
                                              pval.method            = pval.method,
                                              panel.labs             = panel.labs,
                                              surv.scale             = surv.scale,
                                              ggtheme                = ggtheme,
                                              conf.int               = conf.int,
                                              conf.inf.alpha         = conf.inf.alpha,
                                              surv.median.line       = surv.median.line,
                                              legend                 = legend,
                                              legend.title           = legend.title,
                                              legend.labs            = legend.labs,
                                              xlab                   = xlab,
                                              ylab                   = ylab,
                                              break.time.by          = break.time.by,
                                              xlim                   = xlim,
                                              ylim                   = ylim,
                                              palette                = palette,
                                              pval.size              = pval.size * 0.36,
                                              ...
    )
    if (surv.median.line %in% c('hv', 'v', 'h')){
      facetPlots <- facetPlots +
        geom_line(aes(x = 0 , y= 0,linetype = 'Median'), show.legend = TRUE) +
        scale_linetype_manual(name = '',
                              values = c("Median" = "dashed"),
                              label = surv.median.line.legend
        )}

        facetPlots <- facetPlots +
          guides(color= guide_legend(order=1, title = surv.curve.legend),
                 fill = guide_legend(order=2, title = conf.int.legend, override.aes = list(color = NA)) ,
                 linetype = guide_legend(order = 3))


    if (label.parsed == TRUE) {
      if(length(facet.by) == 1){
        facet_formula <- paste0("~", facet.by) %>% stats::as.formula()
        facetPlots <- facetPlots + facet_wrap(facet_formula,
                                              scales = scales,
                                              nrow = nrow,
                                              ncol = ncol,
                                              labeller = label_parsed)
      }
      else if(length(facet.by) == 2){
        facet_formula <- paste(facet.by, collapse = " ~ ") %>% stats::as.formula()
        facetPlots <- facetPlots + facet_grid(facet_formula, scales = scales, labeller = label_parsed)
      }}

    return(facetPlots)
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
                                           risk.table.fontsize   = risk.table.fontsize * 0.36,
                                           palette               = palette,
                                           pval.size = pval.size * 0.36,
                                           ...
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

                                           #pval.size
                                           #pval.method.size
                                           #pval.coord

  )

  if (surv.median.line %in% c('hv', 'v', 'h')){
    FirstEventByArm$plot <- FirstEventByArm$plot +
      geom_line(aes(x = 0 , y= 0,linetype = 'Median'),data= data, show.legend = TRUE) +
      scale_linetype_manual(name = '',
                            values = c("Median" = "dashed"),
                            label = surv.median.line.legend
      )


  }

  FirstEventByArm$plot <- FirstEventByArm$plot +
    guides(color= guide_legend(order=1, title = surv.curve.legend),
           fill = guide_legend(order=2, title = conf.int.legend, override.aes = list(color = NA)) ,
           linetype = guide_legend(order = 3))
  return(FirstEventByArm)
}
