#' Create Kaplan Meier plots
#'
#' This function generates Kaplan Meier curves plot using ggsurvplot from a dataframe
#' @param data dataframe used to fit survival curves
#' @param time_col The name of the column containing the survival times.
#' @param event_col The name of the column indicating an event occurrence (1) or censoring (0)
#' @param cov_col The name of the column to stratify on, must be string (e.g. "DOSE") if NULL there will be no stratification
#' @param label.parsed if TRUE (default), facet labels can include superscipts and greek letters
#' @param ciWidth confidence intervals to be passed in surv_fit default is 0.95
#' @param ggtheme function, ggplot2 theme name. Default value is theme_pmx. Allowed values include ggplot2 official themes.
#' @param risk.table.fontsize size of the font for survival table in points
#' @param risk.table Allowed values include: \itemize{ \item TRUE or FALSE
#'  specifying whether to show or not the risk table. Default is TRUE \item
#'  "absolute" or "percentage". Shows the \bold{absolute number} and the
#'  \bold{percentage} of subjects at risk by time, respectively. \item "abs_pct"
#'  to show both absolute number and percentage. \item "nrisk_cumcensor" and
#'  "nrisk_cumevents". Show the number at risk and, the cumulative number of
#'  censoring and events, respectively. }
#' @param pval.size size of font for the pval in points
#' @param pval logical value, a numeric or a string. If logical and TRUE, the p-value is added on the plot. If numeric, then the computed p-value is substituted with the one passed with this parameter. If character, then the customized string appears on the plot.
#' @param facet.by a character vector containing the name of grouping variables to facet the survival curves into multiple panels. Should be of length <= 2. Alias of the ggsurvplot_facet() function. the function does not facet tables therefor only the plots will be faceted and tables will not be produced.
#' @inheritParams survminer::ggsurvplot
#' @inheritParams survminer::ggsurvplot_facet
#' @inheritParams survminer::ggsurvtable
#' @inheritParams ggpubr::ggpar
#' @inheritDotParams survminer::ggsurvplot
#' @param surv.median.line.legend Text to be used for median survival line in the legend
#' @param legend.nrow The number if rows in the legend for the stratification variable
#' @param legend.ncol The number if columns in the legend for the stratification variable
#' @details The function takes a dataframe (tibble will be coerced to data.frame) and fits the survival curves using surv_fit then plots them using ggsurvplot.
#' The arguments risk.table.fontsize and pval.size convert regular sizes to ggplot sizes. For other font size arguments passed to ggsurvplot you can multiply regular point sizes by 0.36 to convert to ggplot sizes. To change the color of the curves use the argument palette instead of color.
#' @return A "ggsurvplot" object, see the documentation of `survminer::ggsurvplot` for more information. Basically it is a list that notably contains two "ggplot" objects, "x$plot" and "x$table", that renders into a single figure the Kaplan-Meier curves on top and the risk table on the bottom.

#'
#' @examples
#' library(magrittr)
#' #load example data
#' rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
#' rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)
#'
#' #generate time to first event data
#' tte1data <- rttedata %>% filter_xth_event(1)
#'
#' #transform back to "data.frame" not "tibble" do avoid bugs
#' tte1data <- as.data.frame(tte1data)
#'
#' #default plot with one curve
#' plotKaplanMeier(tte1data)
#'
#' # Kaplan Meier plot stratified by Dose and faceted by SEX
#' plotKaplanMeier(tte1data, cov_col = "DOSEN", facet.by = 'SEXN')
#'
#' # Change the panel labs of the facets
#' plotKaplanMeier(tte1data, cov_col = 'DOSEN', facet.by = 'SEXN', panel.labs = list('SEXN' = c('male','female')))
#'
#' # Change the size of pval text
#' plotKaplanMeier(tte1data, cov_col = "DOSEN", pval = TRUE, pval.size = 12)
#'
#' # Change the starting color in the palette
#' plotKaplanMeier(tte1data, cov_col = "DOSEN", palette = PMXtte:::PMXColors_pmx_palettes(firstColorNum = 3))
#'
#' # Change the color for a single curve plot
#' plotKaplanMeier(tte1data, palette = 'cadetblue')
#'
#' # Create custom palette of colors
#' Colors <- c('cadetblue', 'coral1', 'darkorchid', 'deepskyblue3', 'darkred')
#' plotKaplanMeier(tte1data, palette = Colors, cov_col = 'DOSEN')
#'
#' #modify plot using ggplot syntax
#' p <- plotKaplanMeier(tte1data, cov_col = "DOSEN")
#' p$table <- p$table + ggplot2::ylab('Dose (mg)')
#' p
#'
#' # remove median line from legend
#' plotKaplanMeier(tte1data, cov_col = "DOSEN", surv.median.line.legend = FALSE)
#'
#' # Time to second event data
#' # !! NOT ADVISED since the subjects at risk are not at random across groups!!
#' rttedata %>%
#'   filter_xth_event(2) %>%
#'   plotKaplanMeier( # KPM plot of time to second event data
#'     cov_col = "DOSEN",
#'     time_col = "TSLE", #x-axis: time since Last event (not first dose),
#'     xlab = "Time since last event (week)" #in week because `time_col="TSFDW"`
#'   )
#'
#' @export



plotKaplanMeier <- function(data,
                            time_col                = "TSFDW",
                            event_col               ="DV",
                            cov_col                 = NULL ,
                            facet.by                = NULL,
                            panel.labs              = NULL,
                            short.panel.labs        = TRUE,
                            label.parsed            = TRUE,
                            xlab                    = "Time since first dose (weeks)",
                            ylab                    = "Subjects without events (%)",
                            break.time.by           = 4,
                            xlim                    = NULL,
                            ylim                    = c(0,1),
                            surv.scale              = "percent",
                            ggtheme                 = PMXColors_theme_pmx(),
                            palette                 = PMXColors_pmx_palettes(),
                            conf.int                = TRUE,
                            ciWidth                 = 0.95,
                            add.ciWidth.to.legend   = TRUE,
                            conf.int.alpha          = 0.3,
                            conf.int.fill           = 'gray',
                            risk.table              = TRUE,
                            risk.table.y.text       = TRUE,
                            risk.table.y.text.col   = TRUE,
                            risk.table.fontsize     = 8,
                            table.clip              = 'off',
                            pval                    = FALSE,
                            pval.method             = TRUE,
                            pval.size               = 10,
                            surv.median.line        ="hv",
                            surv.median.line.legend = TRUE,
                            surv.median.line.title  = 'Median Survival',
                            legend                  = "top",
                            legend.title            = 'Observed',
                            legend.nrow             = NULL,
                            legend.ncol             = NULL,
                            risk.table.ylab         = cov_col,
                            risk.table.title        ="Number of subjects at risk",
                            legend.labs             = sub(pattern=paste(cov_col,'=', sep = ''),
                                                          replacement="",
                                                          x=names(fit$strata)),
                            nrow                    = NULL,
                            ncol                    = NULL,
                            scales                  = "fixed",
                            legend.box              = 'horizontal',
                            legend.title.size       = 9,
                            legend.spacing           = 5,
                            censor.shape = '|',
                            censor.size = 4,
                            ...){
  # Check if the input is a dataframe
  if (!is.data.frame(data)){
    stop('Input is not a dataframe')
  }

  if(inherits(data, "tbl")){
    data <- as.data.frame(data)
  }

  # convert time_col, event_col to strings if they are not already
  time_col  <- as.character(time_col)
  event_col <- as.character(event_col)

  # Check if the columns exist in the dataframe
  if (!all(c(time_col, event_col) %in% names(data))) {
    stop('one or more specified columns do not exist in the dataframe')
  }


  if (!is.null(cov_col)) {
    cov_col <- as.character(cov_col)
    if (!cov_col %in% names(data)){
      stop('covariate column does not exist in the dataframe')
    }
  }
  else {
    cov_col <- 1
    pval                  <- FALSE
    pval.method           <- FALSE
    conf.int.fill         <- 'strata'

    # if (surv.curve.legend == 'Survival Curves'){
    #   surv.curve.legend <- 'Survival Curve'
    # }
    # if (conf.int.legend == 'Confidence intervals'){
    #   conf.int.legend <- 'Confidence interval'
    # }
    # if (legend.title == 'Survival curves'){
    #   legend.title <- 'Survival curve'
    # }
  }

  #fit survival curves
  formula_text        <- paste("survival::Surv(",time_col,",",event_col,")~",cov_col)
  fit <- survminer::surv_fit(stats::as.formula(formula_text) , data = data , conf.int = ciWidth)

  # title for confidence interval legend
  if (add.ciWidth.to.legend){
    legend.title <- paste0(legend.title,' and ', ciWidth*100, '% CI')
  }
  if (!is.null(facet.by)){
    facetPlots <- survminer::ggsurvplot_facet(fit                    = fit,
                                              data                   = data,
                                              facet.by               = facet.by,
                                              pval                   = pval,
                                              short.panel.labs       = short.panel.labs,
                                              pval.method            = pval.method,
                                              panel.labs             = panel.labs,
                                              surv.scale             = surv.scale,
                                              ggtheme                = ggtheme,
                                              conf.int               = conf.int,
                                              conf.int.alpha         = conf.int.alpha,
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
                                              censor.shape = censor.shape,
                                              ...
    )
    if (surv.median.line %in% c('hv', 'v', 'h')){
      if(!is.null(fit$strata) | is.matrix(fit$surv)){
        .table <- as.data.frame(summary(fit)$table)
      }
      else{
        .table <- t(as.data.frame(summary(fit)$table))
        rownames(.table) <- "All"
      }

      surv_median <- as.vector(.table[, "median"])
      df <- data.frame(x1 = surv_median, x2 = surv_median,
                       y1 = rep(0, length(surv_median)),
                       y2 = rep(0.5, length(surv_median))
      )
      df <- stats::na.omit(df)
      if (nrow(df)>0 & surv.median.line.legend){
      facetPlots <- facetPlots +
        geom_line(aes(x = 0 , y= 0,linetype = 'Median'), show.legend = TRUE) +
        scale_linetype_manual(name = '',
                              values = c("Median" = "dashed"),
                              label = surv.median.line.title
        )
      }}

    # Add a blank geom_point to trigger the Censored entry
    facetPlots <- facetPlots +
      geom_point(x=NA,y=NA,aes(shape= factor(1))) +
      scale_shape_manual(name = '',
                         values = censor.shape,
                         label = "Censored"
      )


    facetPlots <- facetPlots +
      guides(color= guide_legend(order=1,
                                 nrow = legend.nrow,
                                 ncol = legend.ncol
      ),
      fill = guide_legend(order=1,
                          nrow = legend.nrow,
                          ncol = legend.ncol,
                          override.aes=list(shape=NA)
      ) ,
      linetype = guide_legend(order = 3),
      shape = guide_legend(order=3,
                           override.aes=list(shape=censor.shape,
                                             linetype=NA,
                                             size=censor.size)))


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
        facetPlots <- facetPlots + theme(legend.box = legend.box,
                                         legend.title = element_text(size= legend.title.size),
                                         legend.spacing = unit(legend.spacing, 'pt'))
    return(facetPlots)
  }

  #change settings for legend in single curve plots
  if (cov_col == 1){
    if (length(legend.labs) != 1) {
      legend.labs <- ''
    }
    # if (legend.title == 1){
    #   legend.title = 'All'
    #}
    if(risk.table.ylab ==1){
      risk.table.ylab <- 'All'
    }
  } else {
    if (length(unique(data[[cov_col]])) ==1) {
      if (length(legend.labs) != 1) {
        legend.labs <- unique(data[[cov_col]])
      }
      conf.int.fill <- 'strata'
    }
  }


  #Plot survival curves
  kaplanPlot <- survminer::ggsurvplot(xlab                  = xlab,
                                           ylab                  = ylab,
                                           break.time.by         = break.time.by,
                                           xlim                  = xlim,
                                           ylim                  = ylim,
                                           surv.scale            = surv.scale,
                                           ggtheme               = ggtheme,
                                           conf.int              = conf.int,
                                           conf.int.alpha        = conf.int.alpha,
                                           risk.table            = risk.table,
                                           risk.table.y.text     = risk.table.y.text,
                                           risk.table.y.text.col = risk.table.y.text.col,
                                           pval                  = pval,
                                           pval.method           = pval.method,
                                           surv.median.line      = surv.median.line,
                                           legend                = legend,
                                           fit                   = fit,
                                           legend.title          = legend.title,
                                           legend.labs           = legend.labs,
                                           risk.table.fontsize   = risk.table.fontsize * 0.36,
                                           palette               = palette,
                                           pval.size             = pval.size * 0.36,
                                          conf.int.fill          = conf.int.fill,
                                      censor.shape = censor.shape,
                                      risk.table.title = risk.table.title,
                                           ...
                                           #font.x               = 25,
                                           #font.y        = 25,
                                           #font.tickslab = 16,
                                           #font.legend   = 20,
                                           #cumevents = TRUE,
                                           #cumcensor=TRUE,
                                           #linetype = "DOSENF",
                                           #tables.height = 0.15,
                                           #short.panel.labs=TRUE,
                                           #axes.offset   = TRUE, #default is TRUE
                                           #censor           = TRUE, #default is true
                                           #pval.size
                                           #pval.method.size
                                           #pval.coord

  )

  if (surv.median.line %in% c('hv', 'v', 'h')){
    if(!is.null(fit$strata) | is.matrix(fit$surv)){
      .table <- as.data.frame(summary(fit)$table)
    }
    else{
      .table <- t(as.data.frame(summary(fit)$table))
      rownames(.table) <- "All"
    }

    surv_median <- as.vector(.table[, "median"])
    df <- data.frame(x1 = surv_median, x2 = surv_median,
                     y1 = rep(0, length(surv_median)),
                     y2 = rep(0.5, length(surv_median))
                     )
    df <- stats::na.omit(df)
    if (nrow(df)>0 & surv.median.line.legend){

    kaplanPlot$plot <- kaplanPlot$plot +
      geom_line(aes(x = 0 , y= 0,linetype = 'Median'),data= data, show.legend = TRUE) +
      scale_linetype_manual(name = '',
                            values = c("Median" = "dashed"),
                            label = surv.median.line.title,
      )
    }

    # Add a blank geom_point to trigger the Censored entry
    kaplanPlot$plot <- kaplanPlot$plot +
      geom_point(x=NA,y=NA,aes(shape= factor(1))) +
      scale_shape_manual(name = '',
                         values = censor.shape,
                         label = "Censored"
      )

  }

  kaplanPlot$plot <- kaplanPlot$plot +
    guides(color= guide_legend(order=1,
                               nrow = legend.nrow,
                               ncol = legend.ncol
                      ),
             fill = guide_legend(order=1,
                                 nrow = legend.nrow,
                                 ncol = legend.ncol,
                                 override.aes = list(shape=NA)
                               ) ,
           linetype = guide_legend(order = 3),
           shape = guide_legend(order=3,override.aes=list(shape=censor.shape,
                                                          linetype=NA,
                                                          size=censor.size))) +
    theme(legend.box = legend.box,
          legend.title = element_text(size= legend.title.size),
          legend.spacing = unit(legend.spacing, 'pt'))

  # table specifications
if(!is.null(kaplanPlot$table)) {
  kaplanPlot$table <- kaplanPlot$table +
                      theme(panel.grid = element_blank()) +
                      ylab(label = risk.table.ylab)
  if(table.clip == 'off'){
    # unclear to me (FLL) but we need this "if" and we cannot
    # use coord_cartesian(default = TRUE) to suppress the message
    kaplanPlot$table <- suppressMessages(kaplanPlot$table +
                         coord_cartesian(xlim = xlim, clip = 'off'))
  }

}
  return(kaplanPlot)
}
