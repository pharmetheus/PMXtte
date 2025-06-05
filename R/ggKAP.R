#' Visualize Survival Data
#'
#' Single function for graphical exploration of survival data,
#' model evaluation of time-to-event model (VPC) and visualization of simulated data.
#' If only observed data (`data`) is supplied, the Kaplan-Meier curve, censoring events, median survival time,
#' risk table and parametric confidence interval are
#' automatically reported, as well as log rank test p value if data is stratified by coloring groups (`color_var`).
#' If both observed and simulated data (`sdata`) are supplied, censoring events, risk table and
#' simulation-based confidence interval are reported.
#' If only simulated data is supplied, the simulation-based prediction interval is reported.
#' Choose to show/hide any of these elements with the `show_*` arguments.
#'
#' Data can be grouped/colored with the `color_var` argument, and faceted with the `facet_var` argument. Preferably, these should refer to factor variables.
#'
#' Arrange the relative heights between the legends, the figure and the risk table with the `rel_heights` argument.
#'
#' @param data a data.frame, observed data, assuming one time-to-event record per individual.
#' @param sdata a data.frame, simulated data, assuming one time-to-event record per individual and iteration.
#' @param time_var a character, the name of the time column in `data` and `sdata`. The variable should be numeric, ideally > 0.
#' @param dv_var a character, the name of the dependent variable column in `data` and `sdata`. The variable should be numeric, either 0 or 1.
#' @param iter_var a character, the name of the iteration column in `sdata`. The variable should be numeric, ideally a sequence of integers.
#' @param color_var a character, the name of the column in `data` and `sdata` used to stratify by color. The variable should preferably be a factor, possibly a character or a logical, but not a double or integer.
#' @param facet_var a character, the name of the column in `data` and `sdata` used to facet the plot (Kaplan-Meier figure and risk tables) . The variable should preferably be a factor, possibly a character or a logical, but not a double or integer.
#' @param show_kpm a logical, should the Kaplan-Meier curve (and associated legend) appear on the plot? Default is `TRUE` if `data` is provided.
#' @param show_censor a logical, should the censored events (and associated legend) appear on the plot? Default is `TRUE` if `data` is provided.
#' @param show_risktable a logical, should the table with number of patient at risk appear on the plot? Default is `TRUE` if `data` is provided.
#' @param show_se a logical, should a parametric confidence interval (and associated legend) around the Kaplan-Meier curve (computed from observed data) appear on the plot? Default is `TRUE` if `data` alone is provided, `FALSE` if `sdata` is also provided.
#' @param show_pval a logical, should the p-value of a log-rank test appear on the plot? Default is `TRUE` if `data` alone is provided, and there are several Kaplan-Meier curves in each panel of the plot.
#' @param show_median a logical, should the median survival (and associated legend) appear on the plot? Default is `TRUE` if `data` alone is provided, `FALSE` if `sdata` is also provided.
#' @param show_sim a logical, should a confidence interval (and associated legend) computed from simulated data (`sdata`) appear on the plot? Default is `TRUE` if `sdata` is provided.
#' @param ci_level a numeric, between 0 and 1, indicating the level of the confidence interval. Default is `.95` (95%) if the confidence interval is calculated from observed data (`data`), `.90` (90%) if calculated from simulations (`sdata`).
#' @param cuminc a logical, should the plot represent the cumulative incidence? If `FALSE`, the default, the survival is represented.
#' @param ci_alpha a numeric, between 0 and 1, indicating the opacity of the confidence interval.
#' @param censor_shape shape of the censoring event.
#' @param censor_size a numeric, the size of the censoring event.
#' @param median_linetype a numeric, linetype of the median survival line.
#' @param scale_x_break_by a numeric, interval width when the x-axis (i.e. time) will be broken (e.g. every `4` weeks), as well as the times when the number of patient at risk is calculated. Default is `NULL` (calls the default setup of ggplot2).
#' @param scale_y_labels passed to `ggplot2::scale_y_continuous(labels = )` when the Kaplan-Meier curve is constructed. Default is `scales::percent` to label the survival as percentage. Set to `identity` or `ggplot2::waiver()` for the original numeric values.
#' @param label_x a character, the name of the x-axis (i.e. time) of the Kaplan-Meier figure and of the risk table.
#' @param label_y_fig a character, the name of the y-axis (i.e. survival) of the Kaplan-Meier figure.
#' @param label_color a character, the name of the legend for colors (and, by default, of the y-axis of the risk table). Can also be an expression (`expression()`) if special character or super/subscripts should be shown.
#' @param label_y_risktab a character, the name of the y-axis of the risk table. Default is the same as the name of the legend for colors `label_color`.
#' @param legend_label_kpm a character, the label of the legend that describes the line type of the Kaplan-Meier curve.
#' @param legend_label_censor a character, the label of the legend that describes the shape of the censoring events.
#' @param legend_label_se a character, the label of the legend that describes the ribbon of parametric confidence interval. Default is computed as function of `ci_level`.
#' @param legend_label_median a character, the label of the legend that describes the line type of the median survival.
#' @param legend_label_sim a character, the label of the legend that describes the ribbon of simulation-based confidence interval. Default is computed as function of `ci_level`.
#' @param scale_color_labels passed to `ggplot2::scale_color_manual(labels = )` for the Kaplan-Meier figure, and `ggplot2::scale_y_discrete(labels = )` for the risk table. The default is `waiver()` (calls the default setup of ggplot2), but can accept `scales::label_parsed` if the coloring variable is an expression (`expression()`) and special character or super/subscripts should be shown.
#' @param scale_color_values passed to `ggplot2::scale_color_manual(values = )` for the Kaplan-Meier figure and the risk table. Default will use the default PMX graphic charter, but named vectors of colors (with names matching with factor levels) are welcome. Levels not available in the data will be dropped.
#' @param facetting_args a list, arguments that will overwrite the default arguments of `facet_wrap()`. Note that it will not overwrite `vars`, the latter being informed by `facet_var`. Use `list(labeller = ggplot2::label_parsed)` if the facetting variable is an expression (`expression()`) and special character or super/subscripts should be shown.
#' @param xlim a vector of 2 numeric values, limits of the x-axes. Default will use the default ggplot setup.
#' @param ylim a vector of 2 numeric values, limits of the y-axis of the Kaplan-Meier figure. Default is 0 to 1.
#' @param title_risktable a character, the title above the risk table.
#' @param arrange a logical, should the elements of the plot be arranged in a single plot with `cowplot::grid.arrange()` (default is `TRUE`). If `FALSE`, a list with separated elements will be returned (useful for advanced customization.)
#' @param rel_heights a list specifying the relative heights (1) between the legends (default is 1 and 1, same height), (2) between the Kaplan-Meier figure and the risk table (default is 2 and 1, i.e. figure twice higher than table) and (3) overall (default is 1 and 8, combined legends takes 1/9 of heights, combined figure and tables takes 8/9 of heights)
#' @param check_input a logical, should the data format be checked?
#'
#' @return by default, a single "ggplot" object arranged with `cowplot::plot.grid()`. If `arrange = FALSE`, a list of ggplot objects.
#' @export
#'
#' @examples
#'
#' dat <- PMXtte::simplettedata
#' simdat <- PMXtte::simplettesimdata
#'
#' ggKAP(dat) # Graphical exploration
#' ggKAP(dat, simdat) # VPC
#' ggKAP(sdata = simdat) # Simulation only
#'
#' ggKAP(dat, show_kpm = FALSE)
#' ggKAP(dat, show_censor = FALSE)
#' ggKAP(dat, show_se = FALSE)
#' ggKAP(dat, show_risktable = FALSE)
#' ggKAP(dat, show_median = FALSE)
#' try(ggKAP(dat, show_sim = TRUE), TRUE)
#'
#' ggKAP(dat, cuminc = TRUE)
#'
#' ggKAP(dat, color_var = "SEXF")
#' ggKAP(dat, color_var = "SEXF", label_color = "Sex")
#' ggKAP(dat, color_var = "SEXF", show_pval = FALSE)
#' ggKAP(dat, color_var = "SEXF", scale_color_values = c("red", "yellow"))
#' ggKAP(dat, color_var = "SEXF", scale_color_values = c(Male = "yellow", Female = "red"))
#' ggKAP(dat, color_var = "SEXF", ci_alpha = .3)
#' ggKAP(dat, color_var = "SEXF", ci_alpha = .7)
#'
#' ggKAP(dat, color_var = "SEXF", ci_level = .5)
#' ggKAP(
#'   dat, ci_alpha = .2, censor_shape = 19,
#'   censor_size = 6, median_linetype = 3
#' )
#'
#' ggKAP(dat, scale_x_break_by = 12) #Both risk table and graph are updated
#'
#' ggKAP(dat, rel_heights = list(
#'   legends = c(1,1),
#'   figtable = c(8,2),
#'   overall = c(1,8)
#' ))
#' ggKAP(dat, color_var = "SEXF", facet_var = "AUCQF", rel_heights = list(
#'   legends = c(1,1),
#'   figtable = c(8,4),
#'   overall = c(2,8)
#' ))
#'
#' ggKAP(dat, simdat, facet_var = "SEXF")
#' ggKAP(sdata = simdat, facet_var = "SEXF")
#'
#' ggKAP(dat, color_var = "AUCQF")
#' ggKAP(dat, color_var = "AUCQF", label_color = expression(AUC['0-24h']))
#' dat$AUCQF2 <- factor(dat$AUCQF, labels = c("1^st", "2^nd~quartile", "The~3^rd~q"))
#' ggKAP(dat, color_var = "AUCQF2", label_color = expression(AUC['0-24h']), scale_color_labels = scales::label_parse())
#'
#' ggKAP(dat, facet_var = "AUCQF2")
#' ggKAP(dat, facet_var = "AUCQF2", facetting_args = list(labeller = ggplot2::label_parsed, nrow = 2))
ggKAP <- function(data,
                  sdata,
                  time_var = "TIME",
                  dv_var = "DV",
                  iter_var = "ITER",
                  color_var = NULL,
                  facet_var = NULL,
                  show_kpm = !missing(data),
                  show_censor = !missing(data),
                  show_risktable = !missing(data),
                  show_se = !missing(data)&&missing(sdata),
                  show_pval = !missing(data)&&missing(sdata)&&!is.null(color_var)&&(if(!is.null(facet_var)){color_var!=facet_var}else{TRUE}),
                  show_median = !missing(data)&&missing(sdata),
                  show_sim = !missing(sdata),
                  ci_level = if(show_se) .95 else .90,
                  cuminc = FALSE,
                  ci_alpha = .5,
                  censor_shape = "|",
                  censor_size = 4,
                  median_linetype = 2,
                  scale_x_break_by = NULL,
                  scale_y_labels = scales::percent,
                  label_x = "Time",
                  label_y_fig = paste0("Subjects with", if(!cuminc){"out"}, " event (%)"),
                  label_color = color_var,
                  label_y_risktab = label_color,
                  legend_label_kpm = "Observed",
                  legend_label_censor = "Censored",
                  legend_label_se = paste0(scales::percent(ci_level), " CI"),
                  legend_label_median = "Median Survival",
                  legend_label_sim = paste0("Simulated ", scales::percent(ci_level), " CI"),
                  scale_color_labels = waiver(),
                  scale_color_values = unname(PMXColors_PMXcolors$default),
                  facetting_args = list(),
                  xlim = c(NA_real_, NA_real_),
                  ylim = c(0, 1),
                  title_risktable = "Number of subjects at risk",
                  arrange = TRUE,
                  rel_heights = list(
                    legends = c(1,1),
                    figtable = c(2,1),
                    overall = c(1,8)
                  ),
                  check_input = TRUE
){

  if(show_se && show_sim){
    stop("Both `show_se` and `show_sim` are TRUE. Cannot display both confidence intervals on the same plot.")
  }

  if(check_input){
    check_ggKAP_input(
      data = data,
      sdata = sdata,
      time_var = time_var,
      dv_var = dv_var,
      iter_var = iter_var,
      color_var = color_var,
      facet_var = facet_var)
  }

  if(!is.null(scale_x_break_by)){
    scale_x_breaks <- seq(0, max(data[[time_var]]), by = scale_x_break_by)
  } else {
    scale_x_breaks <- ggplot2::waiver()
  }

  if(!is.null(color_var)){
    ..col <- expr(.data[[color_var]])
  } else {
    ..col <- NULL
  }

  if(any(show_kpm, show_censor, show_se, show_pval, show_median, show_sim)){
    fig <- ggplot(mapping = aes(x = eval(expr(.data[[time_var]])), y = eval(expr(.data[[dv_var]]))))
    leg <- ggplot(data = data.frame(x = c(1,2), y = 1))
  } else {
    fig <- NULL
    leg <- NULL
  }

  if(show_kpm){
    fig <- fig +
      stat_kaplanmeier(
        aes(color = eval(..col)),
        data = data,
        cuminc = cuminc
      )

    leg <- leg +
      geom_line(aes(x = x, y = y, linetype = legend_label_kpm))
  }

  if(show_censor){
    fig <- fig +
      stat_kaplanmeier_censor(
        aes(color = eval(..col)),
        data = data,
        cuminc = cuminc,
        shape = censor_shape,
        size = censor_size,
        show.legend = FALSE
      )

    leg <- leg +
      geom_point(
        aes(x = x, y = y, shape = legend_label_censor),
        size = censor_size
      )
  }

  if(show_se){
    fig <- fig +
      stat_kaplanmeier_se(
        aes(fill = eval(..col)),
        data = data,
        cuminc = cuminc,
        alpha = ci_alpha,
        level = ci_level
      )

    leg <- leg +
      geom_ribbon(aes(x = x, ymin = y, ymax = 1, fill = legend_label_se))
  }

  if(show_pval){
    fig <- fig +
      stat_kaplanmeier_pval(
        aes(strata = eval(..col)),
        data = data
      )
  }

  if(show_median){
    fig <- fig +
      stat_kaplanmeier_median(
        aes(col = eval(..col)),
        linetype = median_linetype,
        data = data
      )

    leg <- leg +
      geom_line(aes(x = x, y = y, linetype = legend_label_median))
  }

  if(show_sim){
    fig <- fig +
      stat_kaplanmeier_sim(
        aes(iter = eval(expr(.data[[iter_var]])), fill = eval(..col)),
        data = sdata,
        cuminc = cuminc,
        level = ci_level,
        alpha = ci_alpha
      )

    leg <- leg +
      geom_ribbon(aes(x = x, ymin = y, ymax = 1, fill = legend_label_sim))
  }

  if(show_risktable){
    ..y <- ..col
    if(is.null(..y)){
      ..y <- factor("")
    }

    risktab <- ggplot(data) +
      stat_kaplanmeier_risktable(
        aes(
          x = eval(expr(.data[[time_var]])),
          y = eval(..y),
          col = eval(..col)
        )
      ) + # To align x-axis limits with the Kaplan Meier plot
      geom_blank(aes(x = eval(expr(.data[[time_var]]))))

  } else {
    risktab <- NULL
  }

  # Facetting
  if(!is.null(facet_var)){
    all_facetting_args <- formals(facet_wrap)
    all_facetting_args[names(facetting_args)] <- facetting_args
    all_facetting_args[["facets"]] <- ~ eval(expr(.data[[facet_var]]))

    facet_fun <- do.call(what = facet_wrap, args = all_facetting_args)

    fig <- fig +
      facet_fun
    risktab <- risktab +
      facet_fun
  }

  # Scales
  leg <- leg +
    scale_shape_manual(name = NULL, values = censor_shape) +
    scale_fill_manual(name = NULL, values = "grey")
  if(any(show_kpm, show_median)){ # avoid a warning due to limits = force
    leg <- leg +
      scale_linetype_manual(
        name = NULL,
        values = setNames(c(median_linetype,1), c(legend_label_median, legend_label_kpm)),
        limits = force # so that "observed" or "median" are removed from legend if not in plot
      )
  }

  do_force <- NULL
  if(!is.null(color_var)){
    do_force <- base::force
  }

  fig <- fig +
    scale_x_continuous(
      breaks = scale_x_breaks,
      limits = xlim
    ) +
    scale_y_continuous(
      labels = scale_y_labels,
      limits = ylim
    ) +
    scale_color_manual(
      labels = scale_color_labels,
      values = scale_color_values,
      aesthetics = c("fill", "colour"),
      limits = do_force # so that only levels in the data show up
    )

  risktab <- risktab +
    scale_x_continuous(
      breaks = scale_x_breaks,
      limits = xlim
    ) +
    scale_y_discrete(
      labels = scale_color_labels
    ) +
    scale_color_manual(
      values = scale_color_values
    )

  # Guides
  leg <- leg +
    guides(
      linetype = guide_legend(order = 1),
      fill = guide_legend(order = 2),
      shape = guide_legend(order = 3)
    )

  # Titles
  # labs() don't accept NULL as input, so need to wrap with an if()
  if(!is.null(fig)){
    fig <- fig +
      labs(
        x = label_x,
        y = label_y_fig,
        col = label_color,
        fill = label_color
      )
  }

  if(!is.null(risktab)){
    risktab <- risktab +
      labs(
        subtitle = title_risktable,
        x = label_x,
        y = label_color
      )
  }

  # Theme
  leg <- leg +
    theme(legend.position = "top")

  fig <- fig +
    theme(legend.position = "top")

  risktab <- risktab +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # Make list
  ans <- list(
    leg_col = cowplot::get_legend(fig), # Colors only
    leg_kpm = cowplot::get_legend(leg), # KM legend (Observed, Censored etc)
    fig     = fig + theme(legend.position = "none"),
    risktab = risktab
  )

  ans <- ans[!sapply(ans, is.null)]

  if(arrange){
    ans <- cowplot::plot_grid(
      cowplot::plot_grid(
        plotlist = ans[str_detect(names(ans), "leg")],
        ncol = 1,
        rel_heights = rel_heights[["legends"]]
      ),
      cowplot::plot_grid(
        plotlist = ans[str_detect(names(ans), "leg", negate = TRUE)],
        ncol = 1,
        align = "v",
        rel_heights = rel_heights[["figtable"]]
      ),
      ncol = 1,
      rel_heights = rel_heights[["overall"]]
    )
  }

  ans
}

check_ggKAP_input <- function(
    data,
    sdata,
    time_var = "TIME",
    dv_var = "DV",
    iter_var = "ITER",
    color_var = NULL,
    facet_var = NULL){
  if(!missing(data)){
    # Check if variable names are indeed in the data
    # NULL wont return an error
    check_var_exists(data, time_var)
    check_var_exists(data, dv_var)
    check_var_exists(data, color_var)
    check_var_exists(data, facet_var)

    # Check data classes
    check_valid_class(data, time_var, valid_classes = c("integer", "numeric"))
    check_valid_class(data, dv_var, valid_classes = c("integer", "numeric"))
    check_valid_class(data, color_var, valid_classes = c("logical", "character", "factor"))
  }
  if(!missing(sdata)){
    # Check if variable names are indeed in the data
    check_var_exists(sdata, time_var)
    check_var_exists(sdata, dv_var)
    check_var_exists(sdata, iter_var)
    check_var_exists(sdata, color_var)
    check_var_exists(sdata, facet_var)

    # Check data classes
    check_valid_class(sdata, time_var, valid_classes = c("integer", "numeric"))
    check_valid_class(sdata, dv_var, valid_classes = c("integer", "numeric"))
    check_valid_class(sdata, iter_var, valid_classes = c("integer", "numeric"))
    check_valid_class(sdata, color_var, valid_classes = c("logical", "character", "factor"))
  }
}

check_var_exists <- function(data, x){
  if(is.null(x)) {
    return(TRUE)
  } else {
    if(!is.element(el = x, set = colnames(data))){
      stop("Cannot find \"", x, " in data")
    }
  }
}

check_valid_class <- function(data, x, valid_classes = c("logical", "character", "factor")){
  if(is.null(x)) {
    return(TRUE)
  } else {
    if(!is.element(el = class(data[[x]]), set = valid_classes)){
      stop("Possible classes for \"", x, "\" are: ", paste(valid_classes, collapse = ", "))
    }
  }
}
