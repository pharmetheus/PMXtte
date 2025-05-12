#' Visualize Survival Data
#'
#' @param data
#' @param sdata
#' @param time_var
#' @param dv_var
#' @param iter_var
#' @param col_var
#' @param facet_var
#' @param show_kpm
#' @param show_censor
#' @param show_risktable
#' @param show_se
#' @param show_pval
#' @param show_median
#' @param show_sim
#' @param ci_level
#' @param cuminc
#' @param ci_alpha
#' @param censor_shape
#' @param censor_size
#' @param median_linetype
#' @param scale_x_break_by
#' @param scale_y_labels
#' @param label_x
#' @param label_y_fig
#' @param label_col
#' @param label_y_risktab
#' @param legend_label_kpm
#' @param legend_label_censor
#' @param legend_label_se
#' @param legend_label_median
#' @param legend_label_sim
#' @param scale_color_labels
#' @param scale_color_values
#' @param facetting_args
#' @param xlim
#' @param ylim
#' @param title_risktable
#' @param arrange
#' @param rel_heights
#'
#' @return
#' @export
#'
#' @examples
#'
#' dat <- PMXtte::simplettedata
#' simdat <- PMXtte::simplettesimdata
#'
#' ggKAP(dat)
#' ggKAP(dat, show_kpm = FALSE)
#' ggKAP(dat, show_censor = FALSE)
#' ggKAP(dat, show_se = FALSE)
#' ggKAP(dat, show_risktable = FALSE)
#' ggKAP(dat, show_median = FALSE)
#' try(ggKAP(dat, show_sim = TRUE), TRUE)
#'
#'
#' ggKAP(dat, col_var = "SEXF")
#' ggKAP(dat, col_var = "SEXF", show_pval = FALSE)
#' ggKAP(dat, col_var = "SEXF", scale_color_values = c("red", "yellow"))
#' ggKAP(dat, col_var = "SEXF", scale_color_values = c(Male = "yellow", Female = "red"))
#' ggKAP(dat, col_var = "SEXF", ci_alpha = .3)
#' ggKAP(dat, col_var = "SEXF", ci_alpha = .7)
#'
#' ggKAP(dat, rel_heights = list(
#'   legends = c(1,1),
#'   figtable = c(8,2),
#'   overall = c(1,8)
#' ))
#' ggKAP(dat, col_var = "SEXF", facet_var = "AUCQF", rel_heights = list(
#'   legends = c(1,1),
#'   figtable = c(8,4),
#'   overall = c(2,8)
#' ))
#'
#' ggKAP(dat, simdat, facet_var = "SEXF")
#' ggKAP(sdata = simdat, facet_var = "SEXF")
#'
#' ggKAP(dat, col_var = "AUCQF")
#' ggKAP(dat, col_var = "AUCQF", label_col = expression(AUC['0-24h']))
#' dat$AUCQF2 <- factor(dat$AUCQF, labels = c("1^st", "2^nd~quartile", "The~3^rd~q"))
#' ggKAP(dat, col_var = "AUCQF2", label_col = expression(AUC['0-24h']), scale_color_labels = scales::label_parse())
#'
#' ggKAP(dat, facet_var = "AUCQF2")
#' ggKAP(dat, facet_var = "AUCQF2", facetting_args = list(labeller = ggplot2::label_parsed, nrow = 2))
ggKAP <- function(data,
                  sdata,
                  time_var = "TIME",
                  dv_var = "DV",
                  iter_var = "ITER",
                  col_var = NULL,
                  facet_var = NULL,
                  show_kpm = !missing(data),
                  show_censor = !missing(data),
                  show_risktable = !missing(data),
                  show_se = !missing(data)&&missing(sdata),
                  show_pval = !missing(data)&&missing(sdata)&&!is.null(col_var)&&(if(!is.null(facet_var)){col_var!=facet_var}else{TRUE}),
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
                  label_x = "Time since first dose (weeks)",
                  label_y_fig = paste0("Subjects with", if(!cuminc){"out"}, " event (%)"),
                  label_col = col_var,
                  label_y_risktab = label_col,
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
                    figtable = c(8,4),
                    overall = c(1,8)
                  )
){

  if(show_se && show_sim){
    stop("Both `show_se` and `show_sim` are TRUE. Cannot display both confidence intervals on the same plot.")
  }

  if(!is.null(scale_x_break_by)){
    scale_x_breaks <- seq(0, max(data[[time_var]]), by = scale_x_break_by)
  } else {
    scale_x_breaks <- ggplot2::waiver()
  }

  if(!is.null(col_var)){
    ..col <- expr(.data[[col_var]])
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
        values = setNames(c(2,1), c(legend_label_median, legend_label_kpm)),
        limits = force # so that "observed" or "median" are removed from legend if not in plot
      )
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
      aesthetics = c("fill", "colour")
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
        col = label_col,
        fill = label_col
      )
  }

  if(!is.null(risktab)){
    risktab <- risktab +
      labs(
        subtitle = title_risktable,
        x = label_x,
        y = label_col
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
      axis.ticks.y = element_blank()
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
  } else {
    if(length(ans)==1){ #simplify
      ans <- ans[[1]]
    }
  }

  ans
}
