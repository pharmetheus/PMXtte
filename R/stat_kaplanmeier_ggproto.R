#' StatKaplanMeier ggproto objects
#' @name StatKaplanMeierGGproto
#' @description
#' `StatKaplanMeier`
#' `StatKaplanMeierCensor`
#' `StatKaplanMeierMedian`
#' `StatKaplanMeierSE`
#' `StatKaplanMeierPval`
#' `StatKaplanMeierSim`
#' `StatKaplanMeierRiskTable`
#' @seealso [ggplot2::geom_step()], [ggplot2::geom_point()],
#' [ggplot2::geom_line()], [ggplot2::geom_ribbon()], [ggplot2::geom_text()] from
#' the ggplot2 package.
#'
NULL

#' Kaplan-Meier estimator of the survival function
#'
#' @param cuminc a logical. If `TRUE`, calculates the cumulative incidence of events. If `FALSE` (the default), the survival function.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_step
#'
#' @export
#' @examples
#' library(ggplot2)
#' dat <- PMXtte::simplettedata
#'
#' ggplot(dat) +
#'   stat_kaplanmeier(aes(x = TIME, y = DV))
#'
#' # Supported variables defined in `aes()` and facetting
#' ggplot(dat) +
#'   stat_kaplanmeier(aes(x = TIME, y = DV, color = AUCQF)) +
#'   facet_grid(~SEXF)
#'
#' # Display the cumulative incidence
#' ggplot(dat) +
#'   stat_kaplanmeier(aes(x = TIME, y = DV), cuminc = TRUE)
#'
#' # Default geom is "step", any compatible option can be used
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(size = 2, linetype = 2, color = "red")
#'
#' # or change the geom
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(geom = "point")
stat_kaplanmeier <- function(mapping = NULL, data = NULL, geom = "step",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ..., cuminc = FALSE) {
  layer(
    stat = StatKaplanMeier, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., cuminc = cuminc)
  )
}

#' @rdname StatKaplanMeierGGproto
#' @export
StatKaplanMeier <- ggplot2::ggproto(
  "StatKaplanMeier", Stat,
  compute_group = function(data, scales, cuminc) {
    sf <- kpm_stepfun(time = data$x, dv = data$y, cuminc = cuminc)
    times <- c(0, data$x)
    data.frame(
      x = times,
      y = sf(times)
    )
  },
  required_aes = c("x", "y")
)











#' Censoring events on the survival function
#'
#' @param cuminc a logical. If `TRUE`, calculates the cumulative incidence of events. If `FALSE` (the default), the survival function.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' dat <- PMXtte::simplettedata
#'
#' # Is of little value on itself...
#' ggplot(dat) +
#'   stat_kaplanmeier_censor(aes(x = TIME, y = DV))
#'
#' # Better if supported with the survival function
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier() +
#'   stat_kaplanmeier_censor()
#'
#' # Supported variables defined in `aes()` and facetting
#' ggplot(dat, aes(x = TIME, y = DV, col = AUCQF)) +
#'   stat_kaplanmeier() +
#'   stat_kaplanmeier_censor() +
#'   facet_grid(~SEXF)
#'
#' # Display the cumulative incidence
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(cuminc = TRUE) +
#'   stat_kaplanmeier_censor(cuminc = TRUE)
#'
#' # Default geom is "point", any compatible option can be used
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier() +
#'   stat_kaplanmeier_censor(shape = "x", size = 5, color = "red")
#'
#' # or change the geom
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier() +
#'   stat_kaplanmeier_censor(geom = "line", color = "red")
stat_kaplanmeier_censor <- function(mapping = NULL, data = NULL, geom = "point",
                                    position = "identity", na.rm = FALSE, show.legend = NA,
                                    inherit.aes = TRUE, ..., cuminc = FALSE) {
  layer(
    stat = StatKaplanMeierCensor, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., cuminc = cuminc)
  )
}

#' @rdname StatKaplanMeierGGproto
#' @export
StatKaplanMeierCensor <- ggplot2::ggproto(
  "StatKaplanMeierCensor", Stat,
  default_aes = aes(shape = "|"),
  compute_group = function(data, scales, cuminc) {
    sf <- kpm_stepfun(time = data$x, dv = data$y, cuminc = cuminc)
    times <- data$x[data$y==0]
    data.frame(
      x = times,
      y = sf(times)
    )
  },
  required_aes = c("x", "y")
)








#' Median survival on the survival function
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' dat <- PMXtte::simplettedata
#'
#' # Is of little value on itself...
#' ggplot(dat) +
#'   stat_kaplanmeier_median(aes(x = TIME, y = DV))
#'
#' # Better if supported with the survival function
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier() +
#'   stat_kaplanmeier_median()
#'
#' # Supported variables defined in `aes()` and facetting
#' ggplot(dat, aes(x = TIME, y = DV, col = AUCQF)) +
#'   stat_kaplanmeier() +
#'   stat_kaplanmeier_median() +
#'   facet_grid(~SEXF)
#'
#' # No argument needed if the cumulative incidence is plotted.
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(cuminc = TRUE) +
#'   stat_kaplanmeier_median()
#'
#' # Default geom is "line", any compatible option can be used
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier() +
#'   stat_kaplanmeier_median(linetype = 3, size = 5, color = "red")
#'
#' # or change the geom
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier() +
#'   stat_kaplanmeier_median(geom = "point", color = "red")
stat_kaplanmeier_median <- function(mapping = NULL, data = NULL, geom = "line",
                                    position = "identity", na.rm = FALSE, show.legend = NA,
                                    inherit.aes = TRUE, ...){
  layer(
    stat = StatKaplanMeierMedian, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname StatKaplanMeierGGproto
#' @export
StatKaplanMeierMedian <- ggplot2::ggproto(
  "StatKaplanMeierMedian", Stat,
  default_aes = aes(linetype = 2),
  compute_group = function(data, scales) {
    sf <- kpm_stepfun(time = data$x, dv = data$y)
    medtime <- medsurvtime(
      time = data$x,
      surv = sf(data$x)
    )
    if(is.na(medtime)){
      return(NULL)
    }
    data.frame(
      x = c(0, medtime, medtime),
      y = c(.5, .5, 0)
    )
  },
  required_aes = c("x", "y")
)







#' Standard error-based confidence interval on the survival function
#'
#' @param cuminc a logical. If `TRUE`, calculates the cumulative incidence of events. If `FALSE` (the default), the survival function.
#' @param level Level of confidence interval to use (0.95 by default)
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams ggplot2::geom_ribbon
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' dat <- PMXtte::simplettedata
#'
#' # Is of little value on itself...
#' ggplot(dat) +
#'   stat_kaplanmeier_se(aes(x = TIME, y = DV))
#'
#' # Better if supported with the survival function
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_se(fill = "grey60") +
#'   stat_kaplanmeier()
#'
#' # Supported variables defined in `aes()` and facetting
#' ggplot(dat, aes(x = TIME, y = DV, col = AUCQF, fill = AUCQF)) +
#'   stat_kaplanmeier_se(alpha = .4) +
#'   stat_kaplanmeier() +
#'   facet_grid(~SEXF)
#'
#' # Display the cumulative incidence
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_se(cuminc = TRUE, fill = "grey60") +
#'   stat_kaplanmeier(cuminc = TRUE)
#'
#' # Default geom is "ribbon", any compatible option can be used
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_se(linetype = 3, size = 5, color = "red", fill = "yellow")+
#'   stat_kaplanmeier()
#'
#' # or change the geom
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_se(geom = "errorbar", color = "red") +
#'   stat_kaplanmeier()
stat_kaplanmeier_se <- function(mapping = NULL, data = NULL, geom = "ribbon",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ..., level = .95, cuminc = FALSE) {
  layer(
    stat = StatKaplanMeierSE, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., level = level, cuminc = cuminc)
  )
}

#' @rdname StatKaplanMeierGGproto
#' @export
StatKaplanMeierSE <- ggplot2::ggproto(
  "StatKaplanMeierSE", Stat,
  compute_group = function(data, scales, level, cuminc) {
    sf <- kpm_ci_stepfun(time = data$x, dv = data$y, cuminc = cuminc, width = level)
    times <- c(0, data$x)
    dat <- data.frame(
      x = times,
      ymin = sf(times)[,1],
      ymax = sf(times)[,2]
    )
    # explicitely define two records per time point
    dat <- explicit_step(
      data = dat,
      toexpand = c("ymin", "ymax"),
      time = "x"
    )

    dat
  },

  required_aes = c("x", "y")
)







#' P-value of the likelihood ratio test between several Kaplan-Meier functions
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @param xpos,ypos numeric values, the coordinates of the text. By default (NULL),
#'  coordinates that corresponds toon the bottom left corner will be computed.
#' @export
#'
#' @examples
#' library(ggplot2)
#' dat <- PMXtte::simplettedata
#'
#' # Requires a `strata` aesthetic argument
#' ggplot(dat) +
#'   stat_kaplanmeier_pval(aes(x = TIME, y = DV, strata = SEXF))
#'
#' # Better if supported with the survival function
#' # Make sure the grouping is not done in the main `ggplot()` function.
#' # If you do so, p-value will attempted to be calculated _within_ each group
#' # However we want the p-value to be calculated _between_ each strata level
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(aes(group = AUCQF)) +
#'   stat_kaplanmeier_pval(aes(strata = AUCQF))
#'
#' # Supported facetting
#' # P-value calculated for each panel
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(aes(group = AUCQF)) +
#'   stat_kaplanmeier_pval(aes(strata = AUCQF)) +
#'   facet_grid(~SEXF)
#'
#' # May not support coloring or any variable-related aesthetic mapping
#' # Despite that, the guide (legend) will carry some text attributes
#' # P-value calculated for each panel
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(aes(group = AUCQF, col = SEXF)) +
#'   stat_kaplanmeier_pval(aes(strata = AUCQF, col = SEXF)) + # not colored
#'   facet_grid(~SEXF)
#'
#' # No argument needed if the cumulative incidence is plotted.
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(aes(group = AUCQF), cuminc = TRUE) +
#'   stat_kaplanmeier_pval(aes(strata = AUCQF))
#'
#' # The position of the text is defaulted to the bottom-left of the panel
#' # Change it with `xpos` and `ypos`
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(aes(group = AUCQF), cuminc = TRUE) +
#'   stat_kaplanmeier_pval(aes(strata = AUCQF), xpos = 25, ypos = .75)
#'
#' # Default geom is "text", any compatible option can be used
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(aes(group = AUCQF)) +
#'   stat_kaplanmeier_pval(aes(strata = AUCQF), colour = "red", size = 10)
#'
#' # or change the geom
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(aes(group = AUCQF)) +
#'   stat_kaplanmeier_pval(
#'     aes(strata = AUCQF),
#'     geom = "label", color = "red", fill = "yellow"
#'   )
stat_kaplanmeier_pval <- function(mapping = NULL, data = NULL, geom = "text",
                                  position = "identity", na.rm = FALSE, show.legend = NA,
                                  inherit.aes = TRUE, ..., xpos = NULL, ypos = NULL) {
  layer(
    stat = StatKaplanMeierPval, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., xpos = xpos, ypos = ypos)
  )
}

#' @rdname StatKaplanMeierGGproto
#' @export
StatKaplanMeierPval <- ggplot2::ggproto(
  "StatKaplanMeierPval", Stat,

  compute_panel = function(data, scales, xpos = NULL, ypos = NULL) {
    fit <- survival::survdiff(
      formula = (survival::Surv(x,y) ~ survival::strata(strata)),
      data = data
    )

    if(is.null(xpos)){
      xpos <- min(data$x)+.2*(max(data$x)-min(data$x))
    }

    if(is.null(ypos)){
      ypos <- min(data$y)+.2*(max(data$y)-min(data$y))
    }

    pval <- pretty_pval(fit$pvalue)

    data.frame(
      x = xpos,
      y = ypos,
      label = pval
    )
  },

  required_aes = c("x", "y", "strata")
)








#' Simulation-based confidence interval on the survival function
#'
#' @param cuminc a logical. If `TRUE`, calculates the cumulative incidence of events. If `FALSE` (the default), the survival function.
#' @param level Level of confidence interval to use (0.90 by default)
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams ggplot2::geom_ribbon
#' @inheritParams ggplot2::stat_function
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' dat <- PMXtte::simplettedata
#' simdat <- PMXtte::simplettesimdata
#'
#' # Requires a `iter` aesthetic argument
#' ggplot(simdat) +
#'   stat_kaplanmeier_sim(aes(x = TIME, y = DV, iter = ITER))
#'
#' # Better if supported with the survival function
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_sim(aes(iter = ITER), data = simdat, fill = "grey60") +
#'   stat_kaplanmeier()
#'
#' # Supported variables defined in `aes()` and facetting
#' ggplot(dat, aes(x = TIME, y = DV, col = AUCQF, fill = AUCQF)) +
#'   stat_kaplanmeier_sim(aes(iter = ITER), data = simdat, alpha = .4) +
#'   stat_kaplanmeier() +
#'   facet_grid(~SEXF)
#'
#' # Display the cumulative incidence
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_sim(
#'     aes(iter = ITER), data = simdat,
#'     cuminc = TRUE, fill = "grey60") +
#'   stat_kaplanmeier(cuminc = TRUE)
#'
#' # Change the level of the confidence interval
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_sim(
#'     aes(iter = ITER), data = simdat, fill = "grey60",
#'     level = .50 # Very narrow CI...
#'     ) +
#'   stat_kaplanmeier()
#'
#' # Change the number of time points
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_sim(
#'     aes(iter = ITER), data = simdat, fill = "grey60",
#'     n = 200 # Very high number
#'     ) +
#'   stat_kaplanmeier()
#'
#' # Change the range of time points
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_sim(
#'     aes(iter = ITER), data = simdat, fill = "grey60",
#'     xlim = c(25, 80) # Display the CI in a small time range
#'     ) +
#'   stat_kaplanmeier()
#'
#' # Default geom is "ribbon", any compatible option can be used
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_sim(
#'     aes(iter = ITER), data = simdat,
#'     linetype = 3, size = 5, color = "red", fill = "yellow") +
#'   stat_kaplanmeier()
#'
#' # or change the geom
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier_sim(
#'     aes(iter = ITER), data = simdat,
#'     geom = "errorbar", color = "red") +
#'   stat_kaplanmeier()
stat_kaplanmeier_sim <- function(mapping = NULL, data = NULL, geom = "ribbon",
                                 position = "identity", na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE, ..., cuminc = FALSE, n = 101, xlim = NULL, level = 0.9) {
  layer(
    stat = StatKaplanMeierSim, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., cuminc = cuminc, n = n, xlim = xlim, level = level)
  )
}

#' @rdname StatKaplanMeierGGproto
#' @export
StatKaplanMeierSim <- ggplot2::ggproto(
  "StatKaplanMeierSim", Stat,
  compute_group = function(data, scales, cuminc = FALSE, n = 101, xlim = NULL, level = 0.9) {
    if(is.null(xlim)){
      xlim <- c(0, max(data$x))
    }
    ans <- data %>%
      dplyr::group_by(iter, .add = TRUE) %>%
      dplyr::summarise(
        STEPFUN = list(kpm_stepfun(time = x, dv = y)),
        .groups = "keep"
      ) %>%
      dplyr::mutate(
        OUTTIME = list(seq(xlim[1], xlim[2], length.out = n)),
        SURV = list(do.call(what = STEPFUN[[1]], args = OUTTIME)),
        STEPFUN = NULL
      ) %>%
      tidyr::unnest(c(OUTTIME, SURV)) %>%
      dplyr::ungroup(iter) %>%
      dplyr::group_by(OUTTIME, .add = TRUE) %>%
      dplyr::summarise(
        lci = stats::quantile(SURV, width2bounds(level)[1], names = FALSE),
        uci = stats::quantile(SURV, width2bounds(level)[2], names = FALSE),
        .groups = "drop"
      )

    if(cuminc){
      ans$lci <- 1 - ans$lci
      ans$uci <- 1 - ans$uci
    }

    ans %>%
      dplyr::select(
        x = OUTTIME,
        ymin = lci,
        ymax = uci
      )
  },

  required_aes = c("x", "y", "iter")
)












#' Number of subjects at risk
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @param times time values when the number at risk is computed. The default (NULL)
#' uses an internal algorithm that should match the breaks and limits of the x-axis.
#'
#' @export
#' @examples
#' library(ggplot2)
#' dat <- PMXtte::simplettedata
#'
#' ggplot(dat) +
#'   stat_kaplanmeier_risktable(aes(x = TIME, y = 1))
#'
#' # Supported variables defined in `aes()` and facetting
#' ggplot(dat) +
#'   stat_kaplanmeier_risktable(aes(x = TIME, y = AUCQF, color = AUCQF)) +
#'   facet_grid(~SEXF)
#'
#' # Change the `times` when numbers are calculated
#' ggplot(dat) +
#'   stat_kaplanmeier_risktable(
#'   aes(x = TIME, y = 1),
#'   times = seq(0, 100, 10))
#'
#' # `times` should match the breaks of the x-axis
#' ggplot(dat) +
#'   stat_kaplanmeier_risktable(aes(x = TIME, y = 1)) +
#'   scale_x_continuous(breaks = c(0, 15, 50, 99))
#'
#' # Default geom is "text", any compatible option can be used
#' ggplot(dat, aes(x = TIME, y = 1)) +
#'   stat_kaplanmeier_risktable(size = 10, color = "red")
#'
#' # or change the geom
#' ggplot(dat, aes(x = TIME, y = 1)) +
#'   stat_kaplanmeier_risktable(geom = "label", fill = "yellow")
stat_kaplanmeier_risktable <- function(mapping = NULL, data = NULL, geom = "text",
                                       position = "identity", na.rm = FALSE, show.legend = NA,
                                       inherit.aes = TRUE, ..., times = NULL) {
  layer(
    stat = StatKaplanMeierRiskTable, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., times = times)
  )
}

#' @rdname StatKaplanMeierGGproto
#' @export
StatKaplanMeierRiskTable <- ggplot2::ggproto(
  "StatKaplanMeierRiskTable", Stat,
  compute_group = function(data, scales, times = NULL) {
    if(is.null(times)){
      breaks <- scales$x$get_breaks()
      if(any(is.na(breaks))){
        if(is.null(scales$x$limits)||any(is.na(scales$x$limits)))
          extended_limits <- labeling::extended(
            dmin = scales$x$range$range[1],
            dmax = scales$x$range$range[2],
            m = 2)
        if(is.na(breaks[1])){
          if(is.null(scales$x$limits)||is.na(scales$x$limits[1])){
            breaks[1] <- extended_limits[1]
          } else {
            breaks[1] <- scales$x$limits[1]
          }
        }
        if(is.na(breaks[length(breaks)])){
          if(is.null(scales$x$limits)||is.na(scales$x$limits[2])){
            breaks[length(breaks)] <- extended_limits[2]
          } else {
            breaks[length(breaks)] <- scales$x$limits[2]
          }
        }
      }
      times <- breaks
    }

    data %>%
      dplyr::group_by(y) %>%
      dplyr::summarise(
        STEPFUN = list(atrisk_stepfun(time = x)),
        TABTIME = list(times),
        .groups = "keep"
      ) %>%
      dplyr::mutate(
        ATRISK = list(do.call(what = STEPFUN[[1]], args = TABTIME)),
        STEPFUN = NULL
      ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(c(TABTIME, ATRISK)) %>%
      dplyr::select(
        x = TABTIME,
        y = y,
        label = ATRISK
      )
  },

  required_aes = c("x", "y")
)



