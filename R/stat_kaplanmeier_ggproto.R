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

#' @inherit ggplot2::ggproto
#' @export
StatKaplanMeier <- ggproto(
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
                                    inherit.aes = TRUE, ..., shape = "|", cuminc = FALSE) {
  layer(
    stat = StatKaplanMeierCensor, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., shape = shape, cuminc = cuminc)
  )
}

#' @inherit ggplot2::ggproto
#' @export
StatKaplanMeierCensor <- ggproto(
  "StatKaplanMeierCensor", Stat,
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
                                    inherit.aes = TRUE, ..., linetype = 2){
  layer(
    stat = StatKaplanMeierMedian, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., linetype = linetype)
  )
}

#' @inherit ggplot2::ggproto
#' @export
StatKaplanMeierMedian <- ggproto(
  "StatKaplanMeierMedian", Stat,
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
StatKaplanMeierSE <- ggproto(
  "StatKaplanMeierSE", Stat,
  compute_group = function(data, scales, level, cuminc) {
    sf <- kpm_ci_stepfun(time = data$x, dv = data$y, cuminc = cuminc, width = level)
    times <- c(0, data$x)
    dat <- data.frame(
      x = times,
      ymin = sf(times)[,2],
      ymax = sf(times)[,1]
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

#' @inherit ggplot2::ggproto
#' @export
stat_kaplanmeier_se <- function(mapping = NULL, data = NULL, geom = "ribbon",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ..., level = .95, cuminc = FALSE) {
  layer(
    stat = StatKaplanMeierSE, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., level = level, cuminc = cuminc)
  )
}







#' P-value of the likelihood ratio test between several Kaplan-Meier functions
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
#' # Requires a `strata` argument
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
#' # Supported variables defined in `aes()` and facetting
#' # P-value calculated for each panel
#' ggplot(dat, aes(x = TIME, y = DV)) +
#'   stat_kaplanmeier(aes(group = AUCQF)) +
#'   stat_kaplanmeier_pval(aes(strata = AUCQF, colour = SEXF)) +
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

#' @inherit ggplot2::ggproto
#' @export
StatKaplanMeierPval <- ggproto(
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










StatKaplanMeierRiskTable <- ggproto(
  "StatKaplanMeierRiskTable", Stat,
  compute_group = function(data, scales) {
    times <- seq(0, 8000, 2000)

    data %>%
      group_by(y) %>%
      summarise(
        STEPFUN = list(atrisk_stepfun(time = x)),
        TABTIME = list(times),
        .groups = "keep"
      ) %>%
      mutate(
        ATRISK = list(do.call(what = STEPFUN[[1]], args = TABTIME)),
        STEPFUN = NULL
      ) %>%
      ungroup() %>%
      unnest(c(TABTIME, ATRISK)) %>%
      rename(
        x = TABTIME,
        label = ATRISK
      )
  },

  required_aes = c("x", "y")
)

stat_kaplanmeier_risktable <- function(mapping = NULL, data = NULL, geom = "text",
                                       position = "identity", na.rm = FALSE, show.legend = NA,
                                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatKaplanMeierRiskTable, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatKaplanMeierSim <- ggproto(
  "StatKaplanMeierSim", Stat,
  compute_group = function(data, scales) {
    data %>%
      group_by(iter, .add = TRUE) %>%
      summarise(
        STEPFUN = list(kpm_stepfun(time = x, dv = y)),
        MAXTIME = max(x),
        .groups = "keep"
      ) %>%
      mutate(
        OUTTIME = list(seq(0, MAXTIME, length.out = 50)),
        SURV = list(do.call(what = STEPFUN[[1]], args = OUTTIME)),
        STEPFUN = NULL
      ) %>%
      unnest(c(OUTTIME, SURV)) %>%
      ungroup(iter) %>%
      group_by(OUTTIME, .add = TRUE) %>%
      summarise(
        Q05 = quantile(SURV, 0.05),
        Q95 = quantile(SURV, 0.95),
        .groups = "drop"
      ) %>%
      select(
        x = OUTTIME,
        ymin = Q05,
        ymax = Q95
      )
  },

  required_aes = c("x", "y", "iter")
)

stat_kaplanmeier_sim <- function(mapping = NULL, data = NULL, geom = "ribbon",
                                 position = "identity", na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE, ...) {
  layer(
    stat = StatKaplanMeierSim, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}




