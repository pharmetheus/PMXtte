

StatKaplanMeier <- ggproto(
  "StatKaplanMeier", Stat,
  compute_group = function(data, scales) {
    sf <- kpm_stepfun(time = data$x, dv = data$y)
    times <- c(0, data$x)
    data.frame(
      x = times,
      y = sf(times)
    )
  },

  required_aes = c("x", "y")
)

stat_kaplanmeier <- function(mapping = NULL, data = NULL, geom = "step",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
  layer(
    stat = StatKaplanMeier, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



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


StatKaplanMeierCensor <- ggproto(
  "StatKaplanMeierCensor", Stat,
  compute_group = function(data, scales) {
    sf <- kpm_stepfun(time = data$x, dv = data$y)
    times <- data$x[data$y==0]
    data.frame(
      x = times,
      y = sf(times)
    )
  },

  required_aes = c("x", "y")
)

stat_kaplanmeier_censor <- function(mapping = NULL, data = NULL, geom = "point",
                                    position = "identity", na.rm = FALSE, show.legend = NA,
                                    inherit.aes = TRUE, ..., shape = "|") {
  layer(
    stat = StatKaplanMeierCensor, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., shape = shape)
  )
}

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

stat_kaplanmeier_median <- function(mapping = NULL, data = NULL, geom = "line",
                                    position = "identity", na.rm = FALSE, show.legend = NA,
                                    inherit.aes = TRUE, linetype = 2, ...) {
  layer(
    stat = StatKaplanMeierMedian, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, linetype = linetype, ...)
  )
}


StatKaplanMeierSE <- ggproto(
  "StatKaplanMeierSE", Stat,
  compute_group = function(data, scales, width) {
    sf <- kpm_ci_stepfun(time = data$x, dv = data$y, width = width)
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

stat_kaplanmeier_se <- function(mapping = NULL, data = NULL, geom = "ribbon",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ..., width = .95) {
  layer(
    stat = StatKaplanMeierSE, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., width = width)
  )
}


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

stat_kaplanmeier_pval <- function(mapping = NULL, data = NULL, geom = "text",
                                  position = "identity", na.rm = FALSE, show.legend = NA,
                                  inherit.aes = TRUE, ..., xpos = NULL, ypos = NULL) {
  layer(
    stat = StatKaplanMeierPval, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ..., xpos = xpos, ypos = ypos)
  )
}
