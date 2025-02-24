#' Kaplan-Meier Mean Covariate plots for TTE models
#'
#' @param odata observed data
#' @param sdata simulated data, the number of rows must be proportional to the number of rows in `odata`
#' @param cov_col a character, the covariate column to compute the rolling mean of. Must be present in `cov_data`.
#' @param cov_data covariate data. The default is `odata`, suitable for *non time-varying covariates*. For time-varying covariates, a data set with multiple records per ID that inform the changes in covariate values, should be used, typically a "derived data set"
#' @param time_col a character, the time column. The default is `"TIME"`.  Must be present in `odata`, `sdata` and `cov_data`.
#' @param id_col a character, the subject identifier column. The default is `"ID"`.  Must be present in `odata`, `sdata` and `cov_data`.
#' @param iter_col a character, the simulation replicate identifier column. The default is `"ITER"`.  Must be present in`sdata`.
#' @param grouping_col a character, the grouping variables, useful to facet the plot. The default is `character(0)`, overall data. Must be present in `odata`, and `sdata`.
#' @param bins a numeric, vector of numeric values when the covariate mean should be calculated. Default is set to have 50 values whatever the input data.
#' @param ylab a character, label of the y-axis.
#' @param nocb a logical, should covariate value be interpolated with the "next observation carried backward" rule (the default)? Otherwise "last observation carried forward" will be used
#' @param output_data a logical, should the function return the data that build the plot? If not, the default, a figure is returned
#' @param obsCol a character, the color of the "observed mean" line
#' @param simFill a character, the filling color of the "simulated CI" ribbon
#' @param ci a numeric of length 2, with values between 0 and 1, the width of the confidence interval. Default is a 90% CI.
#'
#' @return By default, a ggplot object. If `output_data = TRUE`, a data.frame to build the plot.
#' @export
#'
#' @details
#'
#' ## Time varying covariates
#' This function calculates the running mean of a covariate as a function of time.
#' This mean is calculated at each time point defined in `bins`, after exclusion of subjects who experienced an event or censoring.
#' For time-varying covariates, the covariate values are interpolated following the "next observation carried backward" rule (unless `nocb=FALSE`).
#' Thus, it is of importance to use the argument `cov_data` with a data set that is rich enough to adequately describe the variation of the covariate in time.
#' Otherwise, only the covariate value in `odata` will be read, which will lead to a biased figure.
#'
#' @examples
#' library(magrittr)
#' derData <- readr::read_csv(
#'   system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'),
#'   show_col_types = FALSE) %>%
#'   dplyr::rename(TIME = TSFDW, SEX = SEXN, DOSE = DOSEN, COSEASON = COSEAS
#'  )
#' odata <- dplyr::filter(derData, EVID == 0, TYPE == 2, STUDYIDN == 1) %>%
#'   filter_xth_event(1)
#' sdata <- readRDS(system.file('extdata/vpcdat.rds', package= 'PMXtte')) %>%
#'   dplyr::group_by(ITER) %>%
#'   filter_xth_event(1) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(TIME = TIME / (24*7), SEX = SEX+1)
#'
#' # Basic plot
#' ggKMMC(odata = odata, sdata = sdata, cov_col = "DOSE")
#'
#' # Works with binary covariate
#' ggKMMC(odata = odata, sdata = sdata, cov_col = "SEX")
#'
#' # For time-varying covariate: use data with records of time-varying values
#' # e.g. the "derived data set" which still has records with `EVID==2`
#' ggKMMC(
#'   odata = odata, sdata = sdata,
#'   cov_col = "COSEASON", cov_data = derData,
#'   bins = seq(0,52,1)
#' )
#'
#' # Grouping variables
#' ggKMMC(odata = odata, sdata = sdata, cov_col = "DOSE", grouping_col = "SEX")
#'
#'
#' # Minimal customization
#' p <- ggKMMC(odata = odata, sdata = sdata, cov_col = "DOSE", obsCol = "blue", simFill = "yellow")
#' p
#'
#' # Customize as a regular ggplot object
#' p +
#'  ggplot2::labs(y = "Mean dose (mg)", x = "Time since first dose (weeks)")
#'
#' # Output the underlying data and make your own plot if you prefer
#' ggKMMC(odata = odata, sdata = sdata, cov_col = "DOSE", output_data = TRUE)
#'
ggKMMC <- function(
    odata,
    sdata,
    cov_col,
    cov_data = odata,
    time_col = "TIME",
    id_col = "ID",
    iter_col = "ITER",
    grouping_col = character(0),
    bins = seq(0, max(odata[[time_col]]), length.out = 50),
    ylab = paste("Mean of", cov_col),
    nocb = TRUE,
    output_data = FALSE,
    obsCol = "black",
    simFill = PMXtte:::PMXColors_pmx_palettes(name="light"),
    ci = c(.05, .95)
){

  # Expanding covariate data over the full time grid
  expanded_cov_data <- expand_covariate_data(
    x = cov_data[cov_data[[id_col]] %in% unique(odata[[id_col]]), ,drop=FALSE],
    cov_col = cov_col,
    bins = bins,
    time_col = time_col,
    id_col = id_col,
    nocb = nocb
  )

  # Combining observed and simulated data
  survival_data <- list(
    ODATA = odata %>% dplyr::select(dplyr::all_of(c(id_col, time_col, grouping_col))),
    SDATA = sdata %>% dplyr::select(dplyr::all_of(c(id_col, time_col, grouping_col, iter_col)))
  ) %>%
    dplyr::bind_rows(.id = "DATASET")
  names(survival_data)[names(survival_data)==time_col] <- "TIMESURV"

  # Merging (time-varying) COVARIATE data and SURVIVAL data
  merged_data <- left_join(
    x = expanded_cov_data,
    y = survival_data,
    by = "ID"
  )

  # Filtering: removing covariate data that happen after the observed/simulated survival time
  merged_data <- merged_data[merged_data[[time_col]] <= merged_data[["TIMESURV"]], , drop = FALSE]

  # Summarizing
  ggKMMCdata <- merged_data %>%
    dplyr::group_by(across(all_of(c('DATASET', grouping_col, time_col, iter_col)))) %>%
    dplyr::summarise(
      MEAN = mean(.data[[cov_col]]), # Calculate the covariate mean at each time
      .groups = "drop_last"
    ) %>% # Data now grouped by DATASET (obs/sim), GROUPING columns and TIME
    dplyr::summarise(
      lo = stats::quantile(.data$MEAN, ci[1], names = FALSE),
      med = stats::quantile(.data$MEAN, 0.50, names = FALSE),
      up = stats::quantile(.data$MEAN, ci[2], names = FALSE),
      n = n(),
      .groups = "drop"
    )

  if(output_data){
    return(ggKMMCdata)
  }

  obs_guide_name <- "Observed mean"
  sim_guide_name <- paste0("Simulated ",100*diff(ci),"% CI")

  p <- ggKMMCdata %>%
    ggplot(aes(x = .data[[time_col]])) +
    geom_ribbon(
      aes(ymin = .data$lo, ymax = .data$up, fill = sim_guide_name),
      data = ggKMMCdata %>% filter(.data$DATASET == "SDATA")
    ) +
    geom_line(
      aes(y = .data$med, col = obs_guide_name),
      data = ggKMMCdata %>% filter(.data$DATASET == "ODATA")
    ) +
    labs(
      y = ylab
    ) +
    scale_color_manual(name = NULL, values = obsCol) +
    scale_fill_manual(name = NULL, values = simFill) +
    theme(
      legend.position = "top"
    )

  if(length(grouping_col) > 0){
    p <- p +
      facet_wrap(
        stats::as.formula(paste("~", paste0(grouping_col, collapse = "+"))),
        scales = "free_y"
        )
  }

  p
}



derive_stepfun <- function(
    time,
    cov,
    nocb = TRUE
){
  stats::stepfun(
    x = time,
    y = c(cov[1], cov),
    f = as.numeric(nocb) #0 = locf 1 = nocb
  )
}

expand_covariate_data <- function(
    x,
    cov_col,
    bins,
    time_col = "TIME",
    id_col = "ID",
    nocb = TRUE
){
  timegrid_data <- tidyr::expand_grid(
    unique(x[[id_col]]), #ID
    bins # TIME
  ) %>%
    rlang::set_names(c(id_col, time_col))
  # a data.frame with columns: ID and TIME,
  # one record per TIME defined in "bins" for each ID

  stepfun_data <- x %>%
    dplyr::group_by(dplyr::across(all_of(id_col))) %>%
    dplyr::summarise(
      STEPFUN = list(derive_stepfun(
        time = .data[[time_col]],
        cov = .data[[cov_col]],
        nocb = nocb))
    ) %>%
    dplyr::ungroup()
  # a data.frame with two columns: ID and STEPFUN
  # one record per ID, one STEPFUN per ID

  ans <- dplyr::left_join(
    x = timegrid_data,
    y = stepfun_data,
    by = id_col
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      tempcov = do.call(what = STEPFUN, args = list(.data[[time_col]])),
      STEPFUN = NULL # remove STEPFUN
    ) %>%
    dplyr::ungroup()
  names(ans)[names(ans)=="tempcov"] <- cov_col
  ans
}




