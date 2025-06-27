#' Kaplan-Meier estimator (low-level implementation)
#' @name kpm
#' @param x a vector of numeric, with possible values of `0` (censoring) or `1` (event), or coercible to this definition.
#' @param width a numeric, width of the confidence interval.
#'
#' @return
#'   \item{`kpm()`}{a vector of numeric, Kaplan-Meier estimator of the survival}
#'   \item{`kpm_se()`}{a vector of numeric, standard error associated to the
#'   Kaplan-Meier estimator of the survival, as returned by `survival::survfit()`}
#'   \item{`kpm_ci()`}{a two-column numerical matrix, confidence interval
#'   associated to the Kaplan-Meier estimator of the survival, as returned by
#'   `survival::survfit()` with default settings.}
#'
#' @export
#'
#' @examples
#' x <- c(1,1,1,0,1,0)
#' kpm(x)
#' kpm_se(x)
#' kpm_ci(x, width = .95)
#'
#' # Errors if x does not have a valid format
#' \dontrun{
#' kpm(c(1,2,1,2,1))
#' kpm(c("Event", "No event"))
#' }
kpm <- function(x){
  check_tteDV(x)
  atrisk <- seq.int(from = length(x), to = 1)
  # Vanilla Kaplan-Meier estimator
  cumprod((atrisk-x) / atrisk)
}

#' @rdname kpm
#' @export
kpm_se <- function(x){
  # In the literature, the Greenwood estimator is often reported as
  # VAR = SURV^2 * SUM (di / (ni * (ni-di)))
  # The summary(survfit()) reports the following:
  # std.error    = sqrt(VAR) = sqrt(SURV^2 * SUM (di / (ni * (ni-di))))
  atrisk <- seq.int(from = length(x), to = 1)
  surv <- kpm(x)
  surv * sqrt(cumsum(x/(atrisk*(atrisk-x))))
}

#' @rdname kpm
#' @export
kpm_ci <- function(x, width = .95){
  # In the literature, the Greenwood estimator is often reported as
  # VAR = SURV^2 * SUM (di / (ni * (ni-di)))
  # The summary(survfit()) reports the following:
  # lower 95% CI = SURV + z * SUM (di / (ni * (ni-di))))
  # upper 95% CI = SURV - z * SUM (di / (ni * (ni-di))))

  atrisk <- seq.int(from = length(x), to = 1)
  surv <- kpm(x)
  se <- sqrt(cumsum(x/(atrisk*(atrisk-x))))
  logsurv <- ifelse(surv == 0, NA_real_, log(surv))
  z <- width2z(width)
  ans <- matrix(c(
    pmax(exp(logsurv - z * se), 0), # lower bound
    pmin(exp(logsurv + z * se), 1)  # upper bound
  ),
  ncol = 2
  )
  ans
}

check_tteDV <- function(x){
  if(length(setdiff(as.double(x), c(0,1)))>0) {
    stop("x must be coercible to a numeric vector of values `0` (censoring) or `1` (event)")
  }
}






#' Kaplan-Meier step functions
#' @name kpm_stepfun
#'
#' @description
#'
#' The Kaplan-Meier estimator only provides an estimation of the survival at the
#' times available in the data. Often, we want to interpolate the survival at
#' any time points, with the following rules:
#'
#' * Survival at time 0 is equal to 1.
#'
#' * Survival between two time points is equal to the survival at the
#'   previous time point (the survival value is carried forward).
#'
#' The functions documented here create step functions that reproduces these
#' interpolation rules, and can predict survival at any provided time.
#'
#' @param time a vector of non-missing numeric values, time of the observation
#' @param dv a vector of non-missing numeric values, type of observation
#' (`0` (censoring) or `1` (event))
#' @param cuminc a logical. If `TRUE`, calculates the cumulative incidence of
#' events. If `FALSE` (the default), the survival function.
#' @param width a numeric, width of the confidence interval.
#'
#' @return step functions (see [stats::stepfun]):
#'   \item{`kpm_stepfun()`}{a step function that calculates the survival
#'   (or cumulative incidence) as a function of time according to the
#'   Kaplan-Meier estimator of survival.}
#'   \item{`kpm_ci_stepfun()`}{a step function that calculates the confidence
#'   interval of the survival (or cumulative incidence) as a function of time
#'   according to the Kaplan-Meier estimator of survival and the Greenwood's
#'   formula.}
#'   \item{`kpm_atrisk()`}{a step function that calculates the number of
#'   subjects at risk as a function of time.}
#' @export
#'
#' @examples
#' # Times do not have to be sorted
#' # (as in a regular data set where data are stored per subject identifier)
#'
#' data <- data.frame(
#'   TIME = c(24, 5, 58, 24, 12, 36, 24),
#'   DV   = c( 1, 1,  0,  1,  0,  1,  1)
#' )
#'
#' KPM <- kpm_stepfun(time = data$TIME, dv = data$DV)
#' KPMCI <- kpm_ci_stepfun(time = data$TIME, dv = data$DV)
#' ATRISK <- atrisk_stepfun(time = data$TIME) # no argument `dv`
#' # KPM, KPMCI and ATRISK are functions that only takes one argument
#'
#' outtimes <- seq(0, 60, 12) # Times we want the survival calculated
#'
#' KPM(outtimes)
#' KPMCI(outtimes)
#' ATRISK(outtimes)
#'
kpm_stepfun <- function(time, dv, cuminc = FALSE){
  dv   <- dv[order(time, -dv)]
  time <- time[order(time, -dv)]
  surv <- kpm(dv)
  xval <- c(0, time)
  yval <- c(1, surv) # before the first time of event, the survival = 1
  if(cuminc){
    yval <- 1 - yval # reverse the survival to obtain cumulative incidence
  }

  stats::approxfun(
    x = xval,
    y = yval,
    method = "constant",
    ties = "ordered",
    rule = 2,
    f = 0, #carrying forward
    na.rm = FALSE,
  )
}

#' @rdname kpm_stepfun
#' @export
kpm_ci_stepfun <- function(time, dv, cuminc = FALSE, width = .95){
  dv   <- dv[order(time, -dv)]
  time <- time[order(time, -dv)]
  survs <- kpm_ci(dv, width = width)
  xval  <- c(0, time)
  yvals <- rbind(c(1,1), survs) # before the first time of event, the survival = 1

  if(cuminc){
    yvals <- 1 - yvals # reverse the survival to obtain cumulative incidence
  }

  lofun <- stats::approxfun(
    x = xval,
    y = yvals[,1],
    method = "constant",
    ties = "ordered",
    rule = 2,
    f = 0, #carrying forward
    na.rm = FALSE
  )

  upfun <- stats::approxfun(
    x = xval,
    y = yvals[,2],
    method = "constant",
    ties = "ordered",
    rule = 2,
    f = 0, #carrying forward
    na.rm = FALSE
  )

  function(v){
    matrix(
      c(lofun(v), upfun(v)),
      ncol = 2
    )
  }
}

atrisk <- function(time){
  seq.int(from = length(time), to = 1)
}

#' @rdname kpm_stepfun
#' @export
atrisk_stepfun <- function(time){
  time <- time[order(time)]
  stats::stepfun(
    x = time,
    y = c(atrisk(time), 0),
    right = TRUE,
    f = 1 # carrying backward
  )
}



medsurvtime <- function(time, surv){
  if(length(time)!=length(surv)){
    stop("time and surv must be of the same length.")
  }
  surv <- surv[order(time)]
  time <- time[order(time)]
  ans <- time[surv<=.5][1]
  ans
}


explicit_step <- function(data, toexpand, timevar = "time"){
  or <- order(data[[timevar]]) # reorder data by time
  ans <- data.frame(time = rep(data[[timevar]][or], each = 2)) # duplicate each row
  colnames(ans) <- timevar
  for(i in toexpand){
    if(length(data[[i]]) < 1){
      # Safety if data has zero row
      ans[[i]] <- numeric(0)
    } else {
      ans[[i]] <- c(
        data[[i]][or][1], # first value
        utils::head(rep(data[[i]][or], each = 2), -1) # duplicate each value, withdraw the last one
      )
    }
  }
  ans
}

pretty_pval <- function(x, digits = 3, cutoff = 0.001){
  ifelse(
    x < cutoff,
    paste0("p<", format(cutoff, scientific = FALSE, trim = TRUE)),
    paste0("p=", format(round(x, digits = digits), scientific = FALSE, trim = TRUE, drop0trailing=TRUE))
  )
}


width2z <- function(width = .95){
  if(length(width)!=1 || width >= 1 || width <=0) stop("invalid width. should be within ]0;1[")
  z <- qnorm(1-(1-width)/2)
  z
}
# width2z(.95)
# width2z(.90)


width2bounds <- function(width){
  if(length(width)!=1 || width >= 1 || width <=0) stop("invalid width. should be within ]0;1[")
  c(
    (1-width)/2,
    1-(1-width)/2
  )
}
# width2bounds(.95)
# width2bounds(.90)


