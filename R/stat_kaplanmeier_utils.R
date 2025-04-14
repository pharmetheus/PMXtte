explicit_step <- function(data, toexpand, timevar = "time"){
  or <- order(data[[timevar]])
  ans <- data.frame(time = rep(data[[timevar]][or], each = 2))
  colnames(ans) <- timevar
  for(i in toexpand){
    ans[[i]] <- c(data[[i]][or][1], head(rep(data[[i]][or], each = 2), -1))
  }
  ans
}

kpm <- function(x){
  atrisk <- seq.int(from = length(x), to = 1)
  cumprod((atrisk-x) / atrisk)
}

kpm_se <- function(x){
  # In the literature, the Greenwood estimator is often reported as
  # VAR = SURV^2 * SUM (di / (ni * (ni-di)))
  # The summary(survfit()) reports the following:
  # std.error    = sqrt(VAR) = sqrt(SURV^2 * SUM (di / (ni * (ni-di))))
  atrisk <- seq.int(from = length(x), to = 1)
  surv <- kpm(x)
  surv * sqrt(cumsum(x/(atrisk*(atrisk-x))))
}


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
  matrix(c(
    pmax(exp(logsurv - z * se), 0),
    pmin(exp(logsurv + z * se), 1)
  ),
  ncol = 2
  )
}



kpm_stepfun <- function(time, dv, cuminc = FALSE){
  dv   <- dv[order(time)]
  time <- time[order(time)]
  surv <- kpm(dv)
  yval <- c(1, surv) # before the first time of event, the survival = 1
  if(cuminc){
    yval <- 1 - yval # reverse the survival to obtain cumulative incidence
  }

  stats::stepfun(
    x = time,
    y = yval,
    f = 0 #carrying forward
  )
}

kpm_ci_stepfun <- function(time, dv, cuminc = FALSE, width = .95){
  dv   <- dv[order(time)]
  time <- time[order(time)]
  survs <- kpm_ci(dv, width = width)
  yvals <- rbind(c(1,1), survs) # before the first time of event, the survival = 1

  if(cuminc){
    yvals <- 1 - yvals # reverse the survival to obtain cumulative incidence
  }

  upfun <- stats::stepfun(
    x = time,
    y = yvals[,1],
    f = 0 #carrying forward
  )

  lofun <- stats::stepfun(
    x = time,
    y = yvals[,1],
    f = 0 #carrying forward
  )

  function(v){
    matrix(
      c(upfun(v), lofun(v)),
      ncol = 2
    )
  }
}

atrisk_stepfun <- function(time){
  time <- time[order(time)]
  atrisk <- seq.int(from = length(time), to = 1)
  stats::stepfun(
    x = time,
    y = c(length(time), atrisk),
    f = 0 #carrying forward
  )
}

medsurvtime <- function(time, surv){
  surv <- surv[order(time)]
  time <- time[order(time)]
  ans <- time[surv<=.5][1]
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
