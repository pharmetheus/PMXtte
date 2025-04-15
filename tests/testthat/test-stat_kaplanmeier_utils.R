# Setup for Kaplan-Meier testing
da <- data.frame(
  x = c(0,1,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1),
  time  = seq(0, by = 24, length.out = 17)
)
da[1,2] <- 1
fit <- survival::survfit(survival::Surv(time, x) ~ 1, da)
sufit <- summary(fit)

testthat::test_that("kpm functions work", {

  # Checker works
  expect_error(check_tteDV(x = c(0,1,0,1)), NA)
  expect_error(check_tteDV(x = c(FALSE, TRUE)), NA)
  expect_error(check_tteDV(x = c("0","1","0","1")), NA)
  expect_error(check_tteDV(x = c(0,1,NA)), "x must be coercible to a numeric vector of values `0` \\(censoring\\) or `1` \\(event\\)")
  expect_error(check_tteDV(x = factor(c(0,1))), "x must be coercible to a numeric vector of values `0` \\(censoring\\) or `1` \\(event\\)")

  timesubset <- da$time %in% sufit$time

  expect_equal(
    kpm(da$x)[timesubset], sufit$surv
  )
  expect_equal(
    kpm_se(da$x)[timesubset], sufit$std.err
  )
  CI <- kpm_ci(x = da$x, width = .95)
  expect_equal(CI[,1][timesubset], sufit$lower)
  expect_equal(CI[,2][timesubset], sufit$upper)

  expect_equal(
    atrisk(da$time)[timesubset], sufit$n.risk
  )

})

testthat::test_that("kpm_stepfun functions work", {

  KPM <- kpm_stepfun(time = da$time, dv = da$x)
  KPMCI <- kpm_ci_stepfun(time = da$time, dv = da$x)
  ATRISK <- atrisk_stepfun(time = da$time)
  outtimes <- c(0, 50, 96, 200)

  expect_equal(
    KPM(v = outtimes),
    c(1, sufit$surv[sufit$time %in%c(48, 96, 192)])
  )
  expect_equal(
    KPM(v = sufit$time),
    sufit$surv
  )

  expect_equal(
    KPMCI(v = outtimes)[,1],
    c(1, sufit$lower[sufit$time %in%c(48, 96, 192)])
  )
  expect_equal(
    KPMCI(v = sufit$time)[,1],
    sufit$lower
  )

  expect_equal(
    KPMCI(v = outtimes)[,2],
    c(1, sufit$upper[sufit$time %in%c(48, 96, 192)])
  )
  expect_equal(
    KPMCI(v = sufit$time)[,2],
    sufit$upper
  )

  expect_equal(
    ATRISK(v = outtimes),
    c(17, 14, 13, 8)
  )
  expect_equal(
    ATRISK(v = sufit$time),
    sufit$n.risk
  )

  # Reordering is correct
  da2 <- dplyr::slice_sample(da, n = nrow(da))

  expect_equal(
    kpm_stepfun(time = da2$time, dv = da2$x),
    kpm_stepfun(time = da$time, dv = da$x)
  )
  expect_equal(
    kpm_ci_stepfun(time = da2$time, dv = da2$x),
    kpm_ci_stepfun(time = da$time, dv = da$x)
  )
  expect_equal(
    atrisk_stepfun(time = da2$time),
    atrisk_stepfun(time = da$time)
  )
})

testthat::test_that("medsurvtime works", {
  expect_equal(medsurvtime(time = c(0,1,2), surv = c(1,0.5,0)), 1)
  expect_equal(medsurvtime(time = c(0,1,2,3), surv = c(1,.75, 0.25,0)), 2)
  expect_equal(medsurvtime(time = c(0,1), surv = c(1,.75)), NA_real_)
  expect_equal(medsurvtime(time = c(0,1), surv = c(0.25,.05)), 0)
  expect_equal(medsurvtime(time = c(0,1), surv = c(0.25,.05)), 0)

  #reorder is ok
  expect_equal(medsurvtime(time = c(1,0,3,2), surv = c(.75,1,0, 0.25)), 2)

  #lengths are checked
  expect_error(medsurvtime(time = c(0,1, 2), surv = c(0.25,.05)), "time and surv must be of the same length")
})


testthat::test_that("explicit_step works", {
  dat <- data.frame(
    time = c(0, 24, 48, 48, 72, 96),
    col1 = c(1, .9, .6, .5, .5, .3),
    col2 = c(1, .8, .7, .6, .5,  0),
    col3 = c(1,  1,  1,  1,  1,  1)
  )

  ans <- data.frame(
    time = c(0, 0, 24, 24, 48, 48, 48, 48, 72, 72, 96, 96),
    col1 = c(1, 1,  1, .9, .9, .6, .6, .5, .5, .5, .5, .3),
    col2 = c(1, 1,  1, .8, .8, .7, .7, .6, .6, .5, .5,  0),
    col2 = c(1, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1)
  )

  # can expand one or multiple colums with "toexpand"
  expect_equal(
    explicit_step(dat, toexpand = "col1"),
    ans[,c("time", "col1")]
  )

  expect_equal(
    explicit_step(dat, toexpand = c("col1", "col2")),
    ans[,c("time", "col1", "col2")]
  )

  # timevar argument works
  dat$time2 <- dat$time
  expect_equal(
    explicit_step(dat, toexpand = "col1", timevar= "time2"),
    data.frame(time2 = ans$time, col1 = ans$col1)
  )

  # deals with zero-row tables
  expect_equal(
    explicit_step(data = data.frame(time = numeric(0), col1 = numeric(0)), toexpand = 'col1'),
    data.frame(time = numeric(0), col1 = numeric(0))
  )
})

testthat::test_that("pretty_pval works", {

  p <-  c(10, 1, 0.3, 0.123456789, 0.0000001)

  expect_equal(
    pretty_pval(p),
    c("p=10", "p=1", "p=0.3", "p=0.123", "p<0.001")
  )
  # same as default
  expect_equal(
    pretty_pval(p, digits = 3, cutoff = 0.001),
    c("p=10", "p=1", "p=0.3", "p=0.123", "p<0.001")
  )
  # more digits
  expect_equal(
    pretty_pval(p, digits = 4),
    c("p=10", "p=1", "p=0.3", "p=0.1235", "p<0.001")
  )

  # less digits
  expect_equal(
    pretty_pval(p, digits = 2),
    c("p=10", "p=1", "p=0.3", "p=0.12", "p<0.001")
  )
  # deeper cutoff
  expect_equal(
    pretty_pval(p, cutoff = 0.0001),
    c("p=10", "p=1", "p=0.3", "p=0.123", "p<0.0001")
  )
})

testthat::test_that("width functions work", {
  expect_equal(
    width2z(.95), 1.96, tolerance = .01
  )
  expect_equal(
    width2z(.90), 1.64, tolerance = .01
  )

  expect_equal(
    width2bounds(.95),
    c(0.025, 0.975)
  )
  expect_equal(
    width2bounds(.90),
    c(0.05, 0.95)
  )
})

