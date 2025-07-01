# Setup for Kaplan-Meier testing
da <- data.frame(
  x = c(0,1,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1),
  time  = seq(0, by = 24, length.out = 17)
)
da[1,2] <- 1
fit <- survival::survfit(survival::Surv(time, x) ~ 1, da)
sufit <- summary(fit)


da2 <- data.frame(
  time = c(0, 6, 12, 24, 24, 24, 30, 36, 36, 42),
  x =    c(0, 1,  0,  0,  1,  1,  1,  0,  0,  1)
)
fit2 <- survival::survfit(survival::Surv(time, x) ~ 1, da2)
sufit2 <- summary(fit2)

da3 <- data.frame(
  time = c(0, 6, 12, 24, 24, 30, 36, 36, 42),
  x =    c(0, 1,  0,  1,  0,  1,  0,  0,  1)
)
fit3 <- survival::survfit(survival::Surv(time, x) ~ 1, da3)
sufit3 <- summary(fit3)

DATAS <- data.frame(
  REP = rep(seq_len(100), each = 20),
  X = sample(x = c(0,1), size = 20*100, replace = TRUE),
  TIME = sample(x = c(6, 12, 24, 36), size = 20*100, replace = TRUE)
) %>%
  split(~REP)







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


  order2 <- order(da2$time, -(da2$x))
  timesubset2 <-  which(da2$x[order2] == 1)[rev(!duplicated(rev(da2$time[order2][(da2$x[order2] == 1)])))]
  timesubset2atrisk <-  which(da2$x[order2] == 1)[(!duplicated(da2$time[order2][(da2$x[order2] == 1)]))]

  expect_equal(
    kpm(da2$x[order2])[timesubset2], sufit2$surv
  )
  expect_equal(
    kpm_se(da2$x[order2])[timesubset2], sufit2$std.err
  )
  CI <- kpm_ci(x = da2$x[order2], width = .95)
  expect_equal(CI[,1][timesubset2], sufit2$lower)
  expect_equal(CI[,2][timesubset2], sufit2$upper)

  expect_equal(
    atrisk(da2$time[order2])[timesubset2atrisk], sufit2$n.risk
  )



  order3 <- order(da3$time, -(da3$x))
  timesubset3 <-  which(da3$x[order3] == 1)[rev(!duplicated(rev(da3$time[order3][(da3$x[order3] == 1)])))]
  timesubset3atrisk <-  which(da3$x[order3] == 1)[(!duplicated(da3$time[order3][(da3$x[order3] == 1)]))]

  expect_equal(
    kpm(da3$x[order3])[timesubset3], sufit3$surv
  )
  expect_equal(
    kpm_se(da3$x[order3])[timesubset3], sufit3$std.err
  )
  CI <- kpm_ci(x = da3$x[order3], width = .95)
  expect_equal(CI[,1][timesubset3], sufit3$lower)
  expect_equal(CI[,2][timesubset3], sufit3$upper)

  expect_equal(
    atrisk(da3$time[order3])[timesubset3atrisk], sufit3$n.risk
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


  # On many data with tied event time/censoring
  for(i in seq_along(DATAS)){
    SUFIT <- summary(survival::survfit(survival::Surv(TIME, X) ~ 1, DATAS[[i]]))
    expect_equal(
      kpm_stepfun(time = DATAS[[i]]$TIME, dv = DATAS[[i]]$X)(SUFIT$time),
      SUFIT$surv
    )
    expect_equal(
      kpm_ci_stepfun(time = DATAS[[i]]$TIME, dv = DATAS[[i]]$X)(SUFIT$time)[,1],
      SUFIT$lower
    )
    expect_equal(
      kpm_ci_stepfun(time = DATAS[[i]]$TIME, dv = DATAS[[i]]$X)(SUFIT$time)[,2],
      SUFIT$upper
    )

  }

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

test_that("atrisk captures multiple events/censoring at same time", {
  expect_equal(
    atrisk_stepfun(rep(0, 10))(0),
    10
  )
  expect_equal(
    atrisk_stepfun(rep(c(0,24,48), each = 2))(c(0,12,24,36,48,60)),
    c(6, 4, 4, 2, 2, 0)
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

  #Results are in-line with survfit
  expect_equal(
    medsurvtime(time = da$time, surv = kpm(da$x)),
    unname(sufit$table["median"])
  )
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

