
testthat::test_that("kpm works", {
  da <- data.frame(
    x = c(0,1,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1),
    time  = seq(0, by = 24, length.out = 17)
  )

  fit <- survival::survfit(survival::Surv(time, x) ~ 1, da)
  sufit <- summary(fit)
  timesubset <- da$time %in% sufit$time

  expect_equal(
    kpm(da$x)[timesubset], sufit$surv
  )
  expect_equal(
    kpm_se(da$x)[timesubset], sufit$std.err
  )

  expect_equal(
    width2z(.95), 1.96, tolerance = .01
  )
  expect_equal(
    width2z(.90), 1.64, tolerance = .01
  )

  CI <- kpm_confint(
    surv = kpm(da$x),
    se = kpm_se(da$x)
  )

  expect_equal(CI[,1][timesubset], sufit$lower)
  expect_equal(CI[,2][timesubset], sufit$upper)

})
