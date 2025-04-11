
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
