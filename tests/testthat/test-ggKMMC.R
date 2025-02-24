test_that("stepfun works", {
  dat <- data.frame(
    TI = c(0,   24,  24,  72),
    DO = c(100, 200, 300, 400)
  )
  newtimes <- c(-1, 0, 1, 23, 24, 25, 71, 72, 73)

  #stats::stepfun works as intended
  SFUN <- stats::stepfun(
    x = dat$TI,
    y = c(0,dat$DO),
    f = 1 #0 = locf 1 = nocb
  )

  expect_equal(
    do.call(stats::stepfun(
      x = dat$TI,
      y = c(0,dat$DO),
      f = 1 #0 = locf 1 = nocb
    ), list(newtimes)),
    c(0, 100, 200,    200, 300, 400,   400, 400, 400)
  )

  expect_equal(
    do.call(stats::stepfun(
      x = dat$TI,
      y = c(0,dat$DO),
      f = 0 #0 = locf 1 = nocb
    ), list(newtimes)),
    c(0, 100, 100,    100, 300, 300,   300, 400, 400)
  )
  # Two values at time 24: in the output vector, it is always 300mg
  # Takes the second value



  # derive_stepfun() works

  one_fun <- derive_stepfun(time = dat$TI, cov = dat$DO)
  expect_equal(
    derive_stepfun(time = dat$TI, cov = dat$DO, nocb = TRUE),
    one_fun
  )
  expect_equal(
    do.call(one_fun, list(newtimes)),
    c(100, 100, 200,    200, 300, 400,   400, 400, 400)
  )

  one_fun <- derive_stepfun(time = dat$TI, cov = dat$DO, nocb = FALSE)
  expect_equal(
    do.call(one_fun, list(newtimes)),
    c(100, 100, 100,    100, 300, 300,   300, 400, 400)
  )

  dat2 <- data.frame(
    ID = c(3,3,3,1,1,1,2,2,2),
    TIME = c(0,24,48,  0,47,72,  0,24,48),
    COV1 = c(1,2,3,    4,5,6,    7,7,7),
    COV2 = 10
    )

  dat2$TIMEbis <- dat2$TIME
  dat2$IDbis <- dat2$ID

  # expand_covariate_data
  ans1 <- expand_covariate_data(x = dat2, cov_col = "COV1", bins = seq(0,84,12))
  expect_equal(
    ans1$ID, rep(c(3,1,2), each = 8)
  )
  expect_equal(
    ans1$TIME, rep(seq(0,84,12), 3)
  )
  expect_equal(
    ans1$COV1, c(
      c(1, 2, 2, 3, 3, 3, 3, 3),
      c(4, 5, 5, 5, 6, 6, 6, 6),
      rep(7, 8)
      )
  )

  ans1 <- expand_covariate_data(x = dat2, cov_col = "COV2", bins = seq(12,36,12))
  expect_equal(ans1$COV2, rep(10, 3*3))

  ans1 <- expand_covariate_data(x = dat2, cov_col = "COV2", bins = seq(0,84,12),
                        id_col = "IDbis", time_col = "TIMEbis")
  expect_named(ans1, c("IDbis", "TIMEbis", "COV2"))

})


test_that("ggKMMC works", {
  odat <- data.frame(ID = c(1,2,3), TIME = c(24, 48, 96), COV = c(100, 200, 300))
  sdat  <- data.frame(
    ID = rep(c(1,2,3), 3), COV = rep(c(100, 200, 300), 3),
    ITER = rep(c(1,2,3), each =3),
    TIME = c(5, 45, 74, 70, 59, 93, 13, 90, 96)
    )

  ans <- ggKMMC(odata = odat, sdata = sdat, cov_col = "COV",
                output_data = TRUE, bins = c(0,24,48,96),
                ci = c(0,1))

  expect_equal(
    filter(ans, DATASET == "ODATA")$med, c(200, 200, 250, 300)
  )

  expect_equal(
    filter(ans, DATASET == "SDATA")$up, c(200, 250, 300, 300)
  )
  expect_equal(
    filter(ans, DATASET == "SDATA")$lo, c(200, 200, 200, 300)
  )
  expect_equal(
    filter(ans, DATASET == "SDATA")$med, c(200, 250, 250, 300)
  )
  expect_equal(
    filter(ans, DATASET == "SDATA")$n, c(3, 3, 3, 1)
  )
})
