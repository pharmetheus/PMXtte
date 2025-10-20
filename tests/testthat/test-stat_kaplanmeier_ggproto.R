# Setup for Kaplan-Meier testing
da <- data.frame(
  x = c(0,1,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1),
  time  = seq(0, by = 24, length.out = 17)
)
da[1,2] <- 1
fit <- survival::survfit(survival::Surv(time, x) ~ 1, da)
sufit <- summary(fit)
ggda <- data.frame(x = da$time, y = da$x)

test_that("StatKaplanMeier works", {
  ans <- StatKaplanMeier$compute_group(data = ggda, cuminc = FALSE)
  expect_equal(
    ans$x,
    c(0, ggda$x)
  )
  expect_equal(
    ans$y[-1][ggda$y==1],
    sufit$surv
  )

  # cumin = TRUE works
  ans <- StatKaplanMeier$compute_group(data = ggda, cuminc = TRUE)
  expect_equal(
    ans$x,
    c(0, ggda$x)
  )
  expect_equal(
    ans$y[-1][ggda$y==1],
    1-sufit$surv
  )

})

test_that("StatKaplanMeierCensor works", {
  ans <- StatKaplanMeierCensor$compute_group(data = ggda, cuminc = FALSE)
  expect_equal(
    ans$x,
    ggda$x[ggda$y==0]
  )
  expect_equal(
    ans$y[ans$x==1],
    1
  )
  expect_equal(
    ans$y[ans$x==72],
    sufit$surv[sufit$time==48]
  )

  # cuminc = TRUE works
  ans <- StatKaplanMeierCensor$compute_group(data = ggda, cuminc = TRUE)
  expect_equal(
    ans$x,
    ggda$x[ggda$y==0]
  )
  expect_equal(
    ans$y[ans$x==1],
    0
  )
  expect_equal(
    ans$y[ans$x==72],
    1-sufit$surv[sufit$time==48]
  )
})

test_that("StatKaplanMeierMedian works", {
  ans <- StatKaplanMeierMedian$compute_group(data = ggda)
  expect_equal(
    ans$x,
    unname(c(0, sufit$table["median"], sufit$table["median"]))
  )

  expect_equal(
    ans$y,
    c(0.5, 0.5, 0)
  )

  # returns NULL if median survival not achieved
  ggda2 <- ggda
  ggda2$y <- 0
  expect_null(
    StatKaplanMeierMedian$compute_group(data = ggda2)
  )
})

test_that("StatKaplanMeierSE works", {
  ans <- StatKaplanMeierSE$compute_group(data = ggda, level = .95, cuminc = FALSE)
  expect_s3_class(ans, "data.frame")
  expect_named(ans, c("x", "ymin", "ymax"))
  expect_equal(
    ans$x,
    c(0, 0, rep(ggda$x, each = 2))
  )
  expect_true(
    all(head(ans, -1)$ymax >= head(ans, -1)$ymin)
  )
  expect_equal(tail(ans, 1)$ymin, NA_real_)
  expect_equal(tail(ans, 1)$ymax, NA_real_)

  ansval <- ans[ans$x%in%sufit$time & duplicated(ans$x),]
  expect_equal(ansval$ymin, sufit$lower)
  expect_equal(ansval$ymax, sufit$upper)

  # level of CI works (narrower CI, numerical values not checked)
  ans2 <- StatKaplanMeierSE$compute_group(data = ggda, level = .5, cuminc = FALSE)
  expect_true(all(head(ans2, -1)$ymin >= head(ans, -1)$ymin))
  expect_true(all(head(ans2, -1)$ymax <= head(ans, -1)$ymax))

  # cuminc=TRUE works
  ans3 <- StatKaplanMeierSE$compute_group(data = ggda, level = .95, cuminc = TRUE)
  expect_equal(ans3$ymin, 1-ans$ymin)
  expect_equal(ans3$ymax, 1-ans$ymax)

})

test_that("StatKaplanMeierPval works", {
  ggda2 <- rbind(ggda, ggda, ggda)
  ggda2$strata <- rep(c(1,2,3), each = nrow(ggda))
  set.seed(123)
  ggda2$y <- round(runif(nrow(ggda2)))


  ans <- StatKaplanMeierPval$compute_panel(data = ggda2)
  expect_s3_class(ans, "data.frame")
  expect_named(ans, c("x", "y", "label"))
  expect_equal(nrow(ans), 1)

  expect_true(all(ans$x > 75, ans$x < 80))
  expect_true(all(ans$y > .18, ans$y < .22))
  # checking the format of the label, not numerical p value
  expect_true(str_detect(ans$label, "p=0\\.\\d\\d\\d"))

  # xpos and ypos are working
  ans2 <- StatKaplanMeierPval$compute_panel(data = ggda2, xpos = 99, ypos = -66)
  expect_equal(ans2$x, 99)
  expect_equal(ans2$y, -66)

  # one level in strata works
  ggda3 <- ggda2
  ggda3$strata <- 1
  expect_error(
    StatKaplanMeierPval$compute_panel(data = ggda3),
    "There is only 1 group"
  )

  # strata is character
  ggda4 <- ggda2
  ggda4$strata <- as.character(ggda4$strata)
  ans <- ggplot(ggda4)+
    stat_kaplanmeier_pval(aes(x = x, y = y, strata = strata))
  expect_equal(ggplot2::ggplot_build(ans)$data[[1]]$label, "p=0.015")
})

test_that("StatKaplanMeierSim works", {
  ggda2 <- rbind(ggda, ggda, ggda)
  ggda2$iter <- rep(c(1,2,3), each = nrow(ggda))
  set.seed(123)
  ggda2$y <- round(runif(nrow(ggda2)))

  ans <- StatKaplanMeierSim$compute_group(data = ggda2)
  expect_s3_class(ans, "data.frame")
  expect_named(ans, c("x", "ymin", "ymax"))
  expect_equal(nrow(ans), 101)

  # numerical value not checked
  expect_true(all(ans$ymin <= ans$ymax))

  #cuminc=TRUE works
  ans2 <- StatKaplanMeierSim$compute_group(data = ggda2, cuminc = TRUE)
  expect_equal(ans2$ymin, 1 - ans$ymin)
  expect_equal(ans2$ymax, 1 - ans$ymax)

  # changing the binning works
  ans2 <- StatKaplanMeierSim$compute_group(data = ggda2, n = 10, xlim = c(100, 200))
  expect_equal(nrow(ans2), 10)
  expect_equal(range(ans2$x), c(100, 200))

  # changing the level of CI works (narrower CI, numerical values not checked)
  ans2 <- StatKaplanMeierSim$compute_group(data = ggda2,level = 0.5)
  expect_true(all(ans$ymin <= ans2$ymin))
  expect_true(all(ans$ymax >= ans2$ymax))

})

test_that("StatKaplanMeierRiskTable works", {
  ggda2 <- ggda
  ggda2$y <- 1
  ans <- StatKaplanMeierRiskTable$compute_group(data = ggda2, times = c(0, 200, 400))
  expect_s3_class(ans, "data.frame")
  expect_named(ans, c("x", "y", "label"))
  expect_equal(nrow(ans), 3)
  expect_equal(ans$x, c(0, 200, 400))
  expect_equal(ans$label, c(17, 8, 0))

  expect_equal(
    ggplot_build(
      ggplot(ggda2) +
        stat_kaplanmeier_risktable(aes(x = x, y = y))
    )$data[[1]]$x,
    c(0, 100, 200, 300, 400)
  )

  expect_equal(
    ggplot_build(
      ggplot(ggda2) +
        stat_kaplanmeier_risktable(aes(x = x, y = y))+
        scale_x_continuous(limits = c(50, 250))
    )$data[[1]]$x %>% suppressWarnings(),
    c(50, 100, 150, 200, 250)
  )

  expect_equal(
    ggplot_build(
      ggplot(ggda2) +
        stat_kaplanmeier_risktable(aes(x = x, y = y))+
        scale_x_continuous(limits = c(NA, 250))
    )$data[[1]]$x %>% suppressWarnings(),
    c(0, 50, 100, 150, 200, 250)
  )

  expect_equal(
    ggplot_build(
      ggplot(ggda2) +
        stat_kaplanmeier_risktable(aes(x = x, y = y))+
        scale_x_continuous(limits = c(NA_real_, NA_real_))
    )$data[[1]]$x %>% suppressWarnings(),
    c(0, 100, 200, 300, 400)
  )

  expect_equal(
    ggplot_build(
      ggplot(ggda2) +
        stat_kaplanmeier_risktable(aes(x = x, y = y))+
        scale_x_continuous(breaks = c(10, 20, 30))
    )$data[[1]]$x %>% suppressWarnings(),
    c(10, 20, 30)
  )

  expect_equal(
    ggplot_build(
      ggplot(ggda2) +
        stat_kaplanmeier_risktable(aes(x = x, y = y))+
        scale_x_continuous(breaks = c(0, 400, 800), limits = c(0,900))
    )$data[[1]]$x %>% suppressWarnings(),
    c(0, 400, 800)
  )

  expect_equal(
    ggplot_build(
      ggplot(ggda2) +
        stat_kaplanmeier_risktable(aes(x = x, y = y))+
        scale_x_continuous(breaks = c(-10, 0), limits = c(-100, 20))
    )$data[[1]]$x %>% suppressWarnings(),
    c(-10, 0)
  )

})





