rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)

test_that("makeSummaryTableTTE works with RTTE", {
  ans <- makeSummaryTableTTE(rttedata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             asList = TRUE)
  expect_equal(ans[[3]]$subjects, c(rep(200, 4), rep(150, 3)))
  expect_equal(ans[[3]]$nObs, c(197, 137, 109, 107, 112, 109, 98))
  expect_equal(ans[[3]]$avnObs, rep("", 7))

  expect_true(ans[[1]]$avnObs == "")

  ans <- makeSummaryTableTTE(rttedata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose"
  ) %>%
    capture.output()
  expect_false(stringr::str_detect(ans[27], "\\{c\\}Proportion of "))

  # Can force avn
  ans <-  makeSummaryTableTTE(rttedata,
                              outerLevel   ="STUDYIDN" ,
                              outerLabel   = "Study",
                              innerLevel   = "DOSEN",
                              innerLabel   = "Dose",
                              showAvnObs = TRUE,
                              asList = TRUE)
  expect_equal(as.double(ans[[3]]$avnObs), ans[[3]]$nObs / ans[[3]]$subjects, tolerance = .001)
  ans <- makeSummaryTableTTE(rttedata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             showAvnObs = TRUE
  ) %>%
    capture.output()
  expect_true(stringr::str_detect(ans[27], "\\{c\\}Proportion of "))

})

test_that("makeSummaryTableTTE works with TTE", {
  tt1edata <- rttedata %>% filter_xth_event(1)
  ans <- makeSummaryTableTTE(tt1edata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             asList = TRUE)
  expect_equal(ans[[3]]$subjects, c(rep(200, 4), rep(150, 3)))
  expect_equal(ans[[3]]$nObs, c(108, 86, 74, 75 ,70, 68, 56))
  expect_equal(as.double(ans[[3]]$avnObs), ans[[3]]$nObs / ans[[3]]$subjects, tolerance = .001)


  ans <- makeSummaryTableTTE(tt1edata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose"
  ) %>%
    capture.output()
  expect_true(stringr::str_detect(ans[27], "\\{c\\}Proportion of "))
})
