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
                              avnObsPercent = FALSE,
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
  expect_equal(ans[[3]]$avnObs[c(1, 4)], c("54.0%", "37.5%")) # checking formatting only
  ans <- makeSummaryTableTTE(tt1edata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             asList = TRUE,
                             digits = 4)
  expect_equal(ans[[3]]$avnObs[c(1, 4)], c("54.00%", "37.50%")) # checking formatting only

  ans <- makeSummaryTableTTE(tt1edata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             asList = TRUE,
                             avnObsPercent = FALSE)
  # checking numbers
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

test_that("myID, myDV work", {
  expect_equal(
    makeSummaryTableTTE(rttedata %>% rename(BLABLA = ID), myID = "BLABLA"),
    makeSummaryTableTTE(rttedata)
  )
  expect_equal(
    makeSummaryTableTTE(rttedata %>% rename(BLABLA = ID, FOO = DV), myID = "BLABLA", myDV = "FOO"),
    makeSummaryTableTTE(rttedata)
  )
})

test_that("No superfluous 'All'-row when grouping by two variables results in a single group", {
  ans <- rttedata %>%
    filter(!(STUDYIDN==1&DOSEN!=200)) %>% # 200mg is the only category in study 1
    makeSummaryTableTTE(outerLevel = "STUDYIDN", innerLevel = "DOSEN") %>%
    capture.output()
  row_to_check <- str_which(ans, "200&200&109")+1
  expect_false(str_detect(ans[row_to_check], "All")) # No "All line" after
  expect_true(str_detect(ans[row_to_check], "hline")) # But new line (hline) instead
})
