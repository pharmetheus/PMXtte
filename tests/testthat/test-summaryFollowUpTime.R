ttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
ttedata <- dplyr::filter(ttedata, EVID == 0, TYPE == 2)

test_that("summaryFollowUpTime()", {
  ans <- summaryFollowUpTime(ttedata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             asList = TRUE)

  expect_equal(ans[[1]]$nEvent, 869)
  expect_equal(ans[[3]]$nEvent / ans[[3]]$TotalFollowUpTime, ans[[3]]$AnnualizedEventRate, tolerance = 0.01)
})
