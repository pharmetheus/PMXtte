rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)

test_that("summaryFollowUpTime() works", {
  ans <- summaryFollowUpTime(rttedata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             asList       = TRUE
  )

  expect_equal(ans[[1]]$nEvent, 869)
  expect_equal(ans[[3]]$nEvent / ans[[3]]$TotalFollowUpTime, ans[[3]]$AnnualizedEventRate, tolerance = 0.1)


})

test_that("digits works", {
  ans <- summaryFollowUpTime(rttedata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             digits       = 0,
                             digits_rate  = 2
  ) %>% capture.output()
  expect_match(ans[12], "189&0.72\\\\tabular")

  ans <- summaryFollowUpTime(rttedata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             digits       = 1,
                             digits_rate  = 2
  ) %>% capture.output()

  expect_match(ans[12], "189.1&0.72\\\\tabular")

  ans <- summaryFollowUpTime(rttedata,
                             outerLevel   ="STUDYIDN" ,
                             outerLabel   = "Study",
                             innerLevel   = "DOSEN",
                             innerLabel   = "Dose",
                             digits       = 1,
                             digits_rate  = 3
  ) %>% capture.output()

  expect_match(ans[12], "189.1&0.724\\\\tabular")



})
