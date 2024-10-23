rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)

test_that("filter_xth_event() works", {
  expect_error(
    filter_xth_event(rttedata, event_col = "DVV"),
    "event_col.*DVV not found"
  )

  expect_error(
    filter_xth_event(rttedata, evcount_col = "EVCOUNTT"),
    "evcount_col.*EVCOUNTT not found"
  )

  expect_error(
    filter_xth_event(rttedata, time_col = "TSFDX"),
    NA
  )

  expect_error(
    filter_xth_event(rttedata, event = 2, time_col = "TSFDX"),
    "time_col.*TSFDX not found"
  )

  expect_error(
    filter_xth_event(mutate(rttedata, TSLE = -99), event = 2),
    "time_since_last_event.*TSLE already exists in the data"
  )

  expect_equal(
    filter_xth_event(rttedata, 1),
    filter(rttedata, INCLFIRST==1)
  )
})
