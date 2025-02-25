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

test_that("deal with missing EVCOUNT", {
  expect_equal(
    filter_xth_event(rttedata %>% select(-EVCOUNT)) %>% relocate("EVCOUNT"),
    filter_xth_event(rttedata) %>% relocate("EVCOUNT")
  )
})

test_that("preserve tibble/data.frame class", {
  input <- as_tibble(rttedata)
  ans <- filter_xth_event(input)
  expect_s3_class(ans, "tbl_df")

  input <- as.data.frame(rttedata)
  ans <- filter_xth_event(input)
  expect_false(inherits(ans ,"tbl_df"))
})

test_that("preserves groups", {
  input <- rttedata %>% group_by(DOSEN, SEXN)
  ans <- filter_xth_event(input)
  expect_equal(group_vars(input), group_vars(ans))
})

test_that("isRTTE is working", {
  expect_true(isRTTE(rttedata))
  expect_false(isRTTE(rttedata %>% filter_xth_event(1)))
  expect_false(isRTTE(rttedata %>% filter_xth_event(2)))
})
