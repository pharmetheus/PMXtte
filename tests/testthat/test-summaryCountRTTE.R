ttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
ttedata <- dplyr::filter(ttedata, EVID == 0, TYPE == 2)
test_that("summaryCountRTTE() works", {
  ans <- summaryCountRTTE(ttedata,
                          outerLevel   = "DOSEN",
                          outerLabel   = "Dose",
                          innerLevel   = "STUDYIDN",
                          innerLabel   = "Study",
                          asList = TRUE)

  expect_type(ans, "list")
  expect_length(ans, 3)
  expect_named(ans[[1]], c(as.character(0:6), "subjects", 'stratlvl',"DOSEN", "STUDYIDN"))
})

test_that("inner/outer levels work", {
  ans <- summaryCountRTTE(ttedata,
                          outerLevel   = "DOSEN",
                          outerLabel   = "Dose",
                          asList = TRUE)

  expect_type(ans, "list")
  expect_length(ans, 2)
  expect_true("DOSEN" %in% names(ans[[2]]))

  ans <- summaryCountRTTE(ttedata,
                          innerLevel   = "STUDYIDN",
                          innerLabel   = "Study",
                          asList = TRUE)

  expect_type(ans, "list")
  expect_length(ans, 2)
  expect_true("STUDYIDN" %in% names(ans[[2]]))

  ans <- summaryCountRTTE(ttedata,
                          asList = TRUE)

  expect_type(ans, "list")
  expect_false(inherits(ans, "data.frame"))
  expect_named(ans, c(as.character(0:6), "subjects", 'stratlvl'))
})

test_that("lump_drop_columns() works", {

  df <- structure(list(subjects = c(200L, 150L, 200L, 150L, 350L, 200L, 150L, 350L, 200L, 1250L),
                       `0` = c(92L, 80L, 114L, 82L, 196L, 126L, 94L, 220L, 125L, 713L),
                       `1` = c(164L, 114L, 141L, 111L,  252L, 120L, 86L, 206L, 124L, 860L),
                       `2` = c(85L, 43L, 51L, 39L, 90L, 44L, 41L, 85L, 43L, 346L),
                       `3` = c(28L, 14L, 17L, 16L, 33L,  15L, 19L, 34L, 14L, 123L),
                       `4` = c(14, 6, 7, 7, 14, 2, 5, 7, 0, 41),
                       `5` = c(10, 3, 5, 1, 6, 0, 1, 1, 0, 20),
                       `6` = c(4, 2,  2, 2, 4, 0, 2, 2, 0, 12)),
                  row.names = c(NA, -10L), class = "data.frame")


  ans <- lump_drop_columns(df)
  expect_equal(ans, df)

  ans <- lump_drop_columns(df, lump = 4)
  expect_named(ans, c("subjects", "0", "1", "2", "3", "4 or more"))
  expect_equal(ans[["4 or more"]], rowSums(df[c("4", "5", "6")]))

  df[["1"]] <- 0
  ans <- lump_drop_columns(df, dropCount0 = TRUE)
  expect_null(ans[["1"]])
})
