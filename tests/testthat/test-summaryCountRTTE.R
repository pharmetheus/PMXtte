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

test_that("factor_and_lump() works", {
  num <- sample(0:15, size = 100, replace = TRUE)
  num[1:16] <- 0:15 # make sure all values are sampled...
  num[num==1] <- 3 # ... except 1

  ans0 <- num %>% factor_and_lump()
  tab0 <- table(ans0)

  expect_equal(levels(ans0), as.character(0:15))

  ans <- factor_and_lump(num, 99)
  expect_equal(ans, ans0)

  ans <- factor_and_lump(num, 11)
  expect_equal(levels(ans), c(0:10, "11 or more"))
  tab <- table(ans)

  expect_equal(
    sum(tab["11 or more"]),
    sum(tab0[as.character(c(11:15))])
  )

  expect_error(factor_and_lump(num, 0), "Cannot lump counts")

  ans <- summaryCountRTTE(ttedata, asList = TRUE, lumpCount = 3)
  expect_named(ans, c("0", "1", "2", "3 or more", "subjects", "stratlvl"))

  ttedata2 <- ttedata %>% mutate(EVCOUNT = ifelse(EVCOUNT==4|(EVCOUNT == 2 & STUDYIDN == 1), 3, EVCOUNT))
  ans <- summaryCountRTTE(ttedata2, innerLevel = "DOSEN", outerLevel = "STUDYIDN", asList = TRUE)
  expect_equal(ans[[2]][1,"2",drop=TRUE], 0)
  expect_equal(ans[[2]][ ,"4",drop=TRUE], c(0,0))

  ans <- summaryCountRTTE(ttedata2, innerLevel="STUDYIDN", asList = TRUE, dropCount0 = TRUE)
  expect_null(ans[[2]][["4"]])
})
