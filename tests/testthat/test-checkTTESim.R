odata_rtte_ref <- tibble(
  ID     = c(1,4,4,4,7,7),
  DV     = c(0,1,1,0,1,0),
  TIME   = c(100, 150, 350, 400, 600, 700),
  RTTE   = c(1,1,2,3,1,2)
)

odata_tte_ref <- filter(odata_rtte_ref, RTTE==1) %>% select(-RTTE)

sdata_rtte_ref <- tibble(
  ID     = c(1,1,4,7,1,4,4,7,1,4,7,7,7),
  DV     = c(1,0,0,0,0,1,0,0,0,0,1,1,0),
  TIME   = c(50,100, 400, 700, 100, 200, 400, 700, 100, 400, 300, 500, 700),
  RTTE   = c(1,2,1,1,1,1,2,1,1,1,1,2,3),
  SUR    = signif(runif(13)),
  ICOUNT = c(1,1,2,3,4,5,5,6,7,8,9,9,9),
  ITER   = c(1,1,1,1,2,2,2,2,3,3,3,3,3),
  RAND   = signif(runif(13))
)

sdata_tte_ref <- filter(sdata_rtte_ref, RTTE==1)

test_that("checkTTESim remains silent if all is ok", {
  expect_no_warning(checkTTESim(sdata_tte_ref))
  expect_no_warning(checkTTESim(sdata_rtte_ref, rtte = TRUE))
})

test_that("checkTTESim accept custom column names", {
  sdata_rtte_ref %>%
    setNames(c("xID", "xDV", "xTIME", "xRTTE", "xSUR", "xICOUNT", "xITER", "xRAND")
    ) %>%
    checkTTESim(rtte = TRUE,
                idVar = "xID", dvVar = "xDV", timeVar = "xTIME",
                rtteVar = "xRTTE", surVar = "xSUR", iCountVar = "xICOUNT",
                iterVar = "xITER", randVar = "xRAND") %>%
    expect_no_warning()
})

test_that("checkTTESim rule 1 works", {
  sdata_rtte_wrong <- sdata_rtte_ref
  sdata_rtte_wrong$ID[sdata_rtte_wrong$ITER==2] <- c(1,1,1,4)
  expect_warning(
    checkTTESim(sdata_rtte_wrong, rtte = TRUE),
    "The number of ID within each ITER is not consistant"
  ) %>%
    suppressWarnings() # Suppress RTTE/ICOUNT warnings
  # they popup because ID/ITER are incorrect
})

test_that("checkTTESim rule 2 works", {
  sdata_rtte_wrong <- sdata_rtte_ref
  sdata_rtte_wrong$DV[3] <- 3
  expect_warning(
    checkTTESim(sdata_rtte_wrong, rtte = TRUE),
    "Some values of DV are not equal to 0 or 1."
  )
  sdata_rtte_wrong$DV[3] <- NA_real_
  expect_warning(
    checkTTESim(sdata_rtte_wrong, rtte = TRUE),
    "Some values of DV are not equal to 0 or 1."
  )
})

test_that("checkTTESim rule 3 works", {
  sdata_tte_wrong <- sdata_tte_ref
  sdata_tte_wrong$RTTE[sdata_tte_wrong$ID==7&sdata_tte_wrong$ITER==3] <- 0
  expect_warning(
    checkTTESim(sdata_tte_wrong),
    "RTTE is not computed correctly."
  )
  sdata_rtte_wrong <- sdata_rtte_ref
  sdata_rtte_wrong$RTTE[sdata_rtte_wrong$ID==7&sdata_rtte_wrong$ITER==3] <- 1
  expect_warning(
    checkTTESim(sdata_rtte_wrong, rtte = TRUE),
    "RTTE is not computed correctly."
  )
})

test_that("checkTTESim rule 4 works", {
  sdata_tte_wrong <- sdata_tte_ref
  sdata_tte_wrong$ICOUNT[sdata_tte_wrong$ID==7&sdata_tte_wrong$ITER==3] <- 8
  expect_warning(
    checkTTESim(sdata_tte_wrong),
    "ICOUNT is not computed correctly."
  )
  sdata_rtte_wrong <- sdata_rtte_ref
  sdata_rtte_wrong$ICOUNT[sdata_rtte_wrong$ID==7&sdata_rtte_wrong$ITER==3] <- c(9,10,11)
  expect_warning(
    checkTTESim(sdata_rtte_wrong, rtte = TRUE),
    "ICOUNT is not computed correctly."
  )
})

test_that("checkTTESim rule 5 works", {
  sdata_tte_wrong <- sdata_tte_ref
  sdata_tte_wrong$ID[sdata_tte_wrong$ID==7&sdata_tte_wrong$ITER==3] <- 8
  expect_warning(
    checkTTESim(sdata_tte_wrong, odata = odata_tte_ref),
    "IDs are not the same in odata and sdata"
  )
})


test_that("checkTTESim rule 6 works", {
  sdata_tte_wrong <- sdata_tte_ref %>%
    filter(!(ID == 7 & ITER== 3))
  expect_warning(
    checkTTESim(sdata_tte_wrong, odata = odata_tte_ref),
    "Dimension of the sdata is not a multiple of the odata"
  ) %>%
    suppressWarnings() # Suppress ID warnings
  # they popup because number of ID/ITER is incorrect

  sdata_rtte_wrong <- sdata_rtte_ref %>%
    filter(!(ID == 7 & ITER== 3))
  expect_warning(
    checkTTESim(sdata_rtte_wrong, odata = odata_rtte_ref, rtte = TRUE),
    "Dimension of the sdata is not a multiple of the odata"
  ) %>%
    suppressWarnings() # Suppress ID warnings
  # they popup because number of ID/ITER is incorrect
})
