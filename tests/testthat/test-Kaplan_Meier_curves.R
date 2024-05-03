PMXRenv::library.unqualified("vdiffr")

test_that("Kaplan Meier curves work", {
  x <- c('x','y')
  expect_error(Kaplan_Meier_curves(data = x),'Input is not a dataframe')

  data_frame <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv',
                                     package= 'PMXtte'),na.strings=c(".","-99","NA"))
  expect_error(Kaplan_Meier_curves(data_frame, time_col = 'times'),
               'one or more specified columns do not exist in the dataframe')
  expect_error(Kaplan_Meier_curves(data_frame, cov_col = 1),
               'covariate column does not exist in the dataframe')
  RTTEdata <- data_frame %>% dplyr::filter(EVID==0&TYPE==0)
  RTTEdata <- RTTEdata %>% dplyr::distinct(ID, .keep_all = TRUE)

  svg()
  p1 <-  Kaplan_Meier_curves(RTTEdata)
  p2 <- Kaplan_Meier_curves(RTTEdata, cov_col = "DOSE")
  p3 <- Kaplan_Meier_curves(RTTEdata, cov_col = "DOSE", facet.by = 'SEX')
  dev.off()

  vdiffr::expect_doppelganger("Kaplan Meier plot with default options", p1)
  vdiffr::expect_doppelganger("Kaplan Meier plot by dose", p2)
  vdiffr::expect_doppelganger("Kaplan Meier plot by dose faceted by SEX", p3)




})

