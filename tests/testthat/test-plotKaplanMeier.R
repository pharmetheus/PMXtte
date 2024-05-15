PMXRenv::library.unqualified("vdiffr")

test_that("plotKaplanMeier work", {
  x <- c('x','y')
  expect_error(plotKaplanMeier(data = x),'Input is not a dataframe')

  data_frame <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv',
                                     package= 'PMXtte'),na.strings=c(".","-99","NA"))
  expect_error(plotKaplanMeier(data_frame, time_col = 'times'),
               'one or more specified columns do not exist in the dataframe')
  expect_error(plotKaplanMeier(data_frame, cov_col = 'nocovariate'),
               'covariate column does not exist in the dataframe')
  RTTEdata <- data_frame %>% dplyr::filter(EVID==0&TYPE==0)
  RTTEdata <- RTTEdata %>% dplyr::distinct(ID, .keep_all = TRUE)

  svg()
  p1 <-  plotKaplanMeier(RTTEdata)
  p2 <- plotKaplanMeier(RTTEdata, cov_col = "DOSE")
  p3 <- plotKaplanMeier(RTTEdata, cov_col = "DOSE", facet.by = 'SEX')
  p4 <- plotKaplanMeier(RTTEdata, cov_col = 'DOSE', facet.by = 'SEX', panel.labs = list('SEX' = c('male','female')))

  dev.off()

  vdiffr::expect_doppelganger("Kaplan Meier plot with default options", p1)
  vdiffr::expect_doppelganger("Kaplan Meier plot by dose", p2)
  vdiffr::expect_doppelganger("Kaplan Meier plot by dose faceted by SEX", p3)
  vdiffr::expect_doppelganger("Change panel labs of facets", p4)





})

