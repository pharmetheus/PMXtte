PMXRenv::library.unqualified("vdiffr")
rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)

test_that("plotKaplanMeier work", {
  x <- c('x','y')
  expect_error(plotKaplanMeier(data = x),'Input is not a dataframe')

  tte1data <- rttedata %>% filter_xth_event(1)
  expect_error(plotKaplanMeier(tte1data, time_col = 'times'),
               'one or more specified columns do not exist in the dataframe')
  expect_error(plotKaplanMeier(tte1data, cov_col = 'nocovariate'),
               'covariate column does not exist in the dataframe')
  tte1data <- as.data.frame(tte1data)

  svg()
  p1 <-  suppressWarnings(plotKaplanMeier(tte1data))
  p2 <- plotKaplanMeier(tte1data, cov_col = "DOSEN")
  p3 <- plotKaplanMeier(tte1data, cov_col = "DOSEN", facet.by = 'SEXN')
  p4 <- plotKaplanMeier(tte1data, cov_col = 'DOSEN', facet.by = 'SEXN', panel.labs = list('SEXN' = c('male','female')))
  p5 <- plotKaplanMeier(tte1data, cov_col = "DOSEN", surv.median.line.legend = FALSE)
  dev.off()

  vdiffr::expect_doppelganger("Kaplan Meier plot with default options", p1)
  vdiffr::expect_doppelganger("Kaplan Meier plot by dose", p2)
  vdiffr::expect_doppelganger("Kaplan Meier plot by dose faceted by SEX", p3)
  vdiffr::expect_doppelganger("Change panel labs of facets", p4)
  vdiffr::expect_doppelganger("Median line not included in legend", p5)

})

