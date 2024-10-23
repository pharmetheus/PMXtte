PMXRenv::library.unqualified("vdiffr")
rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)

test_that('similar plots', {
  # Load observed data
  odata <- rttedata %>%
    dplyr::filter(STUDYIDN == 1) %>%
    dplyr::mutate(
      TIME = TSFDW,
      SEXNF = factor(SEXN, levels = c(1,2), c("Male", "Female"))
    )

  # Load simulated data
  sdata <- readRDS(system.file('extdata/vpcdat.rds', package= 'PMXtte'))
  sdata <- sdata %>%
    dplyr::mutate(
      TIME = TIME / (24*7), # Time in weeks
      SEXNF = factor(SEX, levels = c(0,1), c("Male", "Female"))
    ) %>%
    dplyr::group_by(ITER, ID) %>%
    dplyr::mutate(EVCOUNT = cumsum(DV)) %>% # Compute EVCOUNT
    dplyr::ungroup()

  # VPC for time to first event data
  odata1 <- odata %>% filter_xth_event(1)
  sdata1 <- sdata %>% filter_xth_event(1)

  svg()
  p1 <- ggKMvpc(odata1,
                sdata1,
                time=TIME,
                event=DV,
                iter=ITER,
                strat=c('SEXNF'),
                ci=c(0.05,0.95),
                cuminc=F,
                simCol='blue',
                obsCol='blue',
                posObs=NULL,
                show.censor=T
  )
  p2 <- ggKMvpc(odata,
                sdata,
                time=TIME,
                event=DV,
                iter=ITER,
                strat=NULL,
                ci=c(0.05,0.95),
                cuminc=F,
                simCol='blue',
                obsCol='blue',
                posObs=NULL,
                show.censor=T
  )

  p3 <- ggKMvpc(odata,
                sdata,
                time=TIME,
                event=DV,
                iter=ITER,
                ci=c(0.05,0.95),
                cuminc=F,
                simCol='blue',
                obsCol='blue',
                posObs=NULL,
                show.censor=T,
                palette = PMXColors_pmx_palettes(),
                scale.percent = TRUE,
                ylab = 'Percentage without events (%)',
                xlab = 'Time (Weeks)',
                legend.position = 'top',
                xlim = c(0,5000),
                ylim = NULL

  )

  dev.off()
  vdiffr::expect_doppelganger('vpc with strat', p1)
  vdiffr::expect_doppelganger('vpc without strat', p2)
  vdiffr::expect_doppelganger('changing axis limits and palette', p3)

})


