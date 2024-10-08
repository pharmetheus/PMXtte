PMXRenv::library.unqualified("vdiffr")

test_that('similar plots', {
  odata <- readr::read_csv(
    system.file('extdata/odata.csv', package= 'PMXtte'),
    show_col_types = FALSE
  )
  sdata <- readr::read_csv(
    system.file('extdata/sdata.csv', package= 'PMXtte'),
    show_col_types = FALSE
  )

  svg()
  p1 <- ggKMvpc(odata,
                sdata,
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


