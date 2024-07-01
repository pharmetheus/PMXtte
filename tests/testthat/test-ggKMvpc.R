PMXRenv::library.unqualified("vdiffr")

test_that('similar plots', {
  odata <- read_csv(system.file('extdata/odata.csv',
                                     package= 'PMXtte'))
  sdata <- read_csv(system.file('extdata/sdata.csv',
                                package= 'PMXtte'))

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
  dev.off()
  vdiffr::expect_doppelganger('vpc with strat', p1)
  vdiffr::expect_doppelganger('vpc without strat', p2)

})


