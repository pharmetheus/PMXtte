PMXRenv::library.unqualified("vdiffr")
library(purrr)
rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)

test_that('kmmc works', {
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
    TIME = TIME / (24*7)
  ) %>%
  dplyr::group_by(ITER, ID) %>%
  dplyr::mutate(EVCOUNT = cumsum(DV)) %>% # Compute EVCOUNT
  dplyr::ungroup()

odata1 <- odata %>% filter_xth_event(1) %>% mutate(TIME2 = TIME)
sdata1 <- sdata %>% filter_xth_event(1) %>%
  mutate(TIME2 = TIME) %>%
  rename(DOSEN=DOSE)

svg()
p1 <- ggKMMC(odata1,
             sdata1,
             cov = DOSEN,
             bins=seq(from=0,to=52,by=1)) +
  theme(legend.position="top") +
  theme(legend.title = element_blank())

p1a <- ggKMMC(odata1,
             sdata1,
             cov = DOSEN,
             CI = 95,
             bins=seq(from=0,to=52,by=1)) +
  theme(legend.position="top") +
  theme(legend.title = element_blank())

p2 <- ggKMMC(odata1,
             sdata1,
             cov = DOSEN,
             xlab = "Time (weeks)",
             ylab = "Mean of dose (mg)",
             bins=seq(from=0,to=52,by=1)) +
  theme(legend.position="top") +
  theme(legend.title = element_blank())

p3 <- ggKMMC(odata1,
             sdata1,
             cov = DOSEN,
             timevar = TIME2,
             bins=seq(from=0,to=52,by=1)) +
  theme(legend.position="top") +
  theme(legend.title = element_blank())

p4 <- ggKMMC(odata1,
             sdata1,
             cov = DOSEN,
             xlab = "Time (weeks)",
             ylab = "Mean of dose (mg)",
             fill = PMXtte:::PMXColors_pmx_palettes(name="light",firstColorNum = 4),
             bins=seq(from=0,to=52,by=1)) +
  theme(legend.position="top") +
  theme(legend.title = element_blank())

p5 <- ggKMMC(odata1,
             sdata1,
             cov = DOSEN,
             xlab = "Time (weeks)",
             ylab = "Mean of dose (mg)",
             fill = PMXtte:::PMXColors_pmx_palettes(name="light",firstColorNum = 4),
             obsCol="red",
             bins=seq(from=0,to=52,by=1)) +
  theme(legend.position="top") +
  theme(legend.title = element_blank())

p6 <- ggKMMC(odata1,
             sdata1,
             ci = c(0.02,0.98),
             cov = DOSEN,
             xlab = "Time (weeks)",
             ylab = "Mean of dose (mg)",
             bins=seq(from=0,to=52,by=1)) +
  theme(legend.position="top") +
  theme(legend.title = element_blank())

dev.off()

vdiffr::expect_doppelganger('basic plot', p1)
vdiffr::expect_doppelganger('basic plot with xlab and ylab', p2)
vdiffr::expect_doppelganger('using different time variable', p3)
vdiffr::expect_doppelganger('using fill color via palette', p4)
vdiffr::expect_doppelganger('using col to specify line color', p5)
vdiffr::expect_doppelganger('Changing CI', p1a)
vdiffr::expect_doppelganger('Setting the CI with ci', p6)
})
