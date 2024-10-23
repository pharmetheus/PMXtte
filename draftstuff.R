# Observed data
Adata <- readr::read_table("../PMX-RTTEWorkshop-PFPMX-1/ToClient/ProjectUpdate/Handson/HO4/ProvidedFiles/xptab304", skip = 1) %>%
  filter(EVID==0) %>%
  mutate(TIME = TIME/(24*7)) %>%
  group_by(ID) %>%
  mutate(RTTE = seq_along(DV)) %>%
  ungroup()

Adata %>%
  group_by(ID) %>%
  mutate(TSLE = lag(TIME, 0), .after = TIME)

# Simulated data

Sdata <- readr::read_table("../PMX-RTTEWorkshop-PFPMX-1/ToClient/ProjectUpdate/Handson/HO4/Solutions/vpc304.dat")
length(unique(Sdata$ITER))

Sdata %>%
  filter(ITER <= 50) %>%
  readr::write_rds("inst/extdata/vpcdat.rds")

readRDS()

Sdata <- readr::read_table("../PMX-RTTEWorkshop-PFPMX-1/ToClient/ProjectUpdate/Handson/HO4/Simres.csv")

Sdata <- Sdata %>%
  group_by(ITER, ID) %>%
  mutate(
    DV = if_else(RTTE!=lag(RTTE,default = 0)+1, 1, DV),
    RTTE = seq_along(DV),
    EVCOUNT = cumsum(DV)
  ) %>%
  ungroup() %>%
  mutate(TIME = TIME/(24*7))

ggKMvpc(
  odata = Adata %>% filter_xth_event(1),
  sdata = Sdata %>% filter_xth_event(1),
  cuminc = FALSE
)
ggKMvpc(
  odata = Adata %>% filter_xth_event(2),
  sdata = Sdata %>% filter_xth_event(2,time_since_last_event = NULL),
  cuminc = FALSE
)

maxEvent <- max(Adata$RTTE[Adata$DV==1])

ggKMvpc(
  odata = Adata %>% filter(RTTE <= 5),
  sdata = Sdata %>% filter(RTTE <= 5),
  cuminc = FALSE,
  strat = 'RTTE'
)


Adata %>%
  filter_xth_event(7)
