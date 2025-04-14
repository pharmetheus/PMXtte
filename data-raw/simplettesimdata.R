## code to prepare `simplesimttedata` dataset goes here
set.seed(123)
simplettesimdata <- tidyr::expand_grid(
  ITER = 1:100,
  ID = unique(simplettedata$ID)
) %>%
  dplyr::mutate(
    TIME = round((100+15)*runif(dplyr::n()), 2),
    DV = 1
  ) %>%
  dplyr::left_join(
    dplyr::rename(simplettedata, DVOBS = DV, TIMEOBS = TIME),
    by = "ID"
  ) %>%
  dplyr::mutate(
    DV = dplyr::case_when(
      TIME >= TIMEOBS ~ 0,
      TIME > 100 ~ 0,
      TRUE ~ 1
    ),
    TIME = dplyr::case_when(
      ((DVOBS == 0) & (TIME >= TIMEOBS)) ~ TIMEOBS,
      TIME > 100 ~ 100,
      TRUE ~ TIME
    ),
    DVOBS = NULL, TIMEOBS = NULL
  ) %>%
  as.data.frame()

usethis::use_data(simplettesimdata, overwrite = TRUE)
