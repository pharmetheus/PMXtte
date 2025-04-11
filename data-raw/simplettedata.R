## code to prepare `simplettedata` dataset goes here
set.seed(123)
simplettedata <- data.frame(
  ID = seq_len(50),
  TIME = round(100*runif(50)),
  DV = round(runif(50)),
  SEXF = factor(round(runif(50)+1), levels = c(1,2), c("Male", "Female")),
  AUCQF = factor(sample(1:3, size=50, replace=TRUE), levels = 1:3, labels = c("1st", "2nd", "3rd"))
)


usethis::use_data(simplettedata, overwrite = TRUE)
