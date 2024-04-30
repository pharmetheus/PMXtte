#test input validation
test_that("Function throughs error if in input is not dataframe", {
  x <- c('x','y')
  expect_error(Kaplan_Meier_curves(data = x),'Input is not a dataframe')
})

test_that("columns are in the dataframe", {
  data_frame <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv',
                                     package= 'PMXtte'),na.strings=c(".","-99","NA"))
  expect_error(Kaplan_Meier_curves(data_frame, time_col = 'times'),
               'one or more specified columns do not exist in the dataframe')
})

data_frame <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv',
                                   package= 'PMXtte'),na.strings=c(".","-99","NA"))
data_prep <- prep_dataframe(data_frame)
plot <- Kaplan_Meier_curves(data_prep)
plot_file <- "kaplan_meier.png"
png(plot_file, width = 800, height = 600, units = 'px', res = 96)
print(plot)
dev.off()
#Test the plot against a snapshot
test_that("Plot matches snapshot", {

  expect_snapshot_file(plot_file, 'Kaplan_meier_curves_snapshot')
})
