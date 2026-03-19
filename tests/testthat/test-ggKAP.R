da <- data.frame(
  log = c(TRUE, FALSE, TRUE),
  cha = c("a", "c", "b"),
  fac = factor(c("a", "c", "b")),
  int = c(1L, 6L, 9L),
  dou = c(1.0, 2.0, 9)
)

dat <- PMXtte::simplettedata

test_that("checks works", {
  expect_no_error(
    check_ggKAP_input(data = da, time_var = "dou", dv_var = "dou")
  )
  expect_error(
    check_ggKAP_input(data = da, time_var = "fac", dv_var = "dou"),
    "Possible classes for \"fac\" are: integer, numeric"
  )
  expect_error(
    check_ggKAP_input(data = da, time_var = "dou", dv_var = "fac"),
    "Possible classes for \"fac\" are: integer, numeric"
  )
  expect_error(
    check_ggKAP_input(data = da, time_var = "dou", dv_var = "dou",
                      color_var = "dou"),
    "Possible classes for \"dou\" are: logical, character, factor"
  )

  expect_no_error(
    check_ggKAP_input(data = dat, time_var = "TIME", dv_var = "DV")
  )
  expect_no_error(
    check_ggKAP_input(data = dat, time_var = "TIME", dv_var = "DV",
                      color_var = "SEXF")
  )
  expect_error(
    check_ggKAP_input(data = dat, time_var = "TIME", dv_var = "DV",
                      color_var = "DV"),
    "Possible classes for \"DV\" are: logical, character, factor"
  )

  expect_error(
    ggKAP(dat, color_var = "DV"),
    "Possible classes for \"DV\" are: logical, character, factor"
  )
  expect_no_error(
    suppressWarnings(ggKAP(dat, color_var = "DV", check_input = FALSE))
  )
})

test_that("Definition of x-breaks works", {
  expect_no_warning(ggKAP(dat))
  expect_no_warning(ggKAP(dat, scale_x_breaks = c(0,50,100)))
  expect_no_warning(ggKAP(dat, scale_x_break_by = 20))
  expect_warning(ggKAP(dat, scale_x_breaks = c(0,50,100), scale_x_break_by = 20), regexp = "\\`scale_x_break_by\\` is ignored, breaks of the x-axis being defined in \\`scale_x_breaks\\`")
})

test_that("ggKAParrange() works", {
  dat <- PMXtte::simplettedata

  # This does not pass because grid.arrange() gives unique names
  # that cannot be controled by the seed.
  # We can at least do a visual inspection
  # expect_equal(
  #   ggKAP(dat, color_var = "SEXF"),
  #   ggKAParrange(ggKAP(dat, color_var = "SEXF", arrange = FALSE))
  # )

  plotlist <- ggKAP(dat, color_var = "SEXF", arrange = FALSE)

  expect_length(plotlist, 4)
  expect_named(plotlist, c("leg_col", "leg_kpm", "fig", "risktab"))
  expect_s3_class(plotlist$leg_col, "gtable")
  expect_s3_class(plotlist$leg_kpm, "gtable")
  expect_s3_class(plotlist$fig, "ggplot")
  expect_s3_class(plotlist$risktab, "ggplot")

  plotlist$fig <- plotlist$fig +
    ggplot2::theme(
      text = ggplot2::element_text(size = 20),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
      )
  expect_s3_class(plotlist$fig, "ggplot")

  # Re-arrange them with ggKAParrange:
  ans <- ggKAParrange(plotlist)
  expect_s3_class(plotlist$fig, "ggplot")

  ggKAParrange(plotlist, figtable = c(1,2))
  expect_s3_class(plotlist$fig, "ggplot")

})

