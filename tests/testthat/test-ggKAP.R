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

