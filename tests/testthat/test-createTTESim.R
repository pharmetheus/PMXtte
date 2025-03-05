# test/testthat/test-createTTESim.R

test_that("createTTESim function generates basic NONMEM simulation model", {
  mod_file_path <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
  model_code <- createTTESim(modFile = mod_file_path, outFile = NULL)
  expect_type(model_code, "character")
  expect_true(any(grepl("\\$PROBLEM", model_code)))
  expect_true(any(grepl("\\$SIMULATION", model_code)))
  expect_true(any(grepl("OPEN \\(99, FILE = ", model_code)))
})

test_that("createTTESim differentiates between TTE and RTTE simulations", {
  mod_file_path <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
  tte_model <- createTTESim(modFile = mod_file_path, outFile = NULL, rtte = FALSE)
  rtte_model <- createTTESim(modFile = mod_file_path, outFile = NULL, rtte = TRUE)

  expect_false(any(grepl("RTTE Simulation specific", tte_model)))
  expect_true(any(grepl("RTTE Simulation specific", rtte_model)))

  # expect_false(any(grepl("strRTTEVar=1", tte_model))) # strRTTEVar not defined in TTE $ERROR
  # expect_true(any(grepl("strRTTEVar=1", rtte_model))) # strRTTEVar defined in RTTE $ERROR
})

test_that("createTTESim includes time-varying covariates in output for RTTE", {
  mod_file_path <- system.file("extdata", "rtte_mod.mod", package = "PMXtte")
  rtte_model_cov <- createTTESim(modFile = mod_file_path, outFile = NULL, rtte = TRUE, timeVaryingCovs = "DOSEN",updateInits = FALSE)
  des_section_index <- grep(";---------- RTTE Simulation specific", rtte_model_cov)
  # des_section <- rtte_model_cov[des_section_index + 9] # Line with WRITE statement
  # expect_match(des_section, "WRITE \\(99,.*?DOSEN") # Check if DOSEN is in WRITE statement

  pk_section_com <- rtte_model_cov[grepl("COM\\(7\\+i\\)=", rtte_model_cov)]
  expect_true(any(grepl("COM\\(8\\)=DOSEN", rtte_model_cov))) # Check if COM(8) is assigned DOSEN
})

test_that("createTTESim includes baseline covariates in output", {
  mod_file_path <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
  model_code_basecov <- createTTESim(modFile = mod_file_path, outFile = NULL, baselineCovs = c("AGE", "SEX"))

  des_section_index_tte <- grep(";---------- TTE Simulation specific", model_code_basecov)
  des_section <- model_code_basecov[des_section_index_tte + 31] # Line with WRITE for TTE
  if(any(grepl(";---------- RTTE Simulation specific", model_code_basecov))){ # Handle both RTTE and TTE if test is run for RTTE model
    des_section_index_rtte <- grep(";---------- RTTE Simulation specific", model_code_basecov)
    des_section <- model_code_basecov[des_section_index_rtte + 9] # Line with WRITE for RTTE
  }
  expect_match(des_section, "WRITE \\(99,.*?AGE.*?SEX") # Check if AGE and SEX are in WRITE statement
})


test_that("createTTESim updates initial estimates from .ext file when updateInits=TRUE", {
  mod_file_path <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
  model_code_update_inits <- createTTESim(modFile = mod_file_path, outFile = NULL, updateInits = TRUE)
  theta_lines <- model_code_update_inits[grepl("\\$THETA", model_code_update_inits)]
  # Assuming tte_weibull.ext has THETA1 and THETA2 values different from default in .mod
  expect_match(theta_lines[1], "\\$THETA 0\\.0109.* ") # Check if THETA1 updated
  expect_match(theta_lines[2], "\\$THETA 1\\.31.* ") # Check if THETA2 updated
})

test_that("createTTESim does not update initial estimates when updateInits=FALSE", {
  mod_file_path <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
  model_code_no_update_inits <- createTTESim(modFile = mod_file_path, outFile = NULL, updateInits = FALSE)
  theta_lines <- model_code_no_update_inits[grepl("\\$THETA", model_code_no_update_inits)]
  # Expecting default THETA values from tte_weibull.mod (or function defaults if not in .mod)
  expect_match(theta_lines[1], "\\$THETA.*\\(0,5E-03\\)") # Check if THETA1 is default (or from .mod if default in .mod)
  expect_match(theta_lines[2], "\\$THETA.*\\(0\\.2\\,1\\.3\\)") # Check if THETA2 is default (or from .mod if default in .mod)
})

test_that("createTTESim replaces SUBROUTINE to ADVAN6 when replaceSUB=TRUE", {
  mod_file_path <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
  model_code_replace_sub <- createTTESim(modFile = mod_file_path, outFile = NULL, replaceSUB = TRUE)
  sub_line <- model_code_replace_sub[grepl("\\$SUBROUTINE", model_code_replace_sub)]
  expect_match(sub_line, "\\$SUBROUTINE.*?ADVAN=6") # Check if ADVAN=6 is present

  model_code_no_replace_sub <- createTTESim(modFile = mod_file_path, outFile = NULL, replaceSUB = FALSE)
  sub_line_no_replace <- model_code_no_replace_sub[grepl("\\$SUBROUTINE", model_code_no_replace_sub)]
  original_sub_line <- readLines(mod_file_path)[grepl("\\$SUBROUTINE", readLines(mod_file_path))] # Get original $SUB from mod file
  expect_equal(sub_line_no_replace, original_sub_line) # Check if $SUB is original
})


test_that("createTTESim writes to outFile and returns NULL invisibly when outFile is specified", {
  mod_file_path <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
  temp_file <- tempfile("test_sim_model", fileext = ".mod")
  result <- createTTESim(modFile = mod_file_path, outFile = temp_file)
  expect_null(result)
  expect_true(file.exists(temp_file))
  file_content <- readLines(temp_file)
  expect_true(any(grepl("\\$PROBLEM", file_content))) # Check if file contains model code
  file.remove(temp_file) # Clean up temp file
})

test_that("createTTESim returns character vector when outFile is NULL", {
  mod_file_path <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
  result <- createTTESim(modFile = mod_file_path, outFile = NULL)
  expect_type(result, "character")
})

test_that("createTTESim stops if modFile does not exist", {
  expect_error(createTTESim(modFile = "non_existent_model.mod"), "Cannot find model file: non_existent_model.mod")
})

