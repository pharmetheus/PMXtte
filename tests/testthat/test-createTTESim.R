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

  expect_warning(
    createTTESim(modFile = mod_file_path, outFile = NULL, replaceSUB = FALSE),
    "Note that other ADVAN\'s than ADVAN6 might not work properly with MTIME simulations"
  )
  model_code_no_replace_sub <- suppressWarnings(createTTESim(modFile = mod_file_path, outFile = NULL, replaceSUB = FALSE))
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

test_that("createTTESim make correct simfile name", {
  expect_equal(
    make_simtab_name("run101.mod"),
    "vpctabrun101"
  )
  expect_equal(
    make_simtab_name("path/to/model/run101.mod"),
    "vpctabrun101"
  )
  expect_equal(
    make_simtab_name("run101.mod", prefix = "simtab"),
    "simtabrun101"
  )
  expect_equal(
    make_simtab_name("run101.mod", prefix = "", suffix = "sim"),
    "run101sim"
  )
})

test_that("createTTESim wrap hazard time with abs()", {
  rec1 <- "HAZ = LAM * EXP(SHP*(T-TIMEP)) ; Hazard for Placebo + Placebo "
  rec2 <- "HAZ = LAM * EXP(SHP*(T) + RF) ;"
  rec3 <- "DADT(1)=LAM*GAM*(LAM*(T-TIMEP)+DEL)**(GAM-1) ; Weibull distribution"
  rec4 <- "TIMEP is time previous event"
  rec5 <- "hello world"
  rec6 <- "BEST-TIMEPREVIOUS"  # ignore if T is end of a word and TIMEP the beginning of a new one
  rec7 <- "HAZ = LAM * EXP(SHP*(T -TIMEP))" # deals with white space
  rec8 <- "HAZ = LAM * EXP(SHP*(T - TIMEP))"
  rec9 <- "HAZ = LAM * EXP(SHP*(T- TIMEP))"
  rec10 <- "HAZ = LAM * EXP(SHP*(ABS(T-TIMEP)))" # dont wrap again if already there

  recs <- c(rec1, rec2, rec3, rec4, rec5, rec6, rec7, rec8, rec9, rec10)
  expect_equal(
    wrap_abs(recs, timepVar = "TIMEP"),
    c(
      "HAZ = LAM * EXP(SHP*(ABS(T-TIMEP))) ; Hazard for Placebo + Placebo ",
      "HAZ = LAM * EXP(SHP*(T) + RF) ;",
      "DADT(1)=LAM*GAM*(LAM*(ABS(T-TIMEP))+DEL)**(GAM-1) ; Weibull distribution",
      "TIMEP is time previous event",
      "hello world",
      "BEST-TIMEPREVIOUS",
      "HAZ = LAM * EXP(SHP*(ABS(T-TIMEP)))",
      "HAZ = LAM * EXP(SHP*(ABS(T-TIMEP)))",
      "HAZ = LAM * EXP(SHP*(ABS(T-TIMEP)))",
      "HAZ = LAM * EXP(SHP*(ABS(T-TIMEP)))")
  )

  rec <- "HAZ = LAM * EXP(SHP*(T- PTIME))" #Anything else than TIMEP
  expect_equal(
    wrap_abs(rec, timepVar = "PTIME"),
    "HAZ = LAM * EXP(SHP*(ABS(T-PTIME)))"
  )

  # works in the user-level function
  mypath <- system.file("extdata", "rtte_mod.mod", package = "PMXtte")
  newcode <- createTTESim(modFile = mypath, rtte = TRUE, outFile = NULL, updateInits = FALSE)
  expect_match(newcode[59], "ABS\\(T-COM\\(1\\)\\)")

})

test_that("createTTESim comments out definitions of Y=", {
  expect_equal(comment_y("Y = "), "; Y = ")
  expect_equal(comment_y("Y= "), "; Y= ")
  expect_equal(comment_y("IF(DV.EQ.1) Y= SUR*HAZ"), "; IF(DV.EQ.1) Y= SUR*HAZ")
  expect_equal(comment_y("Hello"), "Hello")
  expect_equal(comment_y("YY= "), "YY= ")
})
