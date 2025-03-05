# test/testthat/test-createTTEStart.R

test_that("createTTEStart function generates basic NONMEM model", {
  model_code <- createTTEStart(outFile = NULL)
  expect_type(model_code, "character")
  expect_true(any(grepl("\\$PROBLEM", model_code)))
  expect_true(any(grepl("\\$INPUT ID TIME DV EVID", model_code)))
  expect_true(any(grepl("\\$DATA datafile.csv", model_code)))
  expect_true(any(grepl("\\$SUBROUTINE", model_code)))
  expect_true(any(grepl("\\$MODEL", model_code)))
  expect_true(any(grepl("\\$PK", model_code)))
  expect_true(any(grepl("\\$DES", model_code)))
  expect_true(any(grepl("\\$ERROR", model_code)))
  expect_true(any(grepl("\\$THETA", model_code)))
  expect_true(any(grepl("\\$OMEGA", model_code)))
  expect_true(any(grepl("\\$ESTIMATION", model_code)))
  expect_true(any(grepl("\\$COVARIANCE", model_code)))
})

test_that("createTTEStart generates exponential distribution model", {
  model_code <- createTTEStart(outFile = NULL, distribution = "exponential")
  des_section <- model_code[grep("\\$DES", model_code) + 1] # Line after $DES
  expect_match(des_section, "DADT\\(1\\)=LAM ; Exponential distribution")
  theta_section <- model_code[grepl("\\$THETA", model_code) & grepl("Scale parameter", model_code)]
  expect_match(theta_section, "\\$THETA \\(0,1\\) ; Scale parameter")
})

test_that("createTTEStart generates weibull distribution model", {
  model_code <- createTTEStart(outFile = NULL, distribution = "weibull")
  des_section <- model_code[grep("\\$DES", model_code) + 3]
  expect_match(des_section, "DADT\\(1\\)=LAM\\*GAM\\*\\(LAM\\*T\\+DEL\\)\\*\\*\\(GAM-1\\) ; Weibull distribution")
  theta_sections <- model_code[grepl("\\$THETA", model_code)]
  expect_match(theta_sections[1], "\\$THETA \\(0,1\\) ; Scale parameter")
  expect_match(theta_sections[2], "\\$THETA \\(0,1\\) ; Shape parameter")
})

test_that("createTTEStart generates gompertz distribution model", {
  model_code <- createTTEStart(outFile = NULL, distribution = "gompertz")
  des_section <- model_code[grep("\\$DES", model_code) + 1]
  expect_match(des_section, "DADT\\(1\\)=LAM\\*EXP\\(GAM\\*T\\) ; Gompertz distribution")
  theta_sections <- model_code[grepl("\\$THETA", model_code)]
  expect_match(theta_sections[1], "\\$THETA \\(0,1\\) ; Scale parameter")
  expect_match(theta_sections[2], "\\$THETA 0\\.01    ; Shape parameter")
})

test_that("createTTEStart generates weibull_exp distribution model", {
  model_code <- createTTEStart(outFile = NULL, distribution = "weibull_exp")
  des_section <- model_code[grep("\\$DES", model_code) + 3]
  expect_match(des_section, "DADT\\(1\\)=LAM\\*EXP\\*\\(BETA\\*LOG\\(T\\+DEL\\)\\) ; Weibull distribution, alt parameterization")
  theta_sections <- model_code[grepl("\\$THETA", model_code)]
  expect_match(theta_sections[1], "\\$THETA \\(0,1\\) ; Scale parameter")
  expect_match(theta_sections[2], "\\$THETA 0\\.01    ; Shape parameter")
})

test_that("createTTEStart generates 'no' distribution model", {
  model_code <- createTTEStart(outFile = NULL, distribution = "no")
  pk_section <- model_code[grep("\\$PK", model_code) & grepl("PAR=THETA", model_code)]
  expect_match(pk_section, "PAR=THETA\\(1\\)\\*EXP\\(ETA\\(1\\)\\)")
  des_section <- model_code[grep("\\$DES", model_code) + 1]
  expect_match(des_section, "DADT\\(1\\)=PAR ; Hazard expression for the distribution")
})

test_that("createTTEStart differentiates between TTE and RTTE models", {
  tte_model <- createTTEStart(outFile = NULL, rtte = FALSE)
  rtte_model <- createTTEStart(outFile = NULL, rtte = TRUE)

  expect_false(any(grepl("TIMEP = 0", tte_model))) # TIMEP reset not in TTE $PK
  expect_true(any(grepl("TIMEP = 0", rtte_model)))  # TIMEP reset in RTTE $PK

  expect_false(any(grepl("OCHZ=0", tte_model))) # OCHZ reset not in TTE $ERROR
  expect_true(any(grepl("OCHZ=0", rtte_model)))  # OCHZ reset in RTTE $ERROR
})

test_that("createTTEStart uses dataFile to create $INPUT and $DATA sections", {
  data_file_path_csv <- system.file("extdata", "DAT-1a-PMX-RTTEWorkshop-PFPMX-1.csv", package = "PMXtte")
  model_code_csv <- createTTEStart(outFile = NULL, dataFile = data_file_path_csv)

  expect_match(model_code_csv[grepl("\\$DATA", model_code_csv)], paste0("\\$DATA ", gsub("\\\\", "/", data_file_path_csv))) # Check $DATA path
  input_line_csv <- model_code_csv[grepl("\\$INPUT", model_code_csv)][1] # Get the first $INPUT line
  expect_match(input_line_csv, "\\$INPUT REF") # Check for header from CSV

  data_file_path_dat <- system.file("extdata", "tte_data1.dat", package = "PMXtte")
  model_code_dat <- createTTEStart(outFile = NULL, dataFile = data_file_path_dat)

  expect_match(model_code_dat[grepl("\\$DATA", model_code_dat)], paste0("\\$DATA ", gsub("\\\\", "/", data_file_path_dat))) # Check $DATA path
  input_line_dat <- model_code_dat[grepl("\\$INPUT", model_code_dat)][1] # Get the first $INPUT line
  expect_match(input_line_dat, "\\$INPUT ID") # Check for header from DAT
})

test_that("createTTEStart replaces headers using replaceAsId and replaceAsTime", {
  data_file_path_csv <- system.file("extdata", "DAT-1a-PMX-RTTEWorkshop-PFPMX-1.csv", package = "PMXtte")
  model_code <- createTTEStart(
    outFile = NULL,
    dataFile = data_file_path_csv,
    replaceAsId = "STUDYIDN",
    replaceAsTime = "TSFDY"
  )
  input_line <- model_code[grepl("\\$INPUT", model_code)][1] # Get the first $INPUT line
  expect_match(input_line, "\\$INPUT REF ID") # Check if STUDYIDN replaced with ID
  expect_match(input_line, " TIME ") # Check if TSFDY replaced with TIME
})

test_that("createTTEStart includes $TABLE section when includeTable is TRUE and dataFile is provided", {
  data_file_path_csv <- system.file("extdata", "DAT-1a-PMX-RTTEWorkshop-PFPMX-1.csv", package = "PMXtte")
  model_code_table <- createTTEStart(outFile = NULL, dataFile = data_file_path_csv, includeTable = TRUE)
  expect_true(any(grepl("\\$TABLE", model_code_table)))

  model_code_no_table <- createTTEStart(outFile = NULL, dataFile = data_file_path_csv, includeTable = FALSE)
  expect_false(any(grepl("\\$TABLE", model_code_no_table)))

  model_code_no_datafile_table <- createTTEStart(outFile = NULL, dataFile = NULL, includeTable = TRUE)
  expect_false(any(grepl("\\$TABLE", model_code_no_datafile_table))) # No table if no dataFile even if includeTable=TRUE
})

test_that("createTTEStart returns NULL invisibly when outFile is specified", {
  temp_file <- tempfile("test_model", fileext = ".mod")
  result <- createTTEStart(outFile = temp_file)
  expect_null(result)
  expect_true(file.exists(temp_file))
  file.remove(temp_file) # Clean up temp file
})

test_that("createTTEStart returns character vector when outFile is NULL", {
  result <- createTTEStart(outFile = NULL)
  expect_type(result, "character")
})
