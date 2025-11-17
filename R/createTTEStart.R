#' Create NONMEM control stream for Time-To-Event (TTE) models
#'
#' @description This function generates a NONMEM control stream file for
#'   time-to-event (TTE) or repeated time-to-event (RTTE) models. It supports
#'   different survival distributions and allows for data input from a file.
#'
#' @param outFile Character string specifying the filename (and path) for the
#'   NONMEM estimation file to be written to disk. If `NULL`, no file is
#'   written, and the model code is returned as a character vector.
#' @param rtte Logical, indicating whether to create a model for repeated
#'   time-to-event simulations (`TRUE`) or standard time-to-event simulations
#'   (`FALSE`, default).
#' @param dataFile Character string specifying the path to a data file to be
#'   used for the `$INPUT` and `$DATA` sections of the NONMEM control stream.
#'   If provided, the function will attempt to read the headers from this file
#'   to create the `$INPUT` statement. If `NULL` (default), a dummy `$DATA`
#'   section is created, and a basic `$INPUT ID TIME DV EVID` is used.
#' @param replaceAsId Character string. If a data file is provided via
#'   `dataFile`, this argument allows you to specify a string in the data file
#'   header that should be replaced with "ID" in the `$INPUT` statement.
#'   Default is `NULL` (no replacement).
#' @param replaceAsTime Character string.  If a data file is provided via
#'   `dataFile`, this argument allows you to specify a string in the data file
#'   header that should be replaced with "TIME" in the `$INPUT` statement.
#'   Default is `NULL` (no replacement).
#' @param distribution Character string specifying the survival distribution
#'   to be used. Options are: `"exponential"` (default), `"weibull"`,
#'   `"weibull_exp"`, `"gompertz"`, and `"no"` (for a user-defined hazard
#'   function, where "PAR=THETA(1)*EXP(ETA(1))" is used).
#' @param includeTable Logical, indicating whether to include a `$TABLE`
#'   section in the NONMEM control stream. Only relevant if `dataFile` is not
#'   `NULL`. Default is `TRUE`.
#'
#' @return If `outFile` is specified, the function writes the NONMEM control
#'   stream to the specified file and returns `NULL` invisibly. If `outFile`
#'   is `NULL`, the function returns the NONMEM control stream as a character
#'   vector.
#' @export
#'
#' @examples
#' # Example TTE model with Weibull distribution and output to file
#' # File path to a data object (embedded in the package as an example)
#' mypath <- system.file("extdata", "tte_data1.dat", package = "PMXtte")
#' \dontrun{
#' createTTEStart(
#'   outFile = "tte_weibull_auto.mod",
#'   dataFile = mypath,
#'   distribution = "weibull",
#'   rtte = FALSE
#' )
#'}
#' # Example RTTE model with Weibull distribution, data from CSV,
#' # replacing "TSFDY" with "TIME" in $INPUT
#' mypath <- system.file("extdata", "DAT-1a-PMX-RTTEWorkshop-PFPMX-1.csv", package = "PMXtte")
#' \dontrun{
#' createTTEStart(
#'   outFile = "rtte_mod.mod",
#'   dataFile = mypath,
#'   distribution = "weibull",
#'   rtte = TRUE,
#'   replaceAsTime = "TSFDY"
#' )
#' }
createTTEStart <- function(outFile = NULL,
                           rtte = FALSE,
                           dataFile = NULL,
                           replaceAsId = NULL,
                           replaceAsTime = NULL,
                           distribution = "exponential",
                           includeTable = TRUE) {

  type <- "time-to-event"
  if (rtte) {
    type <- paste0("repeated ", type)
  }

  inputSection <- c(
    "$INPUT ID TIME DV EVID",
    "; NOTE - DROP COLUMNS NEEDS TO BE ADDED MANUALLY"
  )
  dataSection <- c("$DATA datafile.csv")

  if (!is.null(dataFile)) {
    if (!file.exists(dataFile)) {
      stop(paste0("Cannot find data file: ", dataFile))
    }
    headers <- names(read.csv(dataFile, nrows = 1, header = TRUE))
    if (!is.null(replaceAsId)) {
      headers <- gsub(replaceAsId, "ID", headers)
    }
    if (!is.null(replaceAsTime)) {
      headers <- gsub(replaceAsTime, "TIME", headers)
    }

    inputSection <- c(
      paste0("$INPUT ", paste0(headers, collapse = " ")),
      "; NOTE - DROP COLUMNS NEEDS TO BE ADDED MANUALLY"
    )
    dataSection <- paste0("$DATA ", dataFile)
  }

  dataSection <- c(
    dataSection,
    "          IGNORE=@",
    "; NOTE - ADDITIONAL IGNORE NEEDS TO BE ADDED MANUALLY"
  )

  subroutineSection <- "$SUBROUTINE ADVAN=13 TOL=9"
  modelSection <- "$MODEL    COMP=(HAZARD)"

  pkSection <- "$PK"
  if (rtte) {
    pkSection <- c(
      pkSection,
      "",
      "IF (NEWIND.NE.2) TIMEP = 0  ; New subject, reset previous event time",
      ""
    )
  }
  desSection <- c("$DES")
  if (distribution == "weibull" || distribution == "weibull_exp") {
    desSection <- c(
      desSection,
      "",
      "    DEL=1E-12 ;Protect against events as T=0"
    )
  }

  if (distribution == "no") {
    pkSection <- c(
      pkSection,
      "          PAR=THETA(1)*EXP(ETA(1))"
    )
    desSection <- c(
      desSection,
      "DADT(1)=PAR ; Hazard expression for the distribution"
    )
  }

  hazardNow <- "PAR"
  destination <- "T"
  destinationError <- "TIME"

  if (rtte) {
    destination <- "(ABS(T-TIMEP))"
    destinationError <- "(ABS(TIME-TIMEP))"
  }

  if (distribution == "exponential" || distribution == "weibull" ||
      distribution == "gompertz" || distribution == "weibull_exp") {
    pkSection <- c(
      pkSection,
      "    LAM=THETA(1)*EXP(ETA(1)) ; Scale parameter"
    )
  }
  if (distribution == "weibull" || distribution == "gompertz") {
    pkSection <- c(
      pkSection,
      "    GAM=THETA(2) ; Shape parameter"
    )
  }
  if (distribution == "weibull_exp") {
    pkSection <- c(
      pkSection,
      "    BETA=THETA(2) ; Shape parameters"
    )
  }

  thetasSection <- "$THETA 1"

  if (distribution == "exponential") {
    desSection <- c(
      desSection,
      "    DADT(1)=LAM ; Exponential distribution"
    )
    hazardNow <- "LAM"
    thetasSection <- "$THETA (0,1) ; Scale parameter (1/time unit)"
  }

  if (distribution == "weibull") {
    desSection <- c(
      desSection,
      paste0(
        "    DADT(1)=LAM*GAM*(LAM*", destination, "+DEL)**(GAM-1) ; Weibull distribution"
      )
    )
    hazardNow <- paste0("LAM*GAM*(LAM*", destinationError, "+DELX)**(GAM-1)")
    thetasSection <- c(
      "$THETA (0,1) ; Scale parameter (1/time unit)",
      "$THETA (0,1) ; Shape parameter"
    )
  }

  if (distribution == "weibull_exp") {
    desSection <- c(
      desSection,
      paste0(
        "    DADT(1)=LAM*EXP*(BETA*LOG(", destination, "+DEL)) ; Weibull distribution, alt parameterization"
      )
    )
    hazardNow <- paste0("LAM*EXP*(BETA*LOG(", destinationError, "+DELX))")
    thetasSection <- c(
      "$THETA (0,1) ; Scale parameter (1/time unit)",
      "$THETA 0.01    ; Shape parameter"
    )
  }

  if (distribution == "gompertz") {
    desSection <- c(
      desSection,
      paste0(
        "    DADT(1)=LAM*EXP(GAM*", destination, ") ; Gompertz distribution"
      )
    )
    hazardNow <- paste0("LAM*EXP(GAM*", destinationError, ")")
    thetasSection <- c(
      "$THETA (0,1) ; Scale parameter (1/time unit)",
      "$THETA 0.01    ; Shape parameter"
    )
  }

  errorSection <- c("$ERROR")
  if (distribution == "weibull" || distribution == "weibull_exp") {
    errorSection <- c(
      errorSection,
      "    DELX=1E-12 ;Protect against events as T=0"
    )
  }

  if (rtte) { # RTTE
    errorSection <- c(
      errorSection,
      "",
      "  IF (NEWIND.NE.2) OCHZ=0    ; Reset old cumulative hazard for new subjects",
      "",
      "  IF(EVID.EQ.0) THEN      ; if event observation",
      "    CHZ = A(1)-OCHZ      ; cumulative hazard from previous event time in dataset",
      "    OCHZ = A(1)          ; store CHZ from previous event as OCHZ",
      "    SUR = EXP(-CHZ)      ; survival prob",
      paste0("    HAZNOW = ", hazardNow, "  ; hazard at event time"),
      "    TIMEP = TIME ; save event time",
      "  ENDIF"
    )
    errorSection <- c(
      errorSection,
      "",
      "  IF (EVID.EQ.0.AND.DV.EQ.0) Y=SUR         ; censoring (survival probability)",
      "  IF (EVID.EQ.0.AND.DV.NE.0) Y = SUR*HAZNOW  ; pdf of event variable"
    )
  } else { # TTE
    errorSection <- c(
      errorSection,
      "",
      "  IF(EVID.EQ.0) THEN      ; if event observation",
      "    CHZ = A(1)            ; cumulative hazard",
      "    SURX = EXP(-CHZ)      ; survival prob",
      paste0("    HAZNOW = ", hazardNow, "  ; hazard at event time"),
      "  ENDIF"
    )
    errorSection <- c(
      errorSection,
      "",
      "  IF (EVID.EQ.0.AND.DV.EQ.0) Y=SURX         ; censoring (survival probability)",
      "  IF (EVID.EQ.0.AND.DV.NE.0) Y = SURX*HAZNOW  ; pdf of event variable"
    )
  }

  msfoSection <- paste0("MSFO=msfb_", tools::file_path_sans_ext(outFile))
  estimationSection <- c(
    "$ESTIMATION MAXEVAL=9999 METHOD=0 LIKE SIGL=9 NSIG=3 PRINT=1",
    paste0("    ", msfoSection)
  )
  if (rtte) {
    estimationSection <- c(
      "$ESTIMATION MAXEVAL=9999 METHOD=1 LAPLACE LIKELIHOOD SIGL=9 NSIG=3 PRINT=1",
      paste0("    ", msfoSection)
    )
  }

  omegaSection <- "$OMEGA 0 FIX"
  if (rtte) {
    omegaSection <- "$OMEGA 0.1 ; IIV of scale parameter"
  }

  covarianceSection <- "$COVARIANCE PRINT=E MATRIX=R UNCONDITIONAL"

  tableSection <- ""
  if (includeTable && !is.null(dataFile)) {
    tableSection <- c(
      paste0("$TABLE ", paste0(headers, collapse = " ")),
      paste0(
        "      NOPRINT ONEHEADER FILE=xptab_",
        tools::file_path_sans_ext(outFile)
      )
    )
  }

  lines <- c(
    ";; 1. Based on:",
    paste0(";; 2. Description: ", type, " with ", distribution, " distribution"),
    ";; 3. Label:",
    paste0("$PROBLEM ", type, " with ", distribution, " distribution"),
    inputSection,
    dataSection,
    subroutineSection,
    modelSection,
    pkSection,
    desSection,
    errorSection,
    thetasSection,
    omegaSection,
    estimationSection,
    covarianceSection,
    tableSection
  )

  if (!is.null(outFile)) { # Print simulation file
    con <- file(outFile, open = "w")
    writeLines(lines, con)
    close(con)
    invisible(NULL) # Return NULL invisibly when file is written
  } else { # Or return model code as strings
    return(lines)
  }
}
