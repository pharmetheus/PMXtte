#' Create NONMEM control stream for TTE simulations based on a model file
#'
#' This function generates a NONMEM control stream file for simulating
#' time-to-event (TTE) or repeated time-to-event (RTTE) data using the MTIME
#' technique and is based on an existing NONMEM model file. The function
#' modifies (adding and revising) the model file to enable simulations and
#' output simulated event times and covariates. Default is to change to
#' subroutine ADVAN6 in the simulations since other subroutines have been
#' shown not to work adequately for this purpose. Ensure to use the option
#' endTimeVar to define COM(4) correctly (endTimeVar = "ENDTIME" is the default),
#' since NONMEM will not complain if a variable not available in $INPUT is used.
#'
#' Note also that the created model file may need some further manual editing,
#' eg, in the $DATA section with respect to IGNORE statements; scrutinize your code.
#'
#' @description This function generates a NONMEM control stream file for
#'   simulating time-to-event (TTE) or repeated time-to-event (RTTE) data
#'   based on an existing NONMEM model file. It modifies the model file to
#'   perform simulations, outputting simulated event times and covariates.
#'
#' @param modFile Character string specifying the filename (and path) to the
#'   NONMEM model file to use as a basis for the survival simulations.
#' @param outFile Character string specifying the filename (and path) for the
#'   NONMEM simulation file to be written to disk. If `NULL`, no file is
#'   written, and instead, the modified model code is returned as a character
#'   vector. Default is `<modFile>vpc.mod`.
#' @param simTabFile Character string specifying the filename for the simulation
#'   output file (the simulated events). Default: `vpctab<modFile_without_extension>`.
#' @param rtte Logical, indicating whether to perform repeated time-to-event
#'   simulations (`TRUE`) or standard time-to-event simulations (`FALSE`, default).
#' @param mTimeResolution Numeric value specifying the minimum resolution in the
#'   event time. Default `0.1` time units.
#' @param timeVar Character string specifying the name of the TIME column.
#'   Default `"TIME"`. Used for outputting the header `TIME` of the event in
#'   the `simTabFile` and for `MTIME` update.
#' @param tempVar Character string specifying a temporary variable name used to
#'   update `MTIME`. Default `"TEMP"`.
#' @param idVar Character string specifying the name of the ID column.
#'   Default `"ID"`. Used for outputting the individual ID header in the `simTabFile`.
#' @param dvVar Character string specifying the name of the DV column.
#'   Default `"DV"`. Used for outputting the header in the `simTabFile`.
#' @param rtteVar Character string specifying the name of the RTTE variable.
#'   Default `"RTTE"`. To be used in the code and in the header for the `simTabFile`.
#' @param iCountVar Character string specifying the name of the Individual counter.
#'   Default `"ICOUNT"`. To be used as header in the `simTabFile`.
#' @param iterVar Character string specifying the name of the simulation iteration
#'   counter. Default `"ITER"`. To be used in the code and header in the `simTabFile`.
#' @param tmpDvVar Character string specifying a temporary variable name.
#'   Default `"TDV"`. To be used to store the DV value for output, also used as `<TDV>X` in the code.
#' @param timepVar Character string specifying the "previous event time" variable
#'  name in the case of repeated time-to-event simulations. Default `"TIMEP"`.
#'  To be searched in the `$DES` and replaced by `COM(1)`.
#' @param baselineCovs Character vector of strings for all baseline covariates
#'   to output in the `simTabFile`, e.g., `c("BCOV1","BCOV2")`. Default `NULL`,
#'   i.e., no baseline covariates to output.
#' @param timeVaryingCovs Character vector of strings for all time-varying
#'   covariates to output in the `simTabFile`, e.g., `c("TCOV1","TCOV2")`.
#'   Default `NULL`, i.e., no time-varying covariates to output.
#' @param endTimeVar Character string specifying the name of the max event time
#'   for each individual. Default `"ENDTIME"`. Could also be set to a specific
#'   time (e.g., `"365"`), note though that the time should be less or equal to
#'   the last record per subject.
#' @param mTimeCount Integer specifying the `MTIME` vector index to use.
#'   Default `1`. Change only if multiple `MTIME` statements.
#' @param filePointer Integer specifying the Fortran file pointer to use for
#'   `simTabFile`. Default `99`. Change only if `99` is already used in NONMEM
#'   verbatim code.
#' @param randomSource Integer specifying the random source index for the
#'   univariate random number. Default `2`. Change only if additional random
#'   sources are used in the NONMEM code.
#' @param randomVar Character string specifying the random variable name to use.
#'   Default `"R"`.
#' @param addSimRecord Logical, indicating whether a `$SIMULATION` record should
#'   be added/replaced in the code. Default `TRUE`.
#' @param removeEstRecord Logical, indicating whether the `$ESTIMATION` record
#'   should be removed from the code. Default `TRUE`.
#' @param seed Integer specifying the R seed to be used to generate the NONMEM
#'   seeds. Default `1234`.
#' @param nSubs Integer specifying the number of simulation iterations (subproblems)
#'   to be used.
#' @param hzdCompartment Character string specifying the NONMEM expression for
#'   the hazard compartment, representing the cumulative hazard. Default `"A(1)"`.
#' @param surVar Character string specifying the survival variable name to use
#'   in the NONMEM code. Default `"SUR"`.
#' @param surVarError Character string specifying the survival variable name to
#'   use in the NONMEM code in `$ERROR`. Default `"SURX"`.
#' @param fortranOutputFormat Character string specifying the FORTRAN output
#'   format for the `simTabFile`. Default Scientific notation with 7 decimals `"E13.7"`.
#' @param updateInits Logical, indicating whether the simulation parameters
#'   should be updated to the final estimates of the estimation. Assumes that a
#'   `<modFile>.ext` exists in the `modFile` path. Default `TRUE`.
#' @param includeThetaComments Logical, only applies when `updateInits = TRUE`.
#'   If `TRUE`, tries to get the corresponding theta comments (text following
#'   first ";") from the `modFile` and add to the new updated simulation
#'   parameters. Default `TRUE`.
#' @param includeTABLE Logical, indicating whether `$TABLE` should be included
#'   or not. Default `FALSE`.
#' @param replaceSUB Logical, indicating whether to replace `$SUBROUTINE` to
#'   make sure it is using `ADVAN6`. Default `TRUE`.
#' @param commentERROR Logical, indicating whether to comment out the lines of
#'  original `$ERROR` block. Default `TRUE`.
#'
#' @return If `outFile` is specified, the function writes the NONMEM control
#'   stream to the specified file and returns `NULL` invisibly. If `outFile` is
#'   `NULL`, the function returns the NONMEM control stream as a character vector.
#' @export
#'
#' @examples
#' # Example usage: RTTE with time-varying DOSEN covariate in output, not updating initial estimates
#' # File path to a ".mod" object (embedded in the package as an example)
#' mypath <- system.file("extdata", "rtte_mod.mod", package = "PMXtte")
#' createTTESim(modFile = mypath, rtte = TRUE, timeVaryingCovs = "DOSEN", updateInits = FALSE)
#'
#' # Example usage: TTE with no covariates in output
#' mypath <- system.file("extdata", "tte_weibull.mod", package = "PMXtte")
#' createTTESim(modFile = mypath)
createTTESim <- function(modFile,
                         outFile = paste0(tools::file_path_sans_ext(modFile), "vpc.mod"),
                         simTabFile = PMXtte:::make_simtab_name(modFile),
                         rtte = FALSE,
                         mTimeResolution = 0.1,
                         timeVar = "TIME",
                         tempVar = "TEMP",
                         idVar = "ID",
                         dvVar = "DV",
                         rtteVar = "RTTE",
                         iCountVar = "ICOUNT",
                         iterVar = "ITER",
                         tmpDvVar = "TDV",
                         timepVar = "TIMEP",
                         baselineCovs = NULL,
                         timeVaryingCovs = NULL,
                         endTimeVar = "ENDTIME",
                         mTimeCount = 1,
                         filePointer = 99,
                         randomSource = 2,
                         randomVar = "R",
                         addSimRecord = TRUE,
                         removeEstRecord = TRUE,
                         seed = 1234,
                         nSubs = 100,
                         hzdCompartment = "A(1)",
                         surVar = "SUR",
                         surVarError = "SURX",
                         fortranOutputFormat = "E13.7",
                         updateInits = TRUE,
                         includeThetaComments = TRUE,
                         includeTABLE = FALSE,
                         replaceSUB = TRUE,
                         commentERROR = TRUE) {

  if(!requireNamespace("PMXFrem", quietly = TRUE)) {
    stop(
      "Package \"PMXFrem\" must be installed to use \"PMXtte::createTTESim()\" ",
      call. = FALSE
    )
  }

  if (!file.exists(modFile)) {
    stop(paste0("Cannot find model file: ", modFile))
  }
  if (updateInits && !file.exists(paste0(tools::file_path_sans_ext(modFile), ".ext"))) {
    stop(paste0("Cannot find ext file for updateInits: ", paste0(tools::file_path_sans_ext(modFile), ".ext")))
  }

  # Make a copy of the modFile
  con <- file(modFile, open = "r")
  line <- readLines(con)
  close(con)

  if (length(PMXFrem::findrecord(line, "\\$PK")) == 0) {
    stop("No \\$PK record identified")
  }
  if (length(PMXFrem::findrecord(line, "\\$DES")) == 0) {
    stop("No \\$DES record identified")
  }

  # Adding initial comments to the user
   linesProblem <- PMXFrem::findrecord(line, "\\$PROBLEM")
  line <- PMXFrem::findrecord(line, "\\$PROBLEM",
                              replace = c(
                                linesProblem,
                                "; THIS FILE MAY NEED SOME MANUAL EDITING, EG,",
                                "; CONSIDER IF $DATA NEEDS UPDATE OF IGNORE STATEMENTS"
                              )
  )

  # Get $ABBREVIATED
  linesAbb <- PMXFrem::findrecord(line, "\\$ABB")
  newLinesAbb <- c(paste0("$ABB COMRES=", 7 + length(timeVaryingCovs)))
  if (length(linesAbb) != 0) {
    warning("$ABBR is overwritten by simulation COMRES, please check the $ABB code manually")
  }


  if (replaceSUB) {
    # Get $SUBROUTINE
    linesSub <- PMXFrem::findrecord(line, "\\$SUB")
    if (length(linesSub) > 0) {
      linesSub <- gsub("ADVAN\\s*=\\s*\\d+", "ADVAN=6", linesSub) # Replace ADVAN = XX with ADVAN=6
      linesSub <- gsub("ADVAN\\d+", "ADVAN=6", linesSub) # Replace ADVANXX with ADVAN6
      line <- PMXFrem::findrecord(line, "\\$SUB", replace = linesSub)
    }
  } else {
    warning("Note that other ADVAN's than ADVAN6 might not work properly with MTIME simulations")
  }

  # Get $PK
  linesPk <- PMXFrem::findrecord(line, "\\$PK")

  headerCovs <- c(
    idVar, dvVar, timeVar, rtteVar, "SUR", iCountVar, iterVar, "RAND",
    baselineCovs, timeVaryingCovs
  )
  # Build Open file code
  linesOpenFile <- c(
    "IF (NEWIND.EQ.0) THEN      ; Only for the first record",
    "  COM(5) = (IREP-1)*NINDR+1 ; Reset simulation ID counter",
    "\"  ! Initialize sim output file",
    paste0("\"  OPEN (", filePointer, ", FILE = '", simTabFile, "', POSITION='APPEND')"),
    "\"  IF (IREP.EQ.1) THEN !Write header for 1st subproblem",
    paste0("\"    WRITE (", filePointer, ",'(A,", length(headerCovs) - 1, "(1XA))') ", paste0("'", paste0(headerCovs, collapse = "','"), "'")),
    "\"  ENDIF",
    "ENDIF"
  )

  initCOM1 <- "-1"
  if(rtte) initCOM1 <- "0"

  # Build init code
  linesInit <- c(
    "IF (NEWIND.EQ.1) THEN      ; For every new ind except first in dataset",
    "  COM(5) = COM(5) + 1  ; Update individual counter over simulations",
    "ENDIF",
    "",
    "IF (ICALL.EQ.4) THEN ",
    " IF (NEWIND.NE.2) THEN      ; For every new individual",
    paste0("  CALL RANDOM(", randomSource, ",", randomVar, ")"),
    paste0("  COM(4) = ", endTimeVar, " ; Maxtime per individual (in hours)"),
    paste0("  COM(3) = -1          ; Variable for survival at event time"),
    paste0("  COM(2) = ", randomVar, "; Store the random number"),
    paste0("  COM(1) = ", initCOM1, "            ; Variable for the event time"),
    paste0("  COM(6) = 0            ; Individual event counter"),
    paste0("  COM(7) = 0            ; Cumulative hazard"),
    " ENDIF",
    "ENDIF",
    "",
    ";Store iteration num (dataset sim num)",
    paste0(iterVar, " = IREP")
  )


  # Build MTIME Code
  linesMTime <- c(
    ";---------MTIME for increasing $DES precision --------",
    paste0("IF (TIME.EQ.0) ", tempVar, "=0"),
    paste0(tempVar, "=", tempVar, "+", mTimeResolution),
    paste0("MTIME(", mTimeCount, ")=", tempVar),
    "MTDIFF=1",
    ""
  )

  lineTVCovs <- ""
  # Add COM variables for time-varying variables for output
  if (length(timeVaryingCovs) > 0) {
    for (i in seq_along(timeVaryingCovs)) {
      lineTVCovs <- c(lineTVCovs, paste0("COM(", 7 + i, ")=", timeVaryingCovs[i]))
    }

    # For TTE (not RTTE), a wrapper is needed
    if(!rtte){
      lineTVCovs <- c("IF (COM(1).EQ.-1) THEN ; IF NO EVENT SIMULATED YET",
                      paste0(" ", lineTVCovs), "ENDIF")
    }
  }

  # Concatenate the new lines of the $PK
  linesPk <- c(
      newLinesAbb, linesPk[1], linesOpenFile, linesInit, linesPk[2:length(linesPk)],
      linesMTime, lineTVCovs
    )

  # Update $PK with MTIME and Open file
  line <- PMXFrem::findrecord(line, "\\$PK", replace = linesPk)

  # Create SIM Record
  if (addSimRecord) {
    set.seed(seed)
    simRecord <- paste0(
      "$SIMULATION (", round(runif(1, min = 0, max = 10000000), 0), ") (",
      round(runif(1, min = 0, max = 10000000), 0), " UNIFORM) ONLYSIMULATION NOPREDICTION NSUB=",
      nSubs
    )
  }

  if (removeEstRecord) { # Remove $EST record and possible replace with $SIM record
    if (length(PMXFrem::findrecord(line, "\\$EST")) == 0 && addSimRecord) {
      line <- c(line, simRecord) # Add $SIMULATION record to bottom of code
    }
    if (length(PMXFrem::findrecord(line, "\\$EST")) > 0 && addSimRecord) {
      line <- PMXFrem::findrecord(line, "\\$EST", replace = simRecord)
    }
    if (length(PMXFrem::findrecord(line, "\\$EST")) > 0 && !addSimRecord) {
      line <- PMXFrem::findrecord(line, "\\$EST", replace = "")
    }
    line <- PMXFrem::findrecord(line, "\\$COV", replace = "")
  } else {
    if (addSimRecord) {
      # Add $SIMULATION record to bottom of code
      line <- c(line, simRecord)
    }
  }

  ### Update initial estimates if possible

  # Get $DES
  linesDes <- PMXFrem::findrecord(line, "\\$DES")

  # Create output line
  outputCovs <- baselineCovs
  if (length(timeVaryingCovs) > 0) {
    outputCovs <- c(outputCovs, paste0("COM(", 7 + seq_along(timeVaryingCovs), ")"))
  }
  addOnCovs <- ""
  if (length(outputCovs) != 0) {
    addOnCovs <- paste0(",", paste0(outputCovs, collapse = ","))
  }

  ### Create $DES code
  if (rtte) {

    # wrap any T-TIMEP with ABS()
    linesDes <- wrap_abs(x = linesDes, timepVar = timepVar)

    # update any "TIMEP" into "COM(1)"
    linesDes <- str_replace_all(linesDes, timepVar, "COM(1)")

    # add simulation specific code
    linesDESCode <- c(
      ";---------- RTTE Simulation specific",
      paste0(surVar, " = EXP(-(", hzdCompartment, "-COM(7))) ; Survival time T"),
      paste0("IF(COM(2).GT.", surVar, ".AND.T.LE.COM(4)) THEN      ; If event write event to output file"),
      "  COM(1)=T              ; Store event time",
      paste0("COM(3)=", surVar, "          ; Store survival"),
      paste0("COM(7)=", hzdCompartment, "          ; Set cumulative hazard"),
      "COM(6)=COM(6)+1          ; Event counter",
      paste0(tmpDvVar, "=1"),
      "\" !Write TTE specific output",
      paste0("\"    ", "WRITE (", filePointer, ",'(", fortranOutputFormat, ",", length(headerCovs) - 1, "(1X", fortranOutputFormat, "))') ", idVar, ",", tmpDvVar, ",COM(1),COM(6),COM(3),COM(5),", iterVar, ",COM(2)", addOnCovs),
      paste0("\"    CALL RANDOM(", randomSource, ",", randomVar, ")"),
      paste0("COM(2)=", randomVar, "            ; Store new random number"),
      "ENDIF",
      ";------END: MTIME and RTTE simulation specifics"
    )

  } else { # TTE
    linesDESCode <- c(
      ";---------- TTE Simulation specific",
      paste0(surVar, " = EXP(-", hzdCompartment, ") ; Survival at this T)"),
      "IF(COM(2).GT.SUR.AND.COM(1).EQ.-1) THEN ; If event, save event time in COM(1)",
      " COM(1)=T",
      paste0(" COM(3)=", surVar),
      # paste0(" ",lineTVCovs), # This line was commented out in original code, keeping it commented here as well
      "ENDIF"
    )
  }

  # Replace $DES
  line <- PMXFrem::findrecord(line, "\\$DES", replace = c(linesDes, linesDESCode))

  # Get $ERROR
  linesError <- PMXFrem::findrecord(line, "\\$ERR")

  if(commentERROR){
    linesError <- c(linesError[1], paste0("; ", linesError[-1]))
  }

  linesClose <- c(
    "IF (NDREC.EQ.LIREC.AND.NIREC.EQ.NINDR) THEN ; Last record for last individual",
    paste0("  CLOSE(", filePointer, ") ; Close File pointer"),
    "ENDIF"
  )


  linesRightCensoring <- c(
    "IF (LIREC.EQ.NDREC) THEN ;Right Censoring",
    paste0(" ", tmpDvVar, "X=0"),
    paste0(" COM(3)=", surVarError, "2; Survival at last record, not neccesarily censoring time"),
    " COM(6)=COM(6)+1 ; Event counter",
    paste0(" ", lineTVCovs),
    paste0("\"", " WRITE (", filePointer, ",'(", fortranOutputFormat, ",", length(headerCovs) - 1, "(1X", fortranOutputFormat, "))') ", idVar, ",", paste0(tmpDvVar, "X"), ",COM(4),COM(6),COM(3),COM(5),", iterVar, ",COM(2)", addOnCovs),
    "ENDIF"
  )

  ### Create $ERROR code
  if (rtte) {
    # Update $ERR with writing records
    line <- PMXFrem::findrecord(
      line, "\\$ERR",
      replace = c(
        linesError, paste0(surVarError, "2 = EXP(-(", hzdCompartment, "-COM(7))) ; Survival at last record"),
        paste0(tmpDvVar, "X=-1"), linesRightCensoring, linesClose
      )
    )
  } else { # TTE
    linesErrorSection <- c(
      paste0(surVarError, "2 = EXP(-(", hzdCompartment, "-COM(7))) ; Survival at last record"),
      "IF (LIREC.EQ.NDREC) THEN ;Last record per individual",
      " IF (COM(1).GT.COM(4)) THEN ;IF T > ENDTIME, T=ENDTIME",
      paste0("  IF (COM(2).GT.", surVarError, "2) THEN"),
      "   COM(1) = COM(4)",
      "  ELSE ",
      "   COM(1) = -1 ;Integrated too far, reset event",
      "  ENDIF",
      " ENDIF",
      "  IF (COM(1).NE.-1) THEN ;If an EVENT ",
      paste0("  ", tmpDvVar, "X=1"),
      paste0("  ", rtteVar, "=1 "),
      " ; Write event specific output",
      paste0("\"      ", "WRITE (", filePointer, ",'(", fortranOutputFormat, ",", length(headerCovs) - 1, "(1X", fortranOutputFormat, "))') ", idVar, ",", paste0(tmpDvVar, "X"), ",COM(1),", rtteVar, ",COM(3),COM(5),", iterVar, ",COM(2)", addOnCovs),
      "  ELSE",
      paste0("  ", linesRightCensoring),
      "  ENDIF",
      "ENDIF"
    ) # Update $ERR with writing records
    line <- PMXFrem::findrecord(line, "\\$ERR", replace = c(linesError, linesErrorSection, linesClose))
  }


  if (updateInits) { # Assuming ext file
    dfExt <- subset(PMXFrem::getExt(extFile = paste0(tools::file_path_sans_ext(modFile), ".ext")), ITERATION == "-1000000000") # Read in parameter values
    dfExtFix <- subset(PMXFrem::getExt(extFile = paste0(tools::file_path_sans_ext(modFile), ".ext")), ITERATION == "-1000000006") # Read in which parameters that are fixed
    iNumTHETA <- length(names(dfExt)[grepl("THETA.*", names(dfExt))])
    iNumOM <- length(names(dfExt)[grepl("OMEGA.*", names(dfExt))])
    iNumOM <- -1 / 2 + sqrt(1 / 4 + 2 * iNumOM) # Get the number of diagonals from the number of OM elements
    THETA <- as.numeric(dfExt[, names(dfExt)[grepl("THETA.*", names(dfExt))]])
    THETAFIX <- as.numeric(dfExtFix[, names(dfExtFix)[grepl("THETA.*", names(dfExtFix))]])
    OMEGA <- as.numeric(dfExt[, names(dfExt)[grepl("OMEGA.*", names(dfExt))]])

    OM <- matrix(0, nrow = iNumOM, ncol = iNumOM) # Define an empty matrix
    OM[upper.tri(OM, diag = TRUE)] <- OMEGA # Assign upper triangular + diag
    tOM <- t(OM) # Get a transposed matrix
    OM[lower.tri(OM, diag = FALSE)] <- tOM[lower.tri(tOM, diag = FALSE)] # Assign the lower triangular except diag

    if (includeThetaComments) {
      # Get THETA comments if possible
      lineTh <- PMXFrem::findrecord(line, record = "\\$THETA")
      thComments <- sub("^.*?\\;(.*)", "\\1", lineTh)
      for (i in seq_along(thComments)) {
        if (thComments[i] == lineTh[i]) {
          thComments[i] <- ""
        } else {
          thComments[i] <- paste0(";", thComments[i])
        }
      }

      # Remove empty $THETA rows
      thComments <- thComments[lineTh != ""]
    } else {
      thComments <- rep("", iNumTHETA)
    }

    inputTheta <- ""
    for (i in seq_len(iNumTHETA)) {
      fixValue <- ""
      if (THETAFIX[i] == 1) {
        fixValue <- " FIX"
      }
      inputTheta <- c(inputTheta, paste0("$THETA ", THETA[i], fixValue, " ", thComments[i]))
    }

    # Add new $THETA
    line <- PMXFrem::findrecord(line, record = "\\$THETA", replace = inputTheta)

    # Build OM Matrix
    newOmMatrix <- PMXFrem::buildmatrix(as.matrix(OM))
    # IF TTE, FIX OMEGA
    if (!rtte) {
      newOmMatrix[length(newOmMatrix)] <- paste0(newOmMatrix[length(newOmMatrix)], " FIX")
    }

    # Replace $OMEGA
    line <- PMXFrem::findrecord(line, record = "\\$OMEGA", replace = newOmMatrix)
  }

  if (length(PMXFrem::findrecord(line, "\\$TAB")) > 0 && !includeTABLE) {
    line <- PMXFrem::findrecord(line, "\\$TAB", replace = "")
  }


  if (!is.null(outFile)) { # Print simulation file
    con <- file(outFile, open = "w")
    writeLines(line, con)
    close(con)
    invisible(NULL) # Return NULL invisibly when file is written
  } else { # Or return model code as strings
    return(line)
  }
}

make_simtab_name <- function(x, prefix = "vpctab", suffix = ""){
  paste0(
    prefix,
    tools::file_path_sans_ext(basename(x)),
    suffix
  )
}

wrap_abs <- function(x, timepVar){
  expr <- paste0("T\\s*-\\s*", timepVar)
  pattern <- paste0("\\b", expr,"\\b")
  repl <- paste0("ABS(T-", timepVar,")")
  ans <- stringr::str_replace_all(
    string = x,
    pattern = pattern,
    replacement = repl
  )
  stringr::str_replace_all(
    string = ans, # remove potential double ABS
    pattern = "ABS\\(ABS\\((.*?)\\)\\)",
    replacement = "ABS(\\1)"
  )
}
