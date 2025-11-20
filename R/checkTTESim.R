#' Check simulations data
#'
#' This function checks several assumptions expected from a TTE/RTTE data set simulated using NONMEM.
#' Ideally, it should be called directly after the NONMEM table is imported in R, before any further modifications.
#' If a check fails, a warning will be returned.
#'
#' @param sdata a data.frame, simulated data. Ideally the output from NONMEM without modifications.
#' @param odata a data.frame, observed data. Only event records. If missing, checks that compare odata and sdata will be omitted.
#' @param rtte a logical, indicating if sdata and odata are repeated time to event data (TRUE).
#' @param idVar,dvVar,timeVar,rtteVar,surVar,iCountVar,iterVar,randVar Character string specifying the name of the columns. Defaulted to the output of a NONMEM code generated with `createTTESim()`.
#'
#' @return Used for its side effects: returns warnings if one or multiple checks fail.
#' @export
#'
#' @details
#' Following assumptions are checked:
#'
#'  * The number of ID is the same within each iteration
#'  * DV are only 0 or 1
#'  * RTTE is computed correctly (given ITER/ID)
#'  * ICOUNT is computed correctly (given ITER/ID)
#'  * IDs are the same in odata and sdata
#'  * Dimension of the sdata is a multiple of the odata (multiple records per ID allowed for RTTE data)
#'
#' @examples
#' # Dummy legit data
#' ex_odata <- data.frame(ID = c(1, 4, 7), DV = c(0, 1, 1), TIME = c(100, 150, 600))
#' ex_sdata_ok <- data.frame(
#'   ID = c(1, 4, 7, 1, 4, 7, 1, 4, 7),
#'   DV = c(1, 0, 0, 0, 1, 0, 0, 0, 1),
#'   TIME = c(50, 400, 700, 100, 200, 700, 100, 400, 300),
#'   RTTE = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   SUR = runif(9), # not used in checks
#'   ICOUNT = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'   ITER = c(1,  1, 1, 2, 2, 2, 3, 3, 3),
#'   RAND = runif(9) # not used in checks
#'   )
#'
#' # If all ok, remains silent
#' checkTTESim(sdata = ex_sdata_ok)
#'
#' # Additional checks if odata is provided
#' checkTTESim(sdata = ex_sdata_ok, odata = ex_odata)
#'
#' # Will warn if error:
#' ex_sdata_wrong <- ex_sdata_ok[-5,] # remove one record
#' \dontrun{
#'   checkTTESim(sdata = ex_sdata_wrong, odata = ex_odata)
#' }
checkTTESim <- function(sdata, odata = NULL, rtte = FALSE,
                        idVar = "ID", dvVar = "DV", timeVar = "TIME",
                        rtteVar = "RTTE", surVar = "SUR", iCountVar = "ICOUNT",
                        iterVar = "ITER", randVar = "RAND"
                        ){
  sdata <- ungroup(sdata)

  # Rule 1. Check that the number of ID is the same within each iteration
  ok1 <- sdata %>%
    group_by(across(all_of(iterVar))) %>%
    summarise(N_ID = n_distinct(.data[[idVar]])) %>%
    summarise(ANSWER = n_distinct(N_ID)==1) %>%
    pull(ANSWER)
  if(!ok1){
    rlang::warn(c(x = "The number of ID within each ITER is not consistant."))
  }

  # Rule 2. DV are only 0 or 1
  ok2 <- all(unique(sdata[[dvVar]]) %in% c(0,1))
  if(!ok2){
    rlang::warn(c(x = "Some values of DV are not equal to 0 or 1."))
  }

  # Rule 3. RTTE is computed correctly
  if(!rtte){ # For TTE data
    ok3 <- all(sdata[[rtteVar]]==1)
  } else { # For RTTE data
    ok3 <- sdata %>%
      group_by(across(all_of(c(iterVar, idVar)))) %>%
      mutate(CORRECT_RTTE = seq_len(n())) %>%
      ungroup() %>%
      mutate(isOK = .data[[rtteVar]]==CORRECT_RTTE) %>%
      pull(isOK) %>%
      all()
  }
  if(!ok3){
    rlang::warn(c(x = "RTTE is not computed correctly."))
  }

  # Rule 4. ICOUNT is computed correctly
  if(!rtte){ # For TTE data
    ok4 <- all(sdata[[iCountVar]]==seq_len(nrow(sdata)))
  } else { # For RTTE data
    correct_icount <- cumsum(!duplicated(interaction(sdata[[iterVar]],sdata[[idVar]])))
    ok4 <- all(sdata[[iCountVar]]==correct_icount)
  }
  if(!ok4){
    rlang::warn(c(x = "ICOUNT is not computed correctly."))
  }

  if(!is.null(odata)){
    odata <- ungroup(odata)

    # Rule 5. IDs are the same in odata and sdata
    ok5 <- isTRUE(all.equal(sort(unique(odata[[idVar]])), sort(unique(sdata[[idVar]]))))
    if(!ok5){
      rlang::warn(c(x = "IDs are not the same in odata and sdata."))
    }

    # Rule 6. Dimension of the sdata is a multiple of the odata
    if(!rtte){ # For TTE data
      ok6 <- (nrow(sdata) %% nrow(odata)) == 0
    } else { # For RTTE data, there can be multiple rows per ID
      nrow_unique_sim <- nrow(distinct(sdata, .data[[iterVar]], .data[[idVar]]))
      nrow_unique_obs <- nrow(distinct(odata, .data[[idVar]]))
      ok6 <- (nrow_unique_sim %% nrow_unique_obs) == 0
    }
    if(!ok6){
      rlang::warn(c(x = "Dimension of the sdata is not a multiple of the odata."))
    }
  }
}
