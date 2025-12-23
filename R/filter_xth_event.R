#' Filter time to Xth event data
#' @description Filter a repeated time to event dataset into a time to 1st event, time to 2nd event (etc...) dataset. Particularly useful for summary statistics, Kaplan-Meier plots or Visual Predictive checks of time to second (and more) event data. This function preserves the data.frame/tibble attributes and potential grouping.
#'
#' @param x a dataset, with only observations regarding time to event data. It often means data filtered with `EVID==0` and with a unique "TYPE" value.
#' @param event a integer, one or multiple values, informs if the time to 1st, 2nd, 3rd event (etc...) should be returned. Default is 1 (time to first event data).
#' @param time_since_last_event a character, the name of the column to create that will inform the time since the last event. If `NULL`, no column will be created.
#' @param time_col a character, the name of the time column used to calculate the time since last event. Ignored if `time_since_last_event=NULL` or if `event=1`.
#' @param id_col a character, the name of the column to identify a subject.
#' @param iter_col a character, the name of the column to identify replicates of the data set. Useful to apply on simulated (VPC) data.
#' @param rtte_col a character, the name of the column that will inform the event number (time to 1st, 2nd, 3rd). If missing, will be created from `id_col` and `iter_col`.
#'
#' @return a dataset, filtered for the analysis of (a) particular event number(s) (time to 1st event, time to 2nd event, etc...). If missing, `RTTE` and `TLSE` (the default) will be created.
#' @export
#'
#' @examples
#' library(magrittr)
#' # Read data
#' rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
#' # Filter observations
#' rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)
#'
#'
#' # By default, filters event 1
#' filter_xth_event(rttedata) # By default, filters event 1
#' filter_xth_event(rttedata, event = 2)
#' filter_xth_event(rttedata, 3) # etc... # The number of ID decreases
#'
#' filter_xth_event(rttedata, 1:3) # Max 3 events per ID
#'
#' rttedata %>%
#'   filter_xth_event(1:3) %>%
#'   dplyr::mutate(
#'     RTTEF = factor_rtte(RTTE)
#'   ) %>%
#'   ggKAP( # KPM plot of time to second event data
#'     facet_var = "RTTEF",
#'     time_var = "TSLE", #x-axis: time since Last event (not first dose)
#'     label_x = "Time since first (1) or last (2-3) event (weeks)", # in week because `time_col="TSFDW"`
#'     show_pval = FALSE, show_median = FALSE, show_se = FALSE
#'   )
#'
filter_xth_event <- function(
    x,
    event = 1,
    time_since_last_event = "TSLE",
    time_col = "TSFDW",
    id_col = "ID",
    iter_col = "ITER",
    rtte_col = "RTTE"
){

  if(!id_col %in% colnames(x)){
    stop("`id_col = \"", id_col, "\"`, but column ", id_col, " not found in data.")
  }

  # Group data per ID and possibly ITER (for simulated data)
  ans <- x %>%
    dplyr::group_by(dplyr::across(c(dplyr::all_of(c(id_col)), dplyr::any_of(iter_col))))

  # Create/Use RTTE
  if(!rtte_col %in% colnames(x)){
    ans <- ans %>%
      dplyr::mutate(tmpRTTE = seq_len(dplyr::n()))
    names(ans)[names(ans)=="tmpRTTE"] <- rtte_col
  }

  # Create TSLE if not null
  if(!is.null(time_since_last_event)){
    if(time_since_last_event %in% colnames(x)){
      stop("`time_since_last_event = \"", time_since_last_event, "\"`, but ", time_since_last_event, " already exists in the data.")
    }
    if(!time_col %in% colnames(x)){
      stop("`time_col = \"", time_col, "\"`, but column ", time_col, " not found in data.")
    }

    ans <- ans %>%
      dplyr::mutate(tmpTSLE = .data[[time_col]] - dplyr::lag(.data[[time_col]], n = 1, default = 0), .after = all_of(time_col)
      )
    names(ans)[names(ans)=="tmpTSLE"] <- time_since_last_event
  }

  # Filter out records
  ans <- ans %>%
    ungroup() %>%
    filter(.data[[rtte_col]] %in% event)

  # Restore original attributes (tibble as tibble, grouping variables)
  if(!inherits(x, "tbl")){
    ans <- as.data.frame(ans)
  } else {
    ans <- ans %>%
      group_by(across(all_of(group_vars(x))))
  }

  ans
}

isRTTE <- function(x, myID = "ID"){
  any(duplicated(x[x$EVID==0,][[myID]]))
}


