#' Filter time to Xth event data
#' @description Filter a repeated time to event dataset into a time to 1st event, time to 2nd event (etc...) dataset. Particularly useful for summary statistics, Kaplan-Meier plots or Visual Predictive checks of time to second (and more) event data. This function preserves the data.frame/tibble attributes and potential grouping.
#'
#' @param x a dataset, with only observations regarding time to event data. It often means data filtered with `EVID==0` and with a unique "TYPE" value.
#' @param event a integer of length 1, informs if the time to 1st, 2nd, 3rd event (etc...) should be returned. Default is 1 (time to first event data).
#' @param event_col a character, the event column in the data.
#' @param evcount_col  a character, the event count column in the data. Expected as the cumulative sum of the event column (i.e. starts at 0, not incremented of +1 in case of censoring event).
#' @param time_since_last_event a character, the name of the column to create that will inform the time since the last event. If `NULL`, no column will be created. Ignored if `event=1`.
#' @param time_col a character, the name of the time column used to calculate the time since last event. Ignored if `time_since_last_event=NULL` or if `event=1`.
#'
#' @return a dataset, filtered for the analysis of a particular event number (time to 1st event, time to 2nd event, etc...). If the event number (`event`) is greater than 1, a new column that informs the time since the last event is created (default is `TSLE`, based on `TSFDW`).
#' @export
#'
#' @examples
#' library(magrittr)
#' # Read data
#' rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
#' # Filter observations
#' rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)
#' filter_xth_event(rttedata) # By default, filters event 1
#' filter_xth_event(rttedata, event = 2)
#' filter_xth_event(rttedata, event = 3) # etc... # The number of ID decreases
#'
#' rttedata %>%
#'   filter_xth_event(2) %>%
#'   plotKaplanMeier( # KPM plot of time to second event data
#'     cov_col = "DOSEN",
#'     time_col = "TSLE", #x-axis: time since Last event (not first dose),
#'     xlab = "Time since last event (week)" #in week because `time_col="TSFDW"`
#'   )
#'
filter_xth_event <- function(
    x,
    event = 1,
    event_col = "DV",
    evcount_col = "EVCOUNT",
    time_since_last_event = "TSLE",
    time_col = "TSFDW"
){
  if(length(event)!=1){
    stop("`event` is not of length 1. Only a single value can be passed at a time.")
  }
  if(is.null(x[[event_col]])){
    stop("`event_col = \"", event_col, "\"`, but column ", event_col, " not found in data.")
  }
  if(is.null(x[[evcount_col]])){
    stop("`evcount_col = \"", evcount_col, "\"`, but column ", evcount_col, " not found in data.")
  }
  ans <- x
  if(event > 1){
        ans <- x %>%
      dplyr::group_by(!!quo(ID), .add = TRUE) %>%
      filter(any(.data[[event_col]]==1 & .data[[evcount_col]]==(event-1))) %>%
      dplyr::ungroup(!!quo(ID))
    if(!is.null(time_since_last_event)){
      if(!is.null(x[[time_since_last_event]])){
        stop("`time_since_last_event = \"", time_since_last_event, "\"`, but ", time_since_last_event, " already exists in the data.")
      }
      if(is.null(x[[time_col]])){
        stop("`time_col = \"", time_col, "\"`, but column ", time_col, " not found in data.")
      }
      ans <- ans %>%
        dplyr::group_by(!!quo(ID), .add = TRUE) %>%
        mutate(
          tmpTSLE = (.data[[time_col]])-.data[[time_col]][.data[[event_col]]==1&.data[[evcount_col]]==(event-1)],
          .after = all_of(time_col)
        ) %>%
        dplyr::ungroup(!!quo(ID))
      names(ans)[names(ans)=="tmpTSLE"] <- time_since_last_event
    }
  }
  ans <- ans %>%
    filter((.data[[event_col]]==1&.data[[evcount_col]]==event)|(.data[[event_col]]==0&.data[[evcount_col]]==(event-1)))
  if(!inherits(x, "tbl")){
    ans <- as.data.frame(ans)
  }
  ans
}

isRTTE <- function(x, myID = "ID"){
  any(duplicated(x[x$EVID==0,][[myID]]))
}


