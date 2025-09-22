# PMXtte 0.14.2

* `makeSummaryTableTTE(myID = , myDV = )` now define the column with the subject number and the column of the dependent variable. It was the intended behavior originally but the implementation was wrong. (#31)
* `summaryFollowUpTime()` now rounds the total follow-up times after calculating the event rate. It rounded follow-up time first, possible rounding to 0 years of follow-up and an event rate of Infinity. (#32)
* `summaryCountRTTE()` now correctly lumps the columns if the number of events is > 9. (#33)
* `summaryCountRTTE()` now returns correct column names if these are lumped into a "X and more" category. (#34)
* New `ggKAP(scale_y_risktable_reverse = FALSE)`. If `TRUE`, reverses the y axis of the risk table. Useful to have the first levels of a vector from the first line and not from the bottom. Default remains `FALSE` to preserve reproducibility. (#35)
* `createTTESim()` now provides a NONMEM code where the time of previous event in $DES is updated whenever a new event is simulated (using COM(1)), and where the time since last event is computed as absolute value to avoid potential negative times. This fixes a bug that was identified on several data sets where simulations crashed. New `createTTESim(timepVar = "TIMEP")` to identify the variable that corresponds to the time of previous event. 
* `createTTESim()` now provides a NONMEM code for TTE where SURX2, instead of SURX, is used to simulate a censoring event. This avoids the assumption of a definition of SURX in the legacy code, and make the simulation code more reliable.
* New `createTTESim(commentERROR = TRUE)` to comment out the original $ERROR block.
* 

# PMXtte 0.14.1

* `ggKAP(xlim = )` and `ggKAP(ylim = )` are now passed to `ggplot2::coord_cartesian()` instead of `ggplot2::scale_*_continuous()` to preserve the original underlying data set. (#23)
* New `ggKAP(pval_pos = )` to manually define the coordinate position of the p-value on the plot. (#24)
* `ggKAP(label_y_risk = )` now specifically updates the name of the y-axis of the risk table, independently of `label_color`. It was the intended behavior originally but the implementation was wrong. (#25)
* `createTTESim()` now implements the simulation file name from the base name of the model file, ignoring the entire path if provided. (#28)
* New `makeSummaryTableTTE(avnObsPercent = TRUE)` to show the proportion of events as a percentage. (#29)
* Better handling of tied times for the calculation of survival, especially if both censoring and event are observed at the same time. Internally, 0 are reorder after 1 for the same given time. Better handling incalculable survival values by explicitly keeping NA in the step function. Tested over random 100 dataset with tied times. Better handling of tied times for the calculation of the number at risk, now taking the first number of a vector if tied. Tested against `survival::survdiff()` too.


# PMXtte 0.14.0

* New `ggKAP()` function to visualize time-to-event data, either from observed or simulated data.
* New `stat_kaplanmeier()`, `stat_kaplanmeier_censor()`, `stat_kaplanmeier_median()`, `stat_kaplanmeier_pval()`, `stat_kaplanmeier_risktable()`, `stat_kaplanmeier_se()`, `stat_kaplanmeier_sim()`, low-level implementation of Kaplan-Meier curve elements.
* Start deprecating `plotKaplanMeier()` and `ggKMvpc()`.

# PMXtte 0.13.2

* Editorial changes to the documentaiton

# PMXtte 0.13.1.9000 Development version

* `summaryFollowUpTime()` arguments that inherited their documentation from `makeSummaryTable()` despite having different default values are now properly documented.
* Add `PMXFrem` as a suggested package, it is required to run `createTTESim()`.

# PMXtte 0.13.1

* Minor udpates to funciton documentation

# PMXtte 0.13.0

* makeSummaryTableTTE()
  Changed the "c" footnote:
  From:"\\textsuperscript{c}Proportion of number of event"
  To: "\\textsuperscript{c}Proportion of events"

* summaryFollowUpTime()
  Changed the footnote to
  To: \textsuperscript{a}Number of events\newline\textsuperscript{b}Total observation period
  calculated as sum of individual observation period per
  row.\newline\textsuperscript{c}Annual event rate calculated, for each row, as the number
  of events divided by total the total observation period.
  
  Changed:
  From: fTimeColNm = "\\textbf{Observation period (year)\\textsuperscript{b}}",
  To: fTimeColNm = "\\textbf{Observation period (years)\\textsuperscript{b}}",
  
  Changes to plotKaplanMeier
* Changes in legend: Survival curves -> Observed
* Changes in legend: Added Censored (default |)
* Changes in legend: Removed censor sympol from confidence intervals
* Added arguments censor.shape, censor.size and risk.table.title
* Set the default value for risk.table.title to "Number of subjects at risk"

# PMXtte 0.12.0

* Changes to `makeSummaryTable` so that a column is not printed for RTTE.
* Change default for `avnObsColNm` to `\\textbf{pEvent\\textsuperscript{c}}`
* Changed default axis labels in `plotKaplanMeier` and `ggKMvpc`.
* Changed default values of `caption`, `fTimeColNm`, `EventRateColNm`, `footnote` in `summaryFollowUpTime`.
* Changed `DOSEF` to `DOSENF` and `STUDYF` to `STUDYNF` in the documentation to a number of functions.
* Full refactoring of `ggKMMC()`. The function now calculates the covariate mean at different time points from a step function (instead of binning the observed and simulated data). This enable to deal with time-varying covariates if a data set with time varying values is provided. It can also output the results as a data.frame, and has an argument to use another statistic than the "mean" value.
* `filter_xth_event(evcount_col)` is now defaulted to `NULL`. If so, the column `EVCOUNT` will be used or re-calculated as the cumulative number of event for an individual.
* Added `createTTEStart()`, which is a function to create the starting model for a (R)TTE analysis.
* Added `createTTESim()`, which is a function to create a simulation (R)TTE model file from an existing model file.

# PMXtte 0.11.0

* Added a `NEWS.md` file to track changes to the package.
* Fixed the labeling of the simulated CI in kmmc and ggKMvpc.
