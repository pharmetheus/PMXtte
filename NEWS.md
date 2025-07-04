# PMXtte development version

* `ggKAP(xlim = )` and `ggKAP(ylim = )` are now passed to `ggplot2::coord_cartesian()` instead of `ggplot2::scale_*_continuous()` to preserve the original underlying data set. (#23)

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
