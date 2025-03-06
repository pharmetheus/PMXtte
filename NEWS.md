# PMXtte (development version)

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
