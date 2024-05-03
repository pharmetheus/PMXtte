
#read dataframe
exampledata <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv', package= 'PMXtte'),na.strings=c(".","-99","NA"))
#prepare data for the plot
RTTEdata <- exampledata %>% dplyr::filter(EVID==0&TYPE==0)
RTTEdata <- RTTEdata %>% dplyr::distinct(ID, .keep_all = TRUE)
#create plot with a single curve
plotKaplanMeier(RTTEdata)
#create plot with stratification by dose
plotKaplanMeier(RTTEdata, cov_col = "DOSE")
#create plot with faceted plots by SEX
plotKaplanMeier(RTTEdata, cov_col = "DOSE", facet.by = 'SEX')
#create facet plots changing panel labs
plotKaplanMeier(RTTEdata, cov_col = 'DOSE', facet.by = 'SEX', panel.labs = list('SEX' = c('MALE','Female')))
#create plot with faceted plots by SEX and exp.tertile
RTTEdata2 <- prep_dataframe(exampledata)
plotKaplanMeier(RTTEdata2, cov_col = 'DOSE', facet.by = c('SEX','exp.tertileF'))


