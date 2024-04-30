
#read dataframe
exampledata <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv', package= 'PMXtte'),na.strings=c(".","-99","NA"))
#prepare data for the plot
RTTEdata <- exampledata %>% dplyr::filter(EVID==0&TYPE==0)
RTTEdata <- RTTEdata %>% dplyr::distinct(ID, .keep_all = TRUE)
#create plot with a single curve
Kaplan_Meier_curves(RTTEdata)
#create plot with stratification by dose
Kaplan_Meier_curves(RTTEdata2, cov_col = "DOSE")
#create plot with faceted plots by SEX
Kaplan_Meier_curves(RTTEdata2, cov_col = "DOSE", facet_by = 'SEX')
