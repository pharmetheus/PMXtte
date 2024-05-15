
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
#create facet plots and change panel labs
 # RTTEdata2$SEX <- factor(RTTEdata2$SEX, labels = c('alpha^2','FEMALE'), levels = c(0,1))
 #levels(RTTEdata2$SEX) <- c('alpha^2','female')
plotKaplanMeier(RTTEdata, cov_col = 'DOSE', facet.by = 'SEX', panel.labs = list('SEX' = c('male','female')))

#create plot with faceted plots by SEX and exp.tertile
RTTEdata2 <- prep_dataframe(exampledata)
plotKaplanMeier(RTTEdata2, cov_col = 'DOSE', facet.by = c('SEX','exp.tertileF'),panel.labs = list('SEX' = c('alpha^2','female')))


p1 <- plotKaplanMeier(RTTEdata, cov_col = 'DOSE', facet.by = 'SEX', panel.labs = list('SEX' = c('alpha^2','female')))
p1 <- p1 + facet_wrap(~ SEX, labeller = label_parsed)
p1

p1 <- plotKaplanMeier(RTTEdata, cov_col = 'DOSE', facet.by = 'SEX', panel.labs = list('SEX' = c('alpha^2','female')), ncol =1)
