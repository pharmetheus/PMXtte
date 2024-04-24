
#read dataframe
exampledata <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv', package= 'PMXtte'),na.strings=c(".","-99","NA"))
#prepare data for the plot
RTTEdata3 <- prep_dataframe(exampledata)
#plot
Kaplan_Meier_curves(RTTEdata3)
