library(dplyr)
library(survminer)
library(survMisc)
library(rmarkdown)
library(xfun)
library(gridExtra)
library(tidyverse)

#original data script
#dataDir <- "ProvidedFiles/"
#dataFile <- "DAT-TTE-1c-PMX-RTTE-LEARN-1.csv"

##########################################################
### Read and subset the data for the task at hand
##########################################################
#RTTEdata <- read.csv(paste0(dataDir,dataFile),na.strings=c(".","-99","NA"))

#data from the package
RTTEdata <- read.csv(system.file('extdata/DAT-TTE-1c-PMX-RTTE-LEARN-1.csv', package= 'PMXtte'),na.strings=c(".","-99","NA"))

# Note: Keep only event information
RTTEdata <- RTTEdata %>% filter(EVID==0&TYPE==0)

# Add dose factors to the data (we arrange it to make the plots look nice and in order)
RTTEdata <- RTTEdata %>%
  mutate(DOSEF = factor(DOSE,levels = sort(unique(RTTEdata$DOSE),decreasing = TRUE),
                        labels = c("400 mg","200 mg", "100 mg", "Placebo")))

fit.FirstEventByArm <- survfit(Surv(TSFDW, DV) ~ DOSEF, data = RTTEdata %>%

                                 # This will keep first record in each subject, which is the same as INCLFIRST==1.
                                 distinct(ID, .keep_all = TRUE))





# ggsurvplot has a lot of options, some are commented out in this script.
# Please refer to the ggsurvplot help "?ggsurvplot" to learn more.

FirstEventByArm <- ggsurvplot(fit=fit.FirstEventByArm,
                              #data          = RTTEdata %>%
                              #distinct(ID, .keep_all = TRUE),
                              xlab          = "Time since first dose (week)",
                              ylab          = "Fraction without events",
                              #font.x        = 25,
                              #font.y        = 25,
                              #font.tickslab = 16,
                              #font.legend   = 20,
                              #cumevents = TRUE,
                              #cumcensor=TRUE,
                              #linetype = "DOSEF",
                              break.time.by = 4,
                              xlim          = c(0,52.1),
                              ylim          = c(0,1),
                              axes.offset   = TRUE,
                              surv.scale    = "percent",
                              #tables.height = 0.15,
                              #facet.by        = c("PROJF"),
                              #short.panel.labs=TRUE,
                              ggtheme         = ggplot2::theme_bw(),
                              censor           = TRUE,
                              #ncol            = 2,
                              conf.int      = TRUE,
                              conf.inf.alpha = 0.9,
                              #palette       = regCols,
                              risk.table    = TRUE,
                              #risk.table.title  = "Hej Siv",
                              risk.table.y.text = TRUE,
                              risk.table.y.text.col = TRUE,
                              pval         = TRUE,
                              pval.method  = TRUE,
                              surv.median.line ="hv",
                              legend        = c(0.25, 0.25),
                              legend.title  = "Dose",
                              legend.labs   = sub(pattern="DOSEF=",
                                                  replacement="",
                                                  x=names(fit.FirstEventByArm$strata))
)

FirstEventByArm
