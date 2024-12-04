
#' KMMC plots for TTE and RTTE models
#'
#' @inheritParams ggKMvpc
#' @param cov
#' @param timevar The x-axis variable. Default is TIME.
#' @param bins The bins of the timevar to calculate the y-variable for.
#' @param fill The fill color for the simulation interval.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' rttedata <- readr::read_csv(system.file('extdata/DAT-1c-RED-1a-PMX-WOWTTE-PFPMX-1.csv', package= 'PMXtte'), show_col_types = FALSE)
#' rttedata <- dplyr::filter(rttedata, EVID == 0, TYPE == 2)
#'
#' # Load observed data
#' odata <- rttedata %>%
#'   dplyr::filter(STUDYIDN == 1) %>%
#'   dplyr::mutate(
#'     TIME = TSFDW,
#'     SEXNF = factor(SEXN, levels = c(1,2), c("Male", "Female"))
#'   )
#'
#' # Load simulated data
#' sdata <- readRDS(system.file('extdata/vpcdat.rds', package= 'PMXtte'))
#' sdata <- sdata %>%
#'   dplyr::mutate(
#'     TIME = TIME / (24*7)
#'   ) %>%
#'   dplyr::group_by(ITER, ID) %>%
#'   dplyr::mutate(EVCOUNT = cumsum(DV)) %>% # Compute EVCOUNT
#'   dplyr::ungroup()
#'
#' odata1 <- odata %>% filter_xth_event(1) %>% mutate(TIME2 = TIME)
#' sdata1 <- sdata %>% filter_xth_event(1) %>%
#'   mutate(TIME2 = TIME) %>%
#'   rename(DOSEN=DOSE)
#'
#' ggKMMC(odata1,
#'        sdata1,
#'        cov = DOSEN,
#'        bins=seq(from=0,to=52,by=1)) +
#'   theme(legend.position="top") +
#'   theme(legend.title = element_blank())
#'
ggKMMC <- function(odata,
                   sdata,
                   cov,
                   timevar = TIME,
                   bins,
                   xlab=waiver(),
                   ylab="Mean of covariate",
                   obsCol = "black",
                   fill = PMXtte:::PMXColors_pmx_palettes(name="light"),
                   ...){

  osum <- kmmc_obs(odata,COV=!!enquo(cov),TIME=!!enquo(timevar),bins,...)
  simsum <- kmmc_sim(sdata,COV=!!enquo(cov),TIME=!!enquo(timevar),bins,...)

  p <- ggplot()+
    geom_rect(data=simsum,aes(xmin=TIMES,xmax=TIMEF,ymin=lci,ymax=uci,fill=TP))+
    geom_step(data=osum,aes(TIMEF,MN,linetype=TP),color=obsCol)+
    xlab(xlab)+
    ylab(ylab)

  p <- suppressMessages(ggpubr::ggpar(p, palette = fill))
  return(p)
}


kmmc_obs <- function(odata,COV,TIME,bins,...){
  map_df(seq_along(bins[-length(bins)]),function(i){
    odata %>%
      filter(TIME>=bins[i]) %>%
      group_by(...) %>%
      summarise(MN=mean(!!enquo(COV)),
                TIME=median(TIME),
                N=n()) %>%
      ungroup() %>%
      mutate(TIMES=bins[i],
             TIMEF=bins[i+1],
             TIMEM=(bins[i]+bins[i+1])/2)
  }) %>%
    mutate(TP="Observed")
}


kmmc_sim <- function(sdata,COV,TIME,bins,...){
  map_df(seq_along(bins[-length(bins)]),function(i){
    sdata %>%
      filter(TIME>=bins[i]) %>%
      group_by(ITER,...) %>%
      summarise(MN=mean(!!enquo(COV)),
                TIME=median(TIME)) %>%
      group_by(...) %>%
      summarise_at(vars(MN),.funs = list("med"=function(x) quantile(x,0.5),
                                         "lci"=function(x) quantile(x,0.05),
                                         "uci"=function(x) quantile(x,0.95)))%>%
      ungroup() %>%
      mutate(TIMES=bins[i],
             TIMEF=bins[i+1],
             TIMEM=(bins[i]+bins[i+1])/2)
  }) %>%
    mutate(TP="Simulated")
}

