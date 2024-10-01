#' Title
#'
#' @param odata the observed data with one row per subject with a time and event indicator (1==event, 0==censored)
#' @param sdata the simulated data with one row per subject and replicate with a time and event indicator (1==event, 0==censored)
#' @param time what indicates event/censoring time
#' @param event what indicates if an event occurred
#' @param iter what indicates the replicate number
#' @param strat character vector: what stratifications need to be applied
#' @param ci what level of confidence to we want for the simulated Kaplan-Meier
#' @param cuminc do we want to show the inverse Kaplan-Meier (1-KM) then set to TRUE, for survival-like set to FALSE
#' @param simCol color for the simulation interval
#' @param obsCol color for the observed line
#' @param posObs provide an explicit set of times to smoothen the CI for simulations
#' @param show.censor logical to indicate whether the censoring times should be shown for the observed
#' @param censor.shape sympol for censored items to be passed to geom point
#' @param censor.size size of the censor item
#' @param palette palette of colors will override the simCol and obsCol arguments
#' @param scale.percent logical to convert y axis to percent
#' @param ylab Y-axis label
#' @param xlab X-axis label
#' @param legend.position the position of the legend, default 'top'
#' @param xlim override the limits of x axis
#' @param ylim override the limits of y axis
#'
#' @return
#' @export
#'
#' @examples
ggKMvpc <- function(odata,
                    sdata,
                    time=TIME,
                    event=DV,
                    iter=ITER,
                    strat=NULL,
                    ci=c(0.05,0.95),
                    cuminc=T,
                    simCol='blue',
                    obsCol='blue',
                    obs.size = 0.8,
                    posObs=NULL,
                    show.censor=T,
                    censor.shape = '|',
                    censor.size = 4,
                    palette = pmx_palettes(),
                    scale.percent = TRUE,
                    ylab = 'Fraction without events (%)',
                    xlab = 'Time (Weeks)',
                    legend.position = 'top',
                    xlim = NULL,
                    ylim = NULL
                    ){

  show.obse=F # logical to indicate whether the uncertainty of observed should be shown. NB: this one differs from the survfit function and RA and JL cannot see why it would, kept here for now

  # group by stratifications if present
  if(!is.null(strat)) {
    strat_gr <- purrr::map(seq_along(strat),function(i) as.name(strat[i]))
    odata <-  odata %>%
      group_by(!!!unlist(strat_gr))
    sdata <-  sdata %>%
      group_by(!!!unlist(strat_gr))

  }

  # rename the time and event columsn
  odata <- droplevels(odata) %>% mutate(event=!!enquo(event),time=!!enquo(time))
  sdata <- droplevels(sdata) %>% mutate(event=!!enquo(event),time=!!enquo(time),ITER=!!enquo(iter))



  # define posible observation times once
  if(is.null(strat)){
    # without stratification the standard is to sequence daily from 0 to the maximum observed time to smooth out the simulations
    if(is.null(posObs)) posObs <- seq(0,odata %>% pull(!!enquo(time)) %>% max)
  } else {

    # it's advised to input factors for predictable behavior, but this is an escape
    # odata[[i]] is the group factor column
    for(i in strat) if(!is.factor(odata[[i]])) odata[[i]] <- as.factor(odata[[i]])
    # to supply error message if the posObs is not understood correctly, but is supplied
    if(!is.null(posObs)&(class(posObs)!="list"|is.null(names(posObs)))){stop("Must provide list named with all combinations of covariate levels for posObs when using stratified VPCs")}
    if(!all(grepl("==",names(posObs)))) stop("Provide names in the list in the form of 'VAR1=='LEVELVAR1'&VAR2=='LEVELVAR2'', where VAR1 and VAR2 refer to the variable names as present in both sdata and odata; LEVELVAR1 and LEVELVAR2 refer to the possible levels of the respective variables. Note that this is exemplified for two stratification variables, but the syntax works for 1 to N number of covariates.")
    if(all(grepl("==",names(posObs)))&!all(grepl("\'",names(posObs)))) stop("It looks like you did not put a \' around the possible factor levels in the list names. Names should be in the form of 'VAR1=='LEVELVAR1'&VAR2=='LEVELVAR2'', where VAR1 and VAR2 refer to the variable names as present in both sdata and odata; LEVELVAR1 and LEVELVAR2 refer to the possible levels of the respective variables. Note that this is exemplified for two stratification variables, but the syntax works for 1 to N number of covariates.")

    # with stratification the standard is to sequence daily from 0 to the maximum observed time within each stratum (combination) to smooth out the simulations
    if(is.null(posObs)){
      # retrieve all combinations
      all_levels = purrr::map(strat,function(i){paste(i,paste0('\'',levels(odata[[i]]),'\''),sep="==")})
      mlength <- max(purrr::map_dbl(all_levels,length))
      # get all levels for each factor
      purrr::map(seq_along(all_levels),function(i){ # seq_along the lists in all_levels
        var= all_levels[[i]]
        if(length(var)<mlength) var <- c(var,rep(NA,(mlength-length(var)) ))
        df_tmp = data.frame(var)
        colnames(df_tmp) = paste0('strat',i)
        df_tmp
      }) %>%
        as.data.frame() -> tmpdf # a list of e.g.CAVCYC1QF=='Q1 [0-194.7]'
      # complete to get all possible combinations
      eval(parse(text=paste0('tidyr::complete(tmpdf,',paste(paste0('strat',1:length(strat)),collapse = ","),")"))) %>%
        # remove the NA rows
        filter(!if_any(1:ncol(.), is.na)) %>%
        # turn into filter expression
        mutate(combi=eval(parse(text=paste0('paste(',paste(paste0('strat',1:length(strat)),collapse = ","),",sep=\'&\')")))) %>%
        pull(combi) -> filterExpr # filter of e.g.CAVCYC1QF=='Q1 [0-194.7]' & XXF=='...'
      # for each filter expression / covariate combination retrieve the time window
      purrr::map(filterExpr,function(i){
        expr <- rlang::parse_expr(i)
        odata %>%
          filter(!!expr) %>%
          pull(time) %>%
          max -> mxtime
        0:mxtime
      }) -> posObs
      names(posObs) <- filterExpr
    }

  }

  grps <- groups(sdata)
  sdata <- ungroup(sdata)
  ### Observed #####

  # add time zero
  odata_add <- bind_rows(odata,odata %>% slice(1) %>% mutate(time=0,event=2))
  # browser()
  # compute KM estimator for observed
  odata_add %>%
    mutate(cens=1-event) %>%
    # arrange by time
    arrange(time,cens) %>%
    # calculate the number of cumulative events
    mutate(cumevent=cumsum(event==1),
           cumcens=cumsum(event==0),
           TOT=n(),
           nrisk=TOT-cumcens-cumevent) %>%
    # find the number of subjects at risk in beginning of time frame and number of events by the end
    group_by(!!!grps,time) %>%
    summarise(nrisk=nrisk[1],cumevent=cumevent[n()],cumcens=cumcens[1],TOT=unique(TOT),.groups = "drop") %>%
    # ungroup the time, but keep the stratification
    group_by(!!!grps) %>%
    # calculate the KM estimator
    mutate(newevent=ifelse(time==0,0,cumevent-lag(cumevent,1)),
           KMT=1-newevent/nrisk,
           KM=cumprod(KMT),
           nrisk1=TOT-lag(cumcens,1),
           greenwood_se=KM * sqrt(cumevent / (nrisk1 * (nrisk1 - cumevent))), # RA: this gives the correct estimate at time <=16
           greenwood_se=ifelse(newevent==0,NA,greenwood_se),
           INC=1-KM) %>%
    tidyr::fill(greenwood_se)-> odata_km

  ### Simulated #####
  NREP = sdata %>% pull(!!enquo(iter)) %>% max



  purrr::map_df(1:NREP,function(r){
    # stime=Sys.time()
    sdata_r <- sdata %>%
      # filter(ITER==r)
      filter(!!enquo(iter)==r)
    # add time zero
    sdata_add <- bind_rows(sdata_r,sdata_r %>% group_by(!!!grps) %>% slice(1) %>% mutate(time=0,event=2))

    # compute KM estimator for observed
    sdata_add %>%
      #
      group_by(!!!grps) %>%
      # arrange by time
      arrange(time) %>%
      # calculate the number of cumulative events
      mutate(cumevent=cumsum(event==1),
             cumcens=cumsum(event==0),
             TOT=n(),
             nrisk=TOT-cumcens-cumevent) %>%
      # find the number of subjects at risk in beginning of time frame and number of events by the end
      group_by(!!!grps,time) %>%
      summarise(nrisk=nrisk[1],cumevent=cumevent[n()],.groups = "drop") %>%
      # ungroup the time, but keep the stratification
      group_by(!!!grps) %>%
      # calculate the KM estimator
      mutate(newevent=ifelse(time==0,0,cumevent-lag(cumevent,1)),
             KMT=1-newevent/nrisk,
             KM=cumprod(KMT)) %>%
      ungroup() -> sdata_km
    # stime2=Sys.time()
    if(is.null(strat)){
      # smoothen using stats::stepfun (code from Jocke) #### find alternative for smoother function
      survatt <- stepfun(sdata_km$time[-1], sdata_km$KM)
      # Apply the function to the KM raw results to smoothen according to posObs
      sdata_km_stepped <- data.frame(time=posObs,KM=survatt(posObs)) %>%
        mutate(INC=1-KM)

    } else{


      # apply the smoother for each stratum combination
      sdata_km_stepped <- purrr::map_df(seq_along(posObs),function(i){
        # filter the data according to the expression (i.e. strata combination)
        expr <- rlang::parse_expr(names(posObs)[i]) #filter
        sdata_km_i <- sdata_km %>%
          filter(!!expr)

      # derive the stepfun to smooth it
        survatt <- stepfun(sdata_km_i$time[-1], sdata_km_i$KM)
        # apply the stepfun
        tmp_df = data.frame(time=posObs[[i]],KM=survatt(posObs[[i]])) %>%
          mutate(INC=1-KM)
        # set the covariate values to the correct value
        cov_settings = strsplit(names(posObs)[i],"&")[[1]]
        for(j in seq_along(cov_settings)){
          set_j = strsplit(cov_settings[j],"==")[[1]]
          tmp_df[[set_j[1]]] = gsub('\'','',set_j[2])
        }
        # return the smoothed kaplan meier for the strata combination
        return(tmp_df)
      })
    }
    # return the kaplan meier for all strata for one replicate
    return(sdata_km_stepped)
  }) -> sdata_km_all

  # we need to re-assign the factors
  for(i in strat){
    if(is.factor(odata[[i]])) sdata_km_all[[i]] <- factor(sdata_km_all[[i]], levels=levels(odata[[i]]))
  }


  # here we summarise
  sdata_km_sum <- sdata_km_all %>%
    group_by(!!!grps,time) %>%
    summarise_at(vars(KM,INC),.funs = list('med'=median,
                                           'lci'=function(x) quantile(x,ci[1]),
                                           'uci'=function(x) quantile(x,ci[2])))
  # browser()
  # this is where we choose the incidence or the survival
  if(cuminc){
    sdata_km_sum$lci = sdata_km_sum$INC_lci
    sdata_km_sum$uci = sdata_km_sum$INC_uci
    sdata_km_sum$med = sdata_km_sum$INC_med
    odata_km$obs     = odata_km$INC
  } else {
    sdata_km_sum$lci = sdata_km_sum$KM_lci
    sdata_km_sum$uci = sdata_km_sum$KM_uci
    sdata_km_sum$med = sdata_km_sum$KM_med
    odata_km$obs     = odata_km$KM

  }

  # this is for the coloring and legend
  names(simCol) = paste0(100*diff(ci),"% CI of simulated")
  names(obsCol) = "Observed"



  # make the plot
  p<- ggplot(odata_km)+
    # ribbon for simulated CI
    geom_ribbon(data=sdata_km_sum,aes(x=time,ymin=lci,ymax=uci,fill=names(simCol)),alpha=0.5)+
    # step function for observed KM
    geom_step(data=odata_km,aes(x=time,y=obs),color='black',size=obs.size)+
    geom_step(data=odata_km,aes(x=time,y=obs,color=names(obsCol)),size=obs.size)+
    #
    scale_color_manual(values = obsCol)+
    scale_fill_manual(values = simCol)+
    theme(legend.position = legend.position,legend.title=element_blank())


  if(show.censor) {
    # we may want to see some censoring
    cens_data <- left_join(odata %>% filter(event==0) %>% distinct(time,!!!grps),
                           odata_km  %>% select(time,obs,!!!grps),by=c("time",as.character(grps)))
    p <- p +
      geom_point(data=cens_data,aes(x=time,y=obs,shape="Censored"),color='black',size=censor.size)+
      scale_shape_manual(values = censor.shape)
  }
  # if we want to show the observed SE
  if(show.obse) {
    # browser()
    p <- p+geom_line(data=odata_km,aes(x=time,y=obs*exp(qnorm(ci[1])*greenwood_se/obs),linetype=paste0(100*diff(ci),"% CI of observed")),color='black',size=0.75)
    p <- p+geom_line(data=odata_km,aes(x=time,y=obs*exp(qnorm(ci[2])*greenwood_se/obs),linetype=paste0(100*diff(ci),"% CI of observed")),color='black',size=1)+
      scale_linetype_manual(values = 'dashed')

  }
  if(!is.null(strat)) {
    # if we have only stratifaction factor show the N by panel
    if(length(strat)==1){
      strats=table(odata[[strat]])
      strats_n <- paste0(names(strats),"\n n = ",strats)
      names(strats_n) <- names(strats)

      p <- p+facet_wrap(reformulate(strat),labeller = as_labeller(strats_n))
    } else {
      p <- p+facet_wrap(reformulate(strat))
    }
  } else {
    # also for unstratified we show the N
    p <- p+facet_wrap(~paste0("n = ",nrow(odata)))
  }
  p$simSum <- sdata_km_sum
  p <- ggpubr::ggpar(p, palette = palette)
  p <- p +
       xlab(xlab) +
       ylab(ylab) +
       coord_cartesian(xlim = xlim, ylim = ylim)
  if(scale.percent){
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  p
}

#prepare data for RTTE
# Sdata <- Sdata %>% group_by(ID) %>% mutate(EVCOUNT = cumsum(DV))
# Sdata <- Sdata %>% group_by(ID) %>% mutate(latestEventTime = lag(TIME)) %>% ungroup() %>% mutate(TimeSincelastEvent = TIME - latestEventTime)
 #Sdata <- Sdata %>% mutate(TimeSincelastEvent2 = ifelse(is.na(TimeSincelastEvent), TIME, TimeSincelastEvent))
