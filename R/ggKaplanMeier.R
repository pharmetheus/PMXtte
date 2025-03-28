ggKaplanMeier <- function(data,
                          sdata,
                          time_var = "TIME",
                          dv_var = "DV",
                          iter_var = "ITER",
                          col_var = NULL,
                          facet_var = NULL,
                          show_kpm = TRUE,
                          show_censor = TRUE,
                          show_risktable = TRUE,
                          show_se = missing(sdata),
                          show_pval = missing(sdata)&!is.null(col_var),
                          show_median = missing(sdata),
                          show_sim = !missing(sdata),
                          se_alpha = .5,
                          arrange_function = \(x){
                            cowplot::plot_grid(plotlist = x, nrow = 2, rel_heights = c(4,1), align = "v")
                          }
){

  ..time  <- expr(.data[[time_var]])
  ..dv    <- expr(.data[[dv_var]])

  if(!is.null(col_var)){
    ..col <- expr(.data[[col_var]])
  } else {
    ..col <- NULL
  }

  if(any(show_kpm, show_censor, show_se, show_pval, show_median, show_sim)){
    fig <- ggplot(mapping = aes(x = eval(..time), y = eval(..dv))) +
      theme(legend.position = "top")
  } else {
    fig <- NULL
  }

  if(show_kpm){
    fig <- fig +
      stat_kaplanmeier(
        aes(color = eval(..col)),
        data = data
      )
  }

  if(show_censor){
    fig <- fig +
      stat_kaplanmeier_censor(
        aes(color = eval(..col)),
        data = data
      )
  }

  if(show_se){
    fig <- fig +
      stat_kaplanmeier_se(
        aes(fill = eval(..col)),
        data = data,
        alpha = se_alpha
      )
  }

  if(show_pval){
    fig <- fig +
      stat_kaplanmeier_pval(
        aes(strata = eval(..col)),
        data = data
      )
  }

  if(show_median){
    fig <- fig +
      stat_kaplanmeier_median(
        aes(col = eval(..col)),
        data = data
      )
  }

  if(show_sim){
    ..iter <- expr(.data[[iter_var]])

    fig <- fig +
      stat_kaplanmeier_sim(
        aes(iter = eval(..iter), fill = eval(..col)),
        data = sdata,
        alpha = se_alpha
      )
  }

  if(show_risktable){
    ..y <- ..col
    if(is.null(..y)){
      ..y <- factor("")
    }

    risktab <- ggplot() +
      stat_kaplanmeier_risktable(
        aes(
          x = eval(..time),
          y = eval(..y),
          col = eval(..col)
        ),
        data = data
      ) +
      theme(legend.position = "none")
  } else {
    risktab <- NULL
  }


  if(!is.null(facet_var)){
    ..facet <- expr(.data[[facet_var]])

    fig <- fig +
      facet_wrap(~ eval(..facet))
    risktab <- risktab +
      facet_wrap(~ eval(..facet))
  }

  ans <- list(
    fig = fig,
    risktab = risktab
  )

  ans <- ans[!sapply(ans, is.null)]

  if(length(ans)==1){
    ans <- ans[[1]]
  } else{
    if(!is.null(arrange_function)){
      ans <- arrange_function(x = ans)
    }
  }
  ans
}
