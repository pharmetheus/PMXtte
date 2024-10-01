PMXColors_theme_pmx <- function (base_size = 11)
{
  update_geom_defaults("abline", list(size = 1.25))
  update_geom_defaults("smooth", list(size = 1.25))
  update_geom_defaults("line", list(size = 0.25))
  update_geom_defaults("point", list(shape = 1))
  update_geom_defaults("bar", list(color = "black", fill = "lightgrey",
                                   size = 0.3))
  update_geom_defaults("boxplot", list(color = "black", fill = "lightgrey",
                                       size = 0.3))
  theme_bw(base_size = base_size) %+replace% theme(strip.background = element_rect(fill = "#CBD5D7"),
                                                   panel.grid.minor = element_blank(), plot.background = element_rect(fill = "transparent",
                                                                                                                      color = NA), legend.background = element_blank(),
                                                   legend.box.background = element_rect(fill = "transparent",
                                                                                        color = "transparent"), legend.key = element_rect(colour = "transparent",
                                                                                                                                          fill = "transparent"))
}

