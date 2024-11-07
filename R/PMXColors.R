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

PMXColors_pmx_palettes <- function (n, name = c("default", "strong", "light", "extended",
                      "extendedS", "extendedL", "sexCol", "refLines", "sexColL",
                      "refLinesL", "sexColS", "refLinesS"), contName = c("continuousBlue",
                                                                         "continuousRed", "continuousGreen", "continuousViolet"),
          divName = c("divergentRedBlue", "divergentRedGreen", "divergentRedYellowGreen"),
          firstColorNum = 1, firstColorName = NULL, all_palettes = PMXColors_PMXcolors,
          type = c("discrete", "continuous", "sequential", "divergent"),
          keepName = NULL)
{
  type <- match.arg(type)
  if (type == "continuous") {
    name <- match.arg(contName)
  }
  else if (type == "divergent") {
    name <- match.arg(divName)
  }
  else {
    name <- match.arg(name)
  }
  palette <- all_palettes[[name]]
  if (type == "discrete" || type == "sequential") {
    if (missing(n)) {
      n <- length(palette)
    }
    if (type == "discrete") {
      palette <- PMXColors_startColor(palette, n = firstColorNum,
                            name = firstColorName)
    }
    else {
      orgPalette <- palette
      if(!requireNamespace("PMXColors", quietly = TRUE)) {
        stop(
          "Package \"PMXColors\" must be installed for this type of palette",
          call. = FALSE
        )
      }
      palette <- PMXColors_startColor(PMXColors::sortColors(palette, scale = "H"),
                            name = names(orgPalette)[1])
      palette <- PMXColors_startColor(palette, name = firstColorName,
                            n = firstColorNum)
    }
    if (n <= length(palette)) {
      out <- palette[1:n]
    }
    else {
      out <- (grDevices::colorRampPalette(palette))(n)
    }
  }
  else if (type == "continuous" || type == "divergent") {
    n <- ifelse(missing(n), 10, n)
    out <- (grDevices::colorRampPalette(palette))(n)
  }
  if (is.null(keepName) && (name != "sexCol" && name != "refLines")) {
    return(unname(out))
  }
  else if (is.null(keepName) && (name == "sexCol" || name ==
                                 "refLines")) {
    return(out)
  }
  else if (keepName) {
    return(out)
  }
  else {
    return(unname(out))
  }
}

PMXColors_PMXcolors <- list(
  default = c(`dark blue` = "#1774A8", orange = "#F8A16A",  teal = "#00918B",
              violet = "#AD3492", blue = "#76ACF9", red = "#EB7070",
              green = "#75C16D", purple = "#997BD9", cyan = "#5CD0E6", yellow = "#FFC333"
  ),
  strong = c(`dark blue` = "#13618C", orange = "#F57D32", teal = "#007974",
             violet = "#902B7A", blue = "#3B88F6", red = "#E43D3D", green = "#55B14B",
             purple = "#774FCC", cyan = "#2DC3DF", yellow = "#FFB400"
               ),
  light = c(`dark blue` = "#1C8BCA",
            orange = "#FBCCAE", teal = "#00AEA7", violet = "#C846AB", blue = "#BCD7FC",
            red = "#F4ADAD", green = "#9DD397", purple = "#C2B0E8", cyan = "#94E0EF",
            yellow = "#FFD570"),
  extended = c("#1774A8", "#F8A16A", "#00918B", "#AD3492", "#76ACF9", "#EB7070",
                        "#75C16D", "#997BD9", "#5CD0E6", "#FFC333", "#CF5322", "#6F90BD",
                        "#C09268", "#EAB7D9", "#7681F9", "#EB996F", "#3C7E52", "#F8CF68",
                        "#58B2BC", "#A37F29"
  ),
  extendedS = c("#13618C",  "#F57D32", "#007974", "#902B7A", "#3B88F6", "#E43D3D",
                         "#55B14B",  "#774FCC", "#2DC3DF", "#FFB400", "#AC451C",
                         "#4F76AB", "#AE7949", "#DA81BD", "#3B4BF6", "#E4753C",
                         "#326944", "#F5BD30", "#429AA4",   "#957425"
  ),
  extendedL = c("#1C8BCA", "#FBCCAE", "#00AEA7", "#C846AB",
                         "#BCD7FC", "#F4ADAD", "#9DD397", "#C2B0E8", "#94E0EF", "#FFD570",
                         "#DF693A", "#91AACD", "#CFAB8B", "#F2CDE3", "#B1B7FC", "#F2BDA2",
                         "#469360", "#FBE1A0", "#79C1C9", "#C18C30"
  ),
  continuousRed = c(red = "#E43D3D",   orange = "#FBCCAE"),
  continuousBlue = c(`dark blue` = "#13618C", blue = "#BCD7FC"),
  continuousGreen = c(teal = "#007974", green = "#9DD397"),
  continuousViolet = c(violet = "#902B7A", purple = "#C2B0E8"),
  divergentRedBlue = c(`dark blue` = "#13618C", "white", red = "#E43D3D"),
  divergentRedGreen = c(red = "#E43D3D", "white", teal = "#007974"),
  divergentRedYellowGreen = c(red = "#E43D3D", yellow = "#FFB400", teal = "#007974"),
  refLines = c(`Line of identity` = "black", `y=0` = "black", Smooth = "#F10E0E", `Linear regression` = "#1D8AF7"),
  refLinesS = c(`Line of identity` = "#000000", `y=0` = "#000000", Smooth = "#C90C0C", `Linear regression` = "#0873DE"),
  refLinesL = c(`Line of identity` = "#000000", `y=0` = "#000000", Smooth = "#F33636", `Linear regression` = "#49A1F9"),
  sexCol = c(Male = "#70AABD", Female = "#CA5A5A", Males = "#70AABD", Females = "#CA5A5A"),
  sexColS = c(Male = "#4F95AC", Female = "#B83B3B", Males = "#4F95AC", Females = "#B83B3B"),
  sexColL = c(Male = "#92BECD", Female = "#D67F7F", Males = "#92BECD", Females = "#D67F7F")
)

PMXColors_startColor <- function (x, name = names(x), n = 1)
{
  if (missing(name) && is.null(n))
    stop("Either name or n must be specified.")
  if (is.null(names(x)) && (!is.null(name) && !missing(name))) {
    if (!is.null(n))
      warning("x has no names but name was specified. Will use n to pick start color.")
    if (is.null(n))
      stop("x has no names but name was specified. n is missing.")
  }
  if (!is.null(name) && !missing(name) && !is.null(names(x))) {
    name <- match.arg(name)
    n <- match(name, names(x))
  }
  n <- n - 1
  if (n == 0) {
    out <- x
  }
  else {
    out <- c(utils::tail(x, -n), utils::head(x, n))
  }
  return(out)
}

