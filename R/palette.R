#' Get palette colors
#'
#' Get palette colors from palette theme name and n. The color palettes
#' are from RColorBrewer, but with the middle color changing to '#FFFFFF'(white),
#' thus we can visualize element 0 with white color.
#' palette colors are suitable for visualize a matrix which elements are partly positive and
#' partly negative (e.g. correlation matrix in [-1, 1], or [-20, 100]).
#'
#' @param palette palette color palettes
#' @param reverse default color is in positive order (FALSE, TRUE)
#' @param n the number of colors (>= 1) to be in the palette.
#' @param limits rescale values, default is c(0, 1)
#' @return A character vector containing color names
#' @param aesthetics c("color", "colour", "fill", "custom")
#'
#' @seealso Function \code{\link{colorRamppalette}}, package \code{RColorBrewer}
#'
#' @keywords color
#'
#' @export
#'
pub_pal1 <-  function(
    limits = c(0, 1),
    aesthetics = c("color", "colour", "fill", "custom"),
    palette = c( 'Custom', 'Spectral', 'RdBu', 'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdYlBu'),
    reverse = FALSE,
    n = 200) {

  palette = match.arg(palette)

  colors = switch(
    palette,
    Custom = c("#0F425CFF","#FFFF99","#FFCC66","#FF9933",
               "#CC0C00FF","#800000FF"),
    Spectral = c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598", "#FFFFBF",
                 "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142"),
    RdBu = c('#67001F', '#B2182B', '#D6604D', '#F4A582', '#FDDBC7', '#FFFFFF',
             '#D1E5F0', '#92C5DE', '#4393C3', '#2166AC', '#053061'),
    BrBG = c('#543005', '#8C510A', '#BF812D', '#DFC27D', '#F6E8C3', '#FFFFFF',
             '#C7EAE5', '#80CDC1', '#35978F', '#01665E', '#003C30'),
    PiYG = c('#8E0152', '#C51B7D', '#DE77AE', '#F1B6DA', '#FDE0EF', '#FFFFFF',
             '#E6F5D0', '#B8E186', '#7FBC41', '#4D9221', '#276419'),
    PRGn = c('#40004B', '#762A83', '#9970AB', '#C2A5CF', '#E7D4E8', '#FFFFFF',
             '#D9F0D3', '#A6DBA0', '#5AAE61', '#1B7837', '#00441B'),
    PuOr = c('#7F3B08', '#B35806', '#E08214', '#FDB863', '#FEE0B6', '#FFFFFF',
             '#D8DAEB', '#B2ABD2', '#8073AC', '#542788', '#2D004B'),
    RdYlBu = c('#A50026', '#D73027', '#F46D43', '#FDAE61', '#FEE090', '#FFFFFF',
               '#E0F3F8', '#ABD9E9', '#74ADD1', '#4575B4', '#313695')
  )

  # reverse color
  if (reverse == TRUE) {
    colors = rev(colors)
  }

  if (aesthetics == "color") {
    grad <- scale_color_gradientn(colours = colorRampPalette(colors)(n),
                                  limits = limits,
                                  labels = function(x) paste0(format(round(x, 1))),
                                  oob = scales::squish)
  }

  if (aesthetics == "fill") {
    grad <- scale_fill_gradientn(colours = colorRampPalette(colors)(n),
                                  limits = limits,
                                  labels = function(x) paste0(format(round(x, 1))),
                                  oob = scales::squish)
  }

  if (aesthetics == "custom") {
    grad <- colorRampPalette(colors)(n)
  }

  return(grad)

}

#' Get palette colors
#'
#' Get palette colors from palette theme name and n. The color palettes
#' are from RColorBrewer. palette colors are suitable for visualize a non-negative
#' or non-positive matrix (e.g. matrix in [0, 20], or [-100, -10], or [100, 500]).
#'
#' @param palette palette color palettes
#' @param reverse default color is in positive order (FALSE, TRUE)
#' @param n the number of colors (>= 1) to be in the palette.
#' @param limits rescale values, default is c(0, 1)
#' @return A character vector containing color names
#' @param aesthetics c("color", "fill")
#'
#' @seealso Function \code{\link{colorRamppalette}}, package \code{RColorBrewer}
#'
#' @keywords color
#'
#' @export
#'
pub_pal2 <-  function(
    limits = c(0, 1),
    aesthetics = c("color", "colour", "fill"),
    palette = c('Oranges', 'Purples', 'Reds', 'Blues', 'Greens', 'Greys',
                'OrRd', 'YlOrRd', 'YlOrBr', 'YlGn'),
    reverse = FALSE,
    n = 200) {

  aesthetics = match.arg(aesthetics)
  aesthetics = match.arg(aesthetics, choices = c("color", "colour", "fill", "custom"))

  colors = switch(
    palette,
    Oranges = c('#FFF5EB', '#FEE6CE', '#FDD0A2', '#FDAE6B', '#FD8D3C', '#F16913',
                '#D94801', '#A63603', '#7F2704'),
    Purples = c('#FCFBFD', '#EFEDF5', '#DADAEB', '#BCBDDC', '#9E9AC8', '#807DBA',
                '#6A51A3', '#54278F', '#3F007D'),
    Reds = c('#FFF5F0', '#FEE0D2', '#FCBBA1', '#FC9272', '#FB6A4A', '#EF3B2C',
             '#CB181D', '#A50F15', '#67000D'),
    Blues = c('#F7FBFF', '#DEEBF7', '#C6DBEF', '#9ECAE1', '#6BAED6', '#4292C6',
              '#2171B5', '#08519C', '#08306B'),
    Greens = c('#F7FCF5', '#E5F5E0', '#C7E9C0', '#A1D99B', '#74C476', '#41AB5D',
               '#238B45', '#006D2C', '#00441B'),
    Greys = c('#FFFFFF', '#F0F0F0', '#D9D9D9', '#BDBDBD', '#969696', '#737373',
              '#525252', '#252525', '#000000'),
    OrRd = c('#FFF7EC', '#FEE8C8', '#FDD49E', '#FDBB84', '#FC8D59', '#EF6548',
             '#D7301F', '#B30000', '#7F0000'),
    YlOrRd = c('#FFFFCC', '#FFEDA0', '#FED976', '#FEB24C', '#FD8D3C', '#FC4E2A',
               '#E31A1C', '#BD0026', '#800026'),
    YlOrBr = c('#FFFFE5', '#FFF7BC', '#FEE391', '#FEC44F', '#FE9929', '#EC7014',
               '#CC4C02', '#993404', '#662506'),
    YlGn = c('#FFFFE5', '#F7FCB9', '#D9F0A3', '#ADDD8E', '#78C679', '#41AB5D',
             '#238443', '#006837', '#004529')
  )

  # reverse color
  if (reverse == TRUE) {
    colors = rev(colors)
  }

  aesthetics = match.arg(aesthetics)
  aesthetics = switch(aesthetics, color = "color", colour = "color", fill = "fill")

  if (aesthetics == "color") {
    grad <- scale_color_gradientn(colours = colorRampPalette(colors)(n),
                                  limits = limits,
                                  labels = function(x) paste0(format(round(x, 1))),
                                  oob = scales::squish)
  }

  if (aesthetics == "fill") {
    grad <- scale_fill_gradientn(colours = colorRampPalette(colors)(n),
                                 limits = limits,
                                 labels = function(x) paste0(format(round(x, 1))),
                                 oob = scales::squish)
  }

  return(grad)

}
