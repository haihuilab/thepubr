#' Custom fill color
#'
#' @param values a list of color values
#' @param alpha alpha value (0, 1)
#' @param n  could be vector ranging c(1, 35)
#'
#' @export
#'
# thepubr_colors
thepubr_colors <- function(alpha = 1,n = 1, ...) {
  values = c( "#386cb0", "#660066","#336600",  "#D2691E",  "#A52A2A",
              "#FFB90F", "#7fc97f", "#fb9a99","#B8860B", "#CD1076",
              "aquamarine4","#E5CCFF", "#7071b1", "#0000FF", "#003366",
              "#663300", "#666600", "#7FFF00", "#330066", "#00FFFF",
              "#838B8B", "#330019", "#ffff33",  "#DEB887", "#5F9EA0",
              "#2F4F4F", "#8B4513", "#8B2323", "#FF7F50", "#a6cee3",
              "#66B2FF", "#8B7D6B", "#FF3319",  "#660000", "#CCCC00") %>%
    factor(levels = unique(.))

  values <- alpha(values, alpha)[n]
  return(values)
}


#' Custom scale_fill_manual, scale_colour_manual
#'
#' @param values a list of color values
#' @param alpha alpha value (0, 1)
#' @param aes default is "colour" with scale_colour_manual,
#' use aesthetic mapping with "colour" like geom_line() and geom_point().
#' If aes = "fill" with scale_fill_manual,
#' use aesthetic mapping with "fill" like geom_bar(), geom_area(), and geom_polygon().
#'
#' @export
#'
# thepubr_scale_colors
thepubr_scale_colors <- function(alpha = 1, aes = "colour", ...) {

  values = c( "#386cb0", "#660066","#336600",  "#D2691E",  "#A52A2A",
              "#FFB90F", "#7fc97f", "#fb9a99","#B8860B", "#CD1076",
              "aquamarine4","#E5CCFF", "#7071b1", "#0000FF", "#003366",
              "#663300", "#666600", "#7FFF00", "#330066", "#00FFFF",
              "#838B8B", "#330019", "#ffff33",  "#DEB887", "#5F9EA0",
              "#2F4F4F", "#8B4513", "#8B2323", "#FF7F50", "#a6cee3",
              "#66B2FF", "#8B7D6B", "#FF3319",  "#660000", "#CCCC00")

  # aesthetic mapping with `fill` like geom_bar(), geom_area(), and geom_polygon().
  if (aes == "colour" | aes == "color") {
    scale_colour_manual(values = alpha(values, alpha), ...)
  }

  # aesthetic mapping with `color` like geom_line() and geom_point().
  if (aes == "fill") {
    scale_fill_manual(values = alpha(values, alpha), ...)
    }

}


