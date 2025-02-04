#' Custom fill color
#'
#' @param values a list of color values
#' @param alpha alpha value (0, 1)
#' @param n  could be vector ranging 1:35
#'
#' @export
#'
pub_colors <- function(alpha = 1,n = 1, ...) {
  values = c( "#386cb0", "#660066","#336600",  "#D2691E",  "#A52A2A",
              "#FFB90F", "#7fc97f", "#fb9a99","#B8860B", "#CD1076",
              "#458B74","#E5CCFF", "#7071b1", "#0000FF", "#003366",
              "#663300", "#666600", "#7FFF00", "#330066", "#00FFFF",
              "#838B8B", "#330019", "#ffdd33",  "#DEB887", "#5F9EA0",
              "#2F4F4F", "#8B4513", "#8B2323", "#FF7F50", "#a6cee3",
              "#66B2FF", "#8B7D6B", "#FF3319",  "#660000", "#CCCC00",
              "#be4d25", "#50C9CE", "#72A1E5", "#FCD3DE", "#2E382E",
              "#ffadad", "#ffd6a5", "#caffbf", "#9bf6ff", "#bdb2ff",
              "#8B6508", "#8B2323", "#00008B", "#458B00",  "#8B4513") %>%
    factor(levels = unique(.))

  values <- alpha(values, alpha)[n]
  return(values)
}


#' Custom scale_fill_manual, scale_colour_manual
#'
#' @param values a list of color values
#' @param alpha alpha value (0, 1)
#'
#' @export
## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
pub_scale_colors <- function(alpha = 1, ...) {
  colors = c( "#386cb0", "#660066","#336600",  "#D2691E",  "#A52A2A",
              "#FFB90F", "#7fc97f", "#fb9a99","#B8860B", "#CD1076",
              "#458B74","#E5CCFF", "#7071b1", "#0000FF", "#003366",
              "#663300", "#666600", "#7FFF00", "#330066", "#00FFFF",
              "#838B8B", "#330019", "#ffff33",  "#DEB887", "#5F9EA0",
              "#2F4F4F", "#8B4513", "#8B2323", "#FF7F50", "#a6cee3",
              "#66B2FF", "#8B7D6B", "#FF3319",  "#660000", "#CCCC00",
              "#be4d25", "#50C9CE", "#72A1E5", "#FCD3DE", "#2E382E",
              "#ffadad", "#ffd6a5", "#caffbf", "#9bf6ff", "#bdb2ff",
              "#8B6508", "#8B2323", "#00008B", "#458B00",  "#8B4513")

  scale_discrete_manual(values = alpha(colors, alpha), ...)

}


