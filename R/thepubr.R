#' Theme for publication
#'
#' @param base_size default font size is 12
#' @param base_family default font is "HelveticaNeueLT Std" that requires to be install in the system
#' @param axis TRUE
#' @param grid default without grid, is FALSE, or "major", "minor"
#' @param legend.position default is "none", could be "top", "right", "bottom", or "left"
#'
#' @export
#'
#' theme_publication
theme_publication <- function(base_size = 12,
                              base_family = "HelveticaNeueLT Std",
                              axis = TRUE,
                              grid = FALSE,
                              legend.position = "top",
                              rotate_text = NULL) {

  if(axis) {
    axis_element <- element_line(color = 'black')
  } else {
    axis_element <- element_blank()
  }
  # grid
  if (!is.null(grid)) {
    grid.major <- grid.minor <- element_blank()
  }
  if(grid == "major") {
    grid.major <- element_line(color = 'grey30', size = 0.1, linetype = 1)
  }
  if(grid == "minor") {
    grid.minor <- element_line(color = 'grey30', size = 0.1, linetype = 1)
  }

  if(!is.null(rotate_text)) {
    x_axis_text <- element_text(angle = rotate_text,  vjust = 1, hjust=1)
  } else {
    x_axis_text <- element_text()
  }

  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(plot.title = element_text(face = "plain", size = 14, hjust = 0.5),
          text = element_text(family = base_family),
          panel.background = element_rect(color = NA),
          plot.background = element_rect(color = NA),
          panel.border = element_blank(),
          axis.title = element_text(face = "plain", size = rel(1)),
          axis.title.y = element_text(angle = 90, vjust = 0.5),
          axis.title.x = element_text(vjust = 0),
          axis.text = element_text(color = 'black'),
          axis.line = axis_element,
          panel.grid.major = grid.major,
          panel.grid.minor = grid.minor,
          legend.key = element_rect(color = NA),
          legend.key.size = unit(0.4, "cm"),
          legend.background = element_rect(fill = NA),
          legend.spacing = unit(0, "cm"),
          legend.title = element_blank(), #element_text(face="italic"),
          legend.position = legend.position,
          strip.background = element_rect(color="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold"),
          axis.text.x = x_axis_text) # rotation
}

#' Custom theme
#'
#' @param base_size default font size is 12
#' @param base_family default font is : "HelveticaNeueLT Std"
#' @param border TRUE: plot has border
#' @param axis TRUE
#' @param grid  default is FALSE, or "major", "minor"
#' @param legend.position default is none
#' @param facet_background defalut is 'dodgerblue4'
#' @param facet_border defalut is 'black'
#' @param facet_color default is 'white'
#'
#' @export
#'
# theme_border
theme_border <- function(base_size = 12,
                         base_family = "HelveticaNeueLT Std",
                         border = TRUE,
                         axis = TRUE,
                         grid = FALSE,
                         legend.position = "top",
                         facet_background = 'dodgerblue4',
                         facet_border = 'black',
                         facet_color = 'white',
                         rotate_text = NULL) {
  theme_pub <- theme_publication(base_size = base_size,
                     base_family = base_family,
                     axis = axis,
                     grid = grid,
                     legend.position = legend.position,
                     rotate_text = rotate_text)

  if(border) {
    border_element <- element_rect(color = 'black', fill = NA)
  } else {
    border_element <- element_rect(color = NA, fill = NA)
  }

  if(axis) {
    axis_element <- element_line(color = 'black')
  } else {
    axis_element <- element_blank()
  }


  theme_pub +
    theme(panel.background = border_element,
          plot.background = border_element,
          axis.line = axis_element)
}


#' Custom fill color
#'
#' @param values a list of color values
#' @param alpha alpha value (0, 1)
#'
#' @export
#'
# scale_color_publication
scale_color_publication <- function(alpha = 1, ...) {
  values = c( "#386cb0", "#660066","#336600",  "#D2691E",  "#A52A2A",
              "#FFB90F", "#7fc97f", "#fb9a99","#B8860B", "#CD1076",
             "aquamarine4","#E5CCFF", "#7071b1", "#0000FF", "#003366",
             "#663300", "#666600", "#7FFF00", "#330066", "#00FFFF",
             "#838B8B", "#330019", "#ffff33",  "#DEB887", "#5F9EA0",
             "#2F4F4F", "#8B4513", "#8B2323", "#FF7F50", "#a6cee3",
             "#66B2FF", "#8B7D6B", "#FF3319",  "#660000", "#CCCC00")
  if (alpha == 1) {
    scale_colour_manual(values = values, ...)
  } else {
    scale_colour_manual(values = alpha(values, alpha), ...)

  }
}



#' Custom save figure
#'
#' @param filename subfolder/filename
#' @param directory default is parent directory
#' @param size could be "small", "medium", "large", "super; "small_wide", "small_long"...
#'
#' @export
#'
# Save custom figure size
save_figure <- function(plot = last_plot(),
                        filename,
                        device = "pdf",
                        units = 'in',
                        directory = dirname(getwd()),
                        size = 'small',
                        gg = TRUE,
                        overwrite = TRUE) {

  # Some default sizes
  if (size == 'small') {
    w = 2.5
    h = 2.5
  }
  if (size == 'medium') {
    w = 5
    h = 5
  }
  if (size == 'large') {
    w = 10
    h = 10
  }
  if (size == 'small_wide') {
    w = 4
    h = 2.5
  }
  if (size == 'medium_wide') {
    w = 8
    h = 5
  }
  if (size == 'large_wide') {
    w = 16
    h = 10
  }
  if (size == 'small_long') {
    w = 2.5
    h = 4
  }
  if (size == 'medium_long') {
    w = 5
    h = 8
  }
  if (size == 'large_long') {
    w = 10
    h = 16
  }
  if (size == 'super') {
    w = 20
    h = 20
  }

  # prevent overwrites----------------------------------------------------------
  i = 0
  prefix <- filename
  if(!overwrite) {
      while(file.exists(paste0(directory, filename,'.', device)))
      {
        i = i + 1
        ifilename = paste0(prefix, i)
      }
    }
    cat('Saving as', paste0(filename, '.', device), '\n')

    filename = paste0(filename, '.', device)
    if (gg) { # for ggplot objects
      if (device == 'pdf') {
        device <- cairo_pdf
        ggsave(filename = paste0(directory, "/", filename), device = device, plot = plot, width = w, height = h, units = units)
      } else {
        ggsave(filename = paste0(directory, "/", filename), device = device, plot = plot, width = w, height = h, units = units, dpi = 600)
      }
    }
}

#---------------------------------
# Examples
# library(ggplot2)
# library(gridExtra)
# library(tidyverse)

# scatter <- ggplot(mtcars, aes(mpg,disp,color=factor(carb))) + geom_point(size=3, alpha = 0.7) + labs(title="Scatter Plot")
# g1 <- grid.arrange(scatter,(scatter + scale_color_publication() + theme_publication()),nrow = 1)
# # Small
# save_figure(g1, filename = "example_plot_small", size = "small_wide")

#
# g2 <- grid.arrange((scatter + scale_color_publication() + theme_publication(base_size = 24)),nrow = 1)
# # Medium
# save_figure(g2, filename = "example_plot_medium", size = "medium", device = "png")
#
# bar <- ggplot(mtcars, aes(factor(carb),fill = factor(carb))) + geom_bar(alpha = 0.7) + labs(title = "Bar Plot")
# # grid.arrange(bar,(bar + scale_color_publication() + theme_publication()),nrow = 1)
# g3 <- grid.arrange((bar + scale_color_publication() + theme_publication(base_size = 48)),nrow = 1)
## large
## save_figure(g3, filename = "example_plot_large", size = "large")
