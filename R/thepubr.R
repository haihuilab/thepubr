#' Theme for publication
#'
#' @param base_size default font size is 12
#' @param base_family default font is "HelveticaNeueLT Std" that requires to be install in the system
#' @param axis
#' @param grid plot has grid
#' @param legend.position default is "none", could be "top", "right", "bottom", or "left"
#'
#' @export
#'

theme_publication <- function(base_size = 12,
                              base_family = "HelveticaNeueLT Std",
                              axis = TRUE,
                              grid = FALSE,
                              legend.position = NULL,
                              rotate_text = NULL) {

  if(axis) {
    axis_element <- element_line(color = 'black')
  } else {
    axis_element <- element_blank()
  }

  if(grid) {
    grid_element <- element_line(color = 'grey90')
  } else {
    grid_element <- element_blank()
  }

  if(rotate_text) {
    x_axis_text <- element_text(angle = rotate_text, hjust = 1)
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
          axis.text.x = x_axis_text,
          axis.line = axis_element,
          panel.grid.major = grid_element,
          panel.grid.minor = element_blank(),
          legend.key = element_rect(color = NA),
          legend.key.size = unit(0.4, "cm"),
          legend.background = element_rect(fill = NA),
          legend.spacing = unit(0, "cm"),
          legend.title = element_blank(), #element_text(face="italic"),
          legend.position = legend.position,
          strip.background = element_rect(color="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold"))
}

#' Custom theme
#'
#' @param base_size default font size is 12
#' @param base_family default font is : "HelveticaNeueLT Std"
#' @param border TRUE: plot has border
#' @param axis
#' @param grid
#' @param legend.position
#' @param facet_background
#' @param facet_border
#' @param facet_color facet_color
#'
#' @export
#'
# Border
theme_border <- function(base_size = 12,
                         base_family = "HelveticaNeueLT Std",
                         border = TRUE,
                         axis = TRUE,
                         grid = FALSE,
                         legend.position = 'none',
                         facet_background = 'dodgerblue4',
                         facet_border = 'black',
                         facet_color = 'white') {

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

  if(grid) {
    grid_element <- element_line(color = 'grey90')
  } else {
    grid_element <- element_blank()
  }


  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(plot.title = element_text(face = "plain", size = 14, hjust = 0.5),
          plot.subtitle = element_text(face = 'plain', size = 12, hjust = 0.5),
          text = element_text(family = base_family),
          panel.background = element_rect(color = 'white'),
          plot.background = element_rect(color = 'white'),
          panel.border = border_element,
          axis.title = element_text(face = "plain", size = rel(1)),
          axis.title.y = element_text(angle = 90, vjust = 0.5),
          axis.title.x = element_text(vjust = 0),
          axis.text = element_text(color = 'black'),
          axis.line = axis_element,
          panel.grid.major = grid_element,
          panel.grid.minor = element_blank(),
          legend.key = element_rect(color = NA),
          legend.key.size = unit(0.4, "cm"),
          legend.background = element_rect(fill = NA),
          legend.spacing = unit(0, "cm"),
          legend.title = element_blank(), #element_text(face="italic"),
          legend.position = legend.position,
          strip.background = element_rect(color = facet_border, fill = facet_background),
          strip.text = element_text(face = "plain", color = facet_color))
}


#' Custom fill color
#'
#' @param values a list of color values
#'
#' @export
#'
# Fill color
scale_fill_publication <- function(...){
  library(scales)
  discrete_scale("fill","publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#984ea3","#fb9a99", "#ffff33", "#a6cee3")), ...)

}


#' Custom discrete color
#'
#' @param values a list of color values
#'
#' @export
#'
# Discrete color
scale_color_publication <- function(...){
  library(scales)
  discrete_scale("color","publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#984ea3","#fb9a99", "#ffff33", "#a6cee3")), ...)

}


#' Custom pallete color
#'
#' @param palette 1 for "RdBu"; 2 for todo
#'
#' @export
#'
# Pallete
color_palette <- function(palette = 1) {

  if(palette == 1 | palette == 'RdBu') {
    colors <- RColorBrewer::brewer.pal(n = 11, name = 'RdBu')[c(3,9)]
  }

  colors
}


#' Custom save figure
#'
#' @param filename subfolder/filename
#' @param directory default is parent directory
#' @param size could be "small", "medium", "large"; "small_wide", "small_long"...
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
  if(size == 'small') {
    w = 2.5
    h = 2.5
  } else if(size == 'medium') {
    w = 5
    h = 5
  } else if(size == 'large') {
    w = 10
    h = 10
  } else if(size == 'small_wide') {
    w = 4
    h = 2.5
  } else if(size == 'medium_wide') {
    w = 8
    h = 5
  } else if(size == 'large_wide') {
    w = 16
    h = 10
  } else if(size == 'small_long') {
    w = 2.5
    h = 4
  } else if(size == 'medium_long') {
    w = 5
    h = 8
  } else if(size == 'large_long') {
    w = 10
    h = 16
  }

  # prevent overwrites
  i = 0
  prefix <- filename
  map(device, function(x) {
    if(!overwrite){
      while(file.exists(paste0(directory, filename,'.', x)))
      {
        i = i + 1
        ifilename = paste0(prefix, i)
      }
    }
    cat('Saving as', paste0(filename, '.', x), '\n')

    filename = paste0(filename, '.', x)
    if(gg) { # for ggplot objects
      if(x == 'pdf') {
        x <- cairo_pdf
      }
      ggsave(filename = paste0(directory,"/", filename), device = x, plot = plot, width = w, height = h, units = units)}
    else { # for others
      dev.copy(png, file = paste0(directory,"/", filename), width = w, height = h, units = units, res = 600)
      dev.off() }

  })
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
# # grid.arrange(bar,(bar + scale_fill_publication() + theme_publication()),nrow = 1)
# g3 <- grid.arrange((bar + scale_fill_publication() + theme_publication(base_size = 48)),nrow = 1)
# # large
# save_figure(g3, filename = "example_plot_large", size = "large")







