#' Theme for publication
#'
#' @param font.size default font size is 12
#' @param font.family default font is "HelveticaNeueLT Std" that requires to be install in the system
#' @param axis TRUE
#' @param grid default without grid, is FALSE, or "major", "minor", "all"
#' @param legend.position default is "top", could be "none", "top", "right", "bottom", or "left"
#' @param legend.font.size default legend font size is 12
#' @param aspect.ratio default is "1": square
#'
#' @export
#'
#' theme_publication
theme_publication <- function(font.size = 12,
                              font.family = "HelveticaNeueLT Std",
                              axis = TRUE,
                              grid = FALSE,
                              legend.position = "top",
                              legend.font.size = 12,
                              aspect.ratio = NULL,
                              rotate.text = NULL) {

  if(axis) {
    axis_element <- element_line(color = 'black', size =  0.2)
  } else {
    axis_element <- element_blank()
  }

  # grid
  if (!is.null(grid)) {
    grid.major <- grid.minor <- element_blank()
  }
  if(grid == "major") {
    grid.major <- element_line(color = 'grey85', size = 0.1, linetype = 1)
  }
  if(grid == "minor") {
    grid.minor <- element_line(color = 'grey85', size = 0.1, linetype = 1)
  }
  if(grid == "all") {
    grid.major <- element_line(color = 'grey85', size = 0.1, linetype = 1)
    grid.minor <- element_line(color = 'grey85', size = 0.1, linetype = 1)
  }

  if(!is.null(rotate.text)) {
    x_axis_text <- element_text(angle = rotate.text,  vjust = 1, hjust=1)
  } else {
    x_axis_text <- element_text()
  }

  ggthemes::theme_foundation(base_size = font.size, base_family = font.family) +
    theme(plot.title = element_text(face = "bold", size = font.size, hjust = 0.5),
          plot.subtitle = element_text(face = "plain", size = font.size, hjust = 0.5),
          text = element_text(family = font.family),
          panel.background = element_rect(color = NA),
          plot.background = element_rect(color = NA),
          panel.border = element_blank(),
          axis.title = element_text(face = "plain", size = rel(1)),
          axis.title.y = element_text(angle = 90, vjust = 0.5),
          axis.title.x = element_text(vjust = 0),
          axis.text = element_text(color = 'black'),
          axis.line = axis_element,
          axis.ticks = axis_element,
          panel.grid.major = grid.major,
          panel.grid.minor = grid.minor,
          strip.background = element_rect(color="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold"),
          axis.text.x = x_axis_text, # rotation
          aspect.ratio = aspect.ratio,
          # legend.title = element_blank(), #element_text(face="italic"),
          legend.position = legend.position,
          legend.key = element_rect(color = NA),
          legend.background = element_rect(fill = NA),
          legend.spacing = unit(0, "cm"),
          legend.text = element_text(size = legend.font.size),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.2, "cm"))
  }

#' Custom theme
#'
#' @param font.size default font size is 12
#' @param font.family default font is : "HelveticaNeueLT Std"
#' #' @param rotate.text (optional) numeric angle to rotate x-axis labels; default NULL
#' @param border TRUE: plot has border
#' @param axis TRUE
#' @param grid  default is FALSE, or "major", "minor", "all"
#' @param legend.position default is "top", could be "none", "top", "right", "bottom", or "left"
#' @param legend.font.size default legend font size is 12
#' @param facet.background default is 'grey85'
#' @param facet.border default is 'black'
#' @param facet.color default is 'white'
#' @param rotate.text (optional) numeric angle to rotate x-axis labels; default NULL
#'
#' @export
#'
# theme_border
theme_border <- function(font.size = 12,
                         font.family = "HelveticaNeueLT Std",
                         border = TRUE,
                         axis = TRUE,
                         grid = FALSE,
                         legend.position = "top",
                         legend.font.size = 12,
                         facet.background = "#F0F0F0",
                         facet.border = 'black',
                         facet.color = 'white',
                         aspect.ratio = NULL,
                         rotate.text = NULL) {
  theme_pub <- theme_publication(font.size = font.size,
                     font.family = font.family,
                     axis = axis,
                     grid = grid,
                     legend.position = legend.position,
                     legend.font.size = legend.font.size,
                     aspect.ratio = aspect.ratio,
                     rotate.text = rotate.text)

  if(border) {
    border_element <- element_rect(color = 'black', fill = NA, size = 0.3)
  } else {
    border_element <- element_rect(color = NA, fill = NA)
  }

  if(axis) {
    axis_element <- element_line(color = 'black', size =  0)
  } else {
    axis_element <- element_blank()
  }


  theme_pub +
    theme(panel.background = border_element,
          plot.background = border_element,
          axis.line = axis_element,
          strip.background = element_rect(fill = facet.background, color = facet.border))
}



#' Custom save figure
#'
#' @param filename subfolder/filename
#' @param size could be "small", "medium", "large", "super; "small_wide", "small_long"...
#' @param width custom width
#' @param height custom height
#' @export
#'
# Save custom figure size
save_figure <- function(plot = last_plot(),
                        filename,
                        device = "pdf",
                        units = "in",
                        size = c("custom", "small", "medium", "large",
                                 "small_wide", "medium_wide", "large_wide",
                                 "small_long", "medium_long", "large_long",
                                 "super"),
                        width = 2.5,
                        height = 2.5,
                        gg = TRUE,
                        overwrite = TRUE) {
  # Ensure 'size' is a single string from the valid choices
  size <- match.arg(size, choices = c("custom", "small", "medium", "large",
                                      "small_wide", "medium_wide", "large_wide",
                                      "small_long", "medium_long", "large_long",
                                      "super"))

  wh <- switch(size,
               custom      = c(width, height),
               small       = c(2.5, 2.5),
               medium      = c(5, 5),
               large       = c(10, 10),
               small_wide  = c(4, 2.5),
               medium_wide = c(8, 5),
               large_wide  = c(16, 10),
               small_long  = c(2.5, 4),
               medium_long = c(5, 8),
               large_long  = c(10, 16),
               super       = c(20, 20),
               # Default if no match (though match.arg() ensures we always match)
               c(width, height)
  )

  width <- wh[1]
  height <- wh[2]

  # prevent overwrites----------------------------------------------------------
  i = 0
  prefix <- filename
  if(!overwrite) {
      while(file.exists(paste0(filename,'.', device)))
      {
        i = i + 1
        filename = paste0(prefix, i)
      }
  }

  message("Saving to: ", paste0(filename, '.', device), '\n')

  filename = paste0(filename, '.', device)

  save_dir <- gsub("/[^/]*$", "", filename)
  if (!file.exists(save_dir)) {
      cat("Folder doesn't exist: ", save_dir, "\n")
    }

  if (gg) { # for ggplot objects
      if (device == 'pdf') {
        device <- cairo_pdf
        ggsave(filename = filename, device = device, plot = plot, width = width, height = height, units = units)
        # also save as jpg
        ggsave(filename = gsub("\\.pdf$", ".jpg", filename), device = 'jpg', plot = plot, width = width, height = height, units = units, dpi = 600)
      } else {
        ggsave(filename = filename, device = device, plot = plot, width = width, height = height, units = units, dpi = 600)
      }
    }
}

#-------------------------------------------------------------------------------
# Examples
# library(ggplot2)
# library(gridExtra)
# library(tidyverse)

# scatter <- ggplot(mtcars, aes(mpg,disp,color=factor(carb))) + geom_point(size=3, alpha = 0.7) + labs(title="Scatter Plot")
# g1 <- grid.arrange(scatter,(scatter + scale_color_publication() + theme_publication()),nrow = 1)
# # Small
# save_figure(g1, filename = "example_plot_small", size = "small_wide")

#
# g2 <- grid.arrange((scatter + scale_color_publication() + theme_publication(font.size = 24)),nrow = 1)
# # Medium
# save_figure(g2, filename = "example_plot_medium", size = "medium", device = "png")
#
# bar <- ggplot(mtcars, aes(factor(carb),fill = factor(carb))) + geom_bar(alpha = 0.7) + labs(title = "Bar Plot")
# # grid.arrange(bar,(bar + scale_color_publication() + theme_publication()),nrow = 1)
# g3 <- grid.arrange((bar + scale_color_publication() + theme_publication(font.size = 48)),nrow = 1)
## large
## save_figure(g3, filename = "example_plot_large", size = "large")
