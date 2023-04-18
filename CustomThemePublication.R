
# Theme for publication
theme_publication <- function(base_size = 12,
                              base_family = "Helvetica",
                              axis = TRUE,
                              grid = FALSE,
                              legend.position = 'none',
                              rotate_text = 'none') {

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

  if(rotate_text == 'x') {
    x_axis_text <- element_text(angle = 90, hjust = 1)
  } else {
    x_axis_text <- element_text()
  }

  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(plot.title = element_text(face = "plain", size = 14, hjust = 0.5),
          text = element_text(),
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


# Border
theme_border <- function(base_size = 12,
                      base_family = "Helvetica",
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
          text = element_text(),
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



# Fill color
scale_fill_publication <- function(...){
  library(scales)
  discrete_scale("fill","publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#984ea3","#fb9a99", "#ffff33", "#a6cee3")), ...)

}


# Discrete color
scale_color_publication <- function(...){
  library(scales)
  discrete_scale("color","publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#984ea3","#fb9a99", "#ffff33", "#a6cee3")), ...)

}


# Pallete
color_palette2 <- function(palette = 1) {

  if(palette == 1 | palette == 'RdBu') {
    colors <- RColorBrewer::brewer.pal(n = 11, name = 'RdBu')[c(3,9)]
  }

  colors
}

#---------------------------------
# Examples
library(ggplot2)
library(gridExtra)
pdf("example_plot.pdf", family = "Helvetica", width = 2.5, height = 2.5)
scatter <- ggplot(mtcars, aes(mpg,disp,color=factor(carb))) + geom_point(size=3, alpha = 0.7) + labs(title="Scatter Plot")
# grid.arrange(scatter,(scatter + scale_color_publication() + theme_publication()),nrow = 1)
grid.arrange((scatter + scale_color_publication() + theme_publication()),nrow = 1)

bar <- ggplot(mtcars, aes(factor(carb),fill = factor(carb))) + geom_bar(alpha = 0.7) + labs(title = "Bar Plot")
# grid.arrange(bar,(bar + scale_fill_publication() + theme_publication()),nrow = 1)
grid.arrange((bar + scale_fill_publication() + theme_publication()),nrow = 1)

dev.off()








