
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








