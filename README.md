
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CustomThemePublication

<!-- badges: start -->
<!-- badges: end -->

The goal of CustomThemePublication is to set default theme color... for publication-scale output plot…

## Installation

You can install the development version of CustomThemePublication like
so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CustomThemePublication)
## basic example code
library(ggplot2)
library(gridExtra)
theme_set(theme_publication()) # set default theme
pdf("example_plot.pdf", family = "Helvetica", width = 2.5, height = 2.5)
scatter <- ggplot(mtcars, aes(mpg,disp,color=factor(carb))) + geom_point(size=3, alpha = 0.7) + labs(title="Scatter Plot")
# grid.arrange(scatter,(scatter + scale_color_publication() + theme_publication()),nrow = 1)
grid.arrange((scatter + scale_color_publication() + theme_publication()),nrow = 1)

bar <- ggplot(mtcars, aes(factor(carb),fill = factor(carb))) + geom_bar(alpha = 0.7) + labs(title = "Bar Plot")
# grid.arrange(bar,(bar + scale_fill_publication() + theme_publication()),nrow = 1)
grid.arrange((bar + scale_fill_publication() + theme_publication()),nrow = 1)

dev.off()
#> png 
#>   2
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/example_plot.pdf" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
