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
heatmap_markers <- function(
    markers.matrix = NULL,
    color = c("blue", "white", "red"),
    paletteLength = 100,
    fontsize_row = 10,
    color.rows = T,
    title = "log(FC)",
    add.flag = T,
    de.cutoff = 4,
    top.n = NULL,
    repel.degree = 0,
    legend = T,
    annotation_legend = T,
    cellwidth = NA,
    cellheight = NA,
    save = F,
    width = 7,
    height = 11,
    file = "/home/figo/software/scell_skin_inflammation_master/plot_out/IM11/p.pdf") {

  paletteLength = 100
  if(sum(markers.matrix>=0)==dim(markers.matrix)[1]*dim(markers.matrix)[2]) {
    color = color[2:3]
  }
  myColor = colorRampPalette(color)(paletteLength)
  myBreaks <- unique(c(seq(min(markers.matrix), 0, length.out=ceiling(paletteLength/2) + 1),
                       seq(max(markers.matrix)/paletteLength, max(markers.matrix),
                           length.out=floor(paletteLength/2))))
  annotation_colors = list(
    Ventilated = c(ARDS="red3", NonVent=RColorBrewer::brewer.pal(9, "Oranges")[4]))
  p <- pheatmap(markers.matrix, color = myColor, breaks = myBreaks,
                heatmap_legend_param = list(title = title), angle_col = "90",
                annotation_col = row_annotation, annotation_colors = annotation_colors, legend = legend,
                annotation_legend = annotation_legend, fontsize_row = fontsize_row,
                cellwidth = cellwidth, cellheight = cellheight)
  if(color.rows){
    markers.matrix <- markers.matrix[match(p$gtable$grobs[[5]]$label,rownames(markers.matrix)),]
    p$gtable$grobs[[5]]$gp=gpar(col=ifelse((rowSums(markers.matrix))>0, "red", "blue"), fontsize = fontsize_row)
  }

  if(add.flag){
    if(save){
      pdf(file = file, width = width, height = height)
    }
    if(!is.null(top.n)) {
      kept.labels <- names((abs(rowSums(markers.matrix)) %>% sort(decreasing = T))[1:top.n])
    }
    else {
      kept.labels = names(rowSums(markers.matrix !=0)[rowSums(markers.matrix !=0)>=de.cutoff])
    }
    add.flag(p, kept.labels = kept.labels,
             repel.degree = repel.degree)
    if(save){
      dev.off()
    }
  }
  else{
    p
  }
}
