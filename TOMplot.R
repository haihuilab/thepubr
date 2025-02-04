TOMplot1 <- function (dissim, dendro, Colors = NULL, ColorsLeft = Colors, 
                      terrainColors = FALSE, setLayout = TRUE, ...) 
{
  if (is.null(Colors)) 
    Colors = rep("white", dim(as.matrix(dissim))[[1]])
  if (is.null(ColorsLeft)) 
    ColorsLeft = Colors
  nNodes = length(Colors)
  if (nNodes < 2) {
    warning("You have only 1 or 2 genes in TOMplot. No plot will be produced")
  }
  else {
    if (nNodes != length(ColorsLeft)) 
      stop("ERROR: number of (top) color labels does not equal number of left color labels")
    if (nNodes != dim(dissim)[[1]]) 
      stop(paste("ERROR: number of color labels does not equal number of nodes in dissim.\n", 
                 "     nNodes != dim(dissim)[[1]] "))
    labeltree = as.character(Colors)
    labelrow = as.character(ColorsLeft)
    options(expressions = 10000)
    dendro$height = (dendro$height - min(dendro$height))/(1.15 * 
                                                            (max(dendro$height) - min(dendro$height)))
    if (terrainColors) {
      .heatmap(as.matrix(dissim), Rowv = dendro, Colv = dendro, 
               scale = "none", revC = TRUE, ColSideColors = as.character(labeltree), 
               RowSideColors = as.character(labelrow), labRow = FALSE, 
               labCol = FALSE, col = terrain.colors(100), setLayout = setLayout, 
               ...)
    }
    else {
      .heatmap(as.matrix(dissim), Rowv = dendro, Colv = dendro, 
               scale = "none", revC = TRUE, ColSideColors = as.character(labeltree), 
               RowSideColors = as.character(labelrow), labRow = FALSE, 
               labCol = FALSE, setLayout = setLayout, ...)
    }
  }
}

###############################################################################
.heatmap <- function (x, Rowv = NULL, Colv = if (symm) "Rowv" else NULL, 
                      distfun = dist, hclustfun = fastcluster::hclust, reorderfun = function(d, 
                                                                                             w) reorder(d, w), add.expr, symm = FALSE, revC = identical(Colv, 
                                                                                                                                                        "Rowv"), scale = c("row", "column", "none"), na.rm = TRUE, 
                      margins = c(1.2, 1.2), ColSideColors, RowSideColors, cexRow = 0.2 + 
                        1/log10(nr), cexCol = 0.2 + 1/log10(nc), labRow = NULL, 
                      labCol = NULL, main = NULL, xlab = NULL, ylab = NULL, keep.dendro = FALSE, 
                      verbose = getOption("verbose"), setLayout = TRUE, hang = 0.04, 
                      ...) 
{
  scale <- if (symm && missing(scale)) 
    "none"
  else match.arg(scale)
  if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
    stop("'x' must be a numeric matrix")
  nr <- di[1L]
  nc <- di[2L]
  if (nr <= 1 || nc <= 1) 
    stop("'x' must have at least 2 rows and 2 columns")
  if (!is.numeric(margins) || length(margins) != 2L) 
    stop("'margins' must be a numeric vector of length 2")
  doRdend <- !identical(Rowv, NA)
  doCdend <- !identical(Colv, NA)
  if (!doRdend && identical(Colv, "Rowv")) 
    doCdend <- FALSE
  if (is.null(Rowv)) 
    Rowv <- rowMeans(x, na.rm = na.rm)
  if (is.null(Colv)) 
    Colv <- colMeans(x, na.rm = na.rm)
  if (doRdend) {
    if (inherits(Rowv, "hclust")) 
      ddr <- Rowv
    else {
      hcr <- hclustfun(distfun(x))
      if (inherits(hcr, "hclust")) {
        hcr$height = hcr$height - min(hcr$height) + 
          hang * (max(hcr$height) - min(hcr$height))
      }
      ddr = hcr
    }
    rowInd = ddr$order
  }
  else rowInd <- 1:nr
  if (doCdend) {
    if (inherits(Colv, "hclust")) 
      ddc <- Colv
    else if (identical(Colv, "Rowv")) {
      if (nr != nc) 
        stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
      ddc <- ddr
    }
    else {
      hcc <- hclustfun(distfun(if (symm) 
        x
        else t(x)))
      if (inherits(hcr, "hclust")) {
        hcc$height = hcc$height - min(hcc$height) + 
          hang * (max(hcc$height) - min(hcc$height))
      }
      ddc = hcc
    }
    colInd = ddc$order
  }
  else colInd <- 1:nc
  x <- x[rowInd, colInd]
  labRow <- if (is.null(labRow)) 
    if (is.null(rownames(x))) 
      (1:nr)[rowInd]
  else rownames(x)
  else labRow[rowInd]
  labCol <- if (is.null(labCol)) 
    if (is.null(colnames(x))) 
      (1:nc)[colInd]
  else colnames(x)
  else labCol[colInd]
  if (scale == "row") {
    x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
    sx <- apply(x, 1, sd, na.rm = na.rm)
    x <- sweep(x, 1, sx, "/")
  }
  else if (scale == "column") {
    x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
    sx <- apply(x, 2, sd, na.rm = na.rm)
    x <- sweep(x, 2, sx, "/")
  }
  lmat <- rbind(c(NA, 3), 2:1)
  lwid <- c(if (doRdend) 1 else 0.05, 4)
  lhei <- c((if (doCdend) 1 else 0.05) + if (!is.null(main)) 0.5 else 0, 
            4)
  if (!missing(ColSideColors)) {
    if (!is.character(ColSideColors) || length(ColSideColors) != 
        nc) 
      stop("'ColSideColors' must be a character vector of length ncol(x)")
    lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
    lhei <- c(lhei[1], 0.2, lhei[2])
  }
  if (!missing(RowSideColors)) {
    if (!is.character(RowSideColors) || length(RowSideColors) != 
        nr) 
      stop("'RowSideColors' must be a character vector of length nrow(x)")
    lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 
                                         1), 1), lmat[, 2] + 1)
    lwid <- c(lwid[1], 0.2, lwid[2])
  }
  lmat[is.na(lmat)] <- 0
  if (verbose) {
    cat("layout: widths = ", lwid, ", heights = ", lhei, 
        "; lmat=\n")
    print(lmat)
  }
  if (!symm || scale != "none") 
    x <- t(x)
  op <- par(no.readonly = TRUE)
  if (revC) {
    iy <- nc:1
    ddr$order = rev(ddr$order)
    rowInd.colors = rev(rowInd)
    x <- x[, iy]
  }
  else {
    iy <- 1:nr
    rowInd.colors = rowInd
  }
  if (setLayout) 
    layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
  if (!missing(RowSideColors)) {
    par(mar = c(margins[1], 0, 0, 0.5))
    image(rbind(1:nr), col = RowSideColors[rowInd.colors], 
          axes = FALSE)
  }
  if (!missing(ColSideColors)) {
    par(mar = c(0.5, 0, 0, margins[2]))
    image(cbind(1:nc), col = ColSideColors[colInd], axes = FALSE)
  }
  par(mar = c(margins[1], 0, 0, margins[2]))
  image(x = 1:nc, y = 1:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
          c(0, nr), axes = FALSE, xlab = "", ylab = "", ...)
  axis(1, 1:nc, labels = labCol, las = 2, line = -0.5, tick = 0, 
       cex.axis = cexCol)
  if (!is.null(xlab)) 
    mtext(xlab, side = 1, line = margins[1] - 1.25)
  axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0, 
       cex.axis = cexRow)
  if (!is.null(ylab)) 
    mtext(ylab, side = 4, line = margins[2] - 1.25)
  if (!missing(add.expr)) 
    eval.parent(substitute(add.expr))
  par(mar = c(margins[1], 0, 0, 0))
  if (doRdend) {
    .plotDendrogram(ddr, horiz = TRUE, labels = FALSE, axes = FALSE, 
                    adjustRange = TRUE)
  }
  else frame()
  par(mar = c(0, 0, if (!is.null(main)) 1.8 else 0, margins[2]))
  if (doCdend) {
    .plotDendrogram(ddc, horiz = FALSE, labels = FALSE, 
                    axes = FALSE, adjustRange = TRUE)
  }
  else if (!is.null(main)) 
    frame()
  if (!is.null(main)) 
    title(main, cex.main = 1.2 * op[["cex.main"]])
  invisible(list(rowInd = rowInd, colInd = colInd, Rowv = if (keep.dendro && 
                                                              doRdend) ddr, Colv = if (keep.dendro && doCdend) ddc))
}