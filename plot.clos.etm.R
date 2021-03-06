# plot.clos.etm.R
# downloaded from github https://raw.githubusercontent.com/aallignol/etm/master/R/plot.clos.etm.R
# to fix margins issue

plot.clos.etm <- function(x, xlab = "Time",
                          ylab.e = "Expected LOS", ylab.w = "Weights",
                          xlim, ylim.e, ylim.w, col.e = c(1, 2), col.w = 1,
                          lty.e = c(1, 1), lty.w = 1, legend = TRUE,
                          xtick, # added by AGB
                          legend.pos, curvlab, legend.bty = "n", ...) {
  if (!inherits(x, "clos.etm")) {
    stop("'x' must be a 'clos.etm' object")
  }
  if (missing(xlim)) {
    xlim <- c(0, max(x$w.time))
  }
  if (missing(ylim.e)) {
    ylim.e <- c(0, max(c(x$phi.case, x$phi.control)))
  }
  if (missing(ylim.w)) {
    ylim.w <- c(0, max(x$weights))
  }
  #
  def.par <- graphics::par(no.readonly = TRUE)
  on.exit(par(def.par))
  # first plot of weights
  layout(mat=1:2, widths=c(1,1.5)) # new from AGB
  par(mai=c(0.2,0.9,0.1,0.1))
  graphics::plot(c(0,x$w.time), c(0, x$weights), type = "s", axes = FALSE, lty = lty.w, xlim = xlim,
                 ylim = ylim.w , xlab = '' , ylab = ylab.w, col=col.w, ...)
  graphics::axis(side=2)
  graphics::axis(side=1, at=xtick, labels = FALSE) # new from AGB
  graphics::box()
#  graphics::par(op)
#  graphics::screen(1)      
#  op <- graphics::par(mar=c(5, 5, 4, 1))
  # second plot of differences
  par(mai=c(0.9,0.9,0.1,0.1))
  graphics::plot(x$time, x$phi.case, type = "s", lty = lty.e[1], xlim = xlim,
                 ylim = ylim.e, xlab = xlab, ylab = ylab.e, col = col.e[1], ...)
  graphics::lines(x$time, x$phi.control, type = "s", lty = lty.e[2], col = col.e[2], ...)
#  graphics::par(op)
  if (legend == TRUE) {
    if (missing(legend.pos))
      legend.pos <- "bottomright"
    if (missing(curvlab))
      curvlab <- c("Intermediate event by time t", "No intermediate event by time t")
    if (is.list(legend.pos)) legend.pos <- unlist(legend.pos)
    if (length(legend.pos) == 1) {
      xx <- legend.pos
      yy <- NULL
    }
    if (length(legend.pos) == 2) {
      xx <- legend.pos[1]
      yy <- legend.pos[2]
    }
    args <- list(...)
    ii <- pmatch(names(args),
                 names(formals("legend")[-charmatch("bty",names(formals("legend")))]))
    do.call("legend", c(list(xx, yy, curvlab, col = col.e, lty = lty.e, bty = legend.bty),
                        args[!is.na(ii)]))
  }
  #graphics::close.screen(all.screens = TRUE)
  invisible()
}