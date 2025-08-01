require(ggplot2)

ggcoxsp.plt <- function(
    spHR,
    xlim = NULL, ylim = NULL, xtik = NULL, ytik = NULL,
    xlab = NULL, ylab = NULL, main = NULL
) {
  
  #' @description Plot association HR results using ggplot2
  #' @details Association between outcome and one predictor
  #' @param spHR output of predict.coxsp function
  #' @param spHR_unadjust output of predict.coxsp function of unadjusted model
  
  plt <- ggplot(spHR, aes(x = Value, y = HR, colour = label)) +
    geom_line() +
    geom_line(aes(y = CI_L), linetype = "dashed") +
    geom_line(aes(y = CI_U), linetype = "dashed") +
    geom_hline(yintercept = 1, colour = "grey60", linetype = "longdash") +
    scale_color_manual(values = c("darkslateblue", "peru", "orchid4", "forestgreen")) +
    labs(title = main, x = xlab, y = ylab) +
    theme_bw() +
    theme(axis.title  = element_text(size = 15),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          aspect.ratio = 1)
  
  # add legend only if there are more than two groups
  if (length(unique(spHR$label)) == 1) {
    plt <- plt + theme(legend.position = "none")
  } else {
    plt <- plt + theme(
      legend.position        = "inside",
      legend.position.inside = c(0.5, 0.9),
      legend.text            = element_text(size = 15),
      legend.title           = element_blank(),
      legend.background      = element_blank(),   # Remove overall border
      legend.key             = element_blank(),   # Remove border around each item
    )
  }
  
  # specify axis limit and axis ticks if needed
  if (!is.null(xlim)) plt <- plt + coord_cartesian(xlim = xlim)
  if (!is.null(ylim)) plt <- plt + coord_cartesian(ylim = ylim)
  if (!is.null(xtik)) plt <- plt + scale_x_continuous(breaks = xtik)
  if (!is.null(ytik)) plt <- plt + scale_y_continuous(breaks = ytik)
  
  return(plt)
}
