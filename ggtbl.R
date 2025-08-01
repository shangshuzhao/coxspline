require(ggplot2)
require(gridExtra)

ggcoxsp.tbl <- function(spHR, xval, row.name) {
  
  #' @description present the HR results using ggplot2 table
  #' This table has three rows, used when x-axis is transformed
  #' Three rows: transformed predictor, original predictor, HR
  #' @param spHR: the output object of predict.coxsp.
  #' @param xval: values of variable of interest to be displayed.
  #' @param row.name: array of length 1 or 2, for non-transformed or transformed x
  
  xpos <- sapply(xval, function(i){which.min(abs(spHR$Value - i))})
  yhat <- sapply(xpos, function(i){
    sprintf("%.2f\n(%.2f, %.2f)", spHR$HR[i], spHR$CI_L[i], spHR$CI_U[i])
  })
  
  if (length(row.name) == 1) {
    tbl <- rbind(z = xval,
                 `adj. HR\n(95% CI)` = yhat)
    row.names(tbl)[1] <- row.name
  } else if (length(row.name) == 2) {
    tbl <- rbind(z = xval,
                 x = round(spHR$raw[xpos], 2),
                 `adj. HR\n(95% CI)` = yhat)
    row.names(tbl)[1:2] <- row.name
  } else {
    stop("row.name must be a character vector with length 1 or 2")
  }
  
  return(gridExtra::tableGrob(tbl))
}
