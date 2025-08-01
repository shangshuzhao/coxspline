predict.coxsp <- function(object, varraw, vartran, ref = "mean") {
  
  #' @description
    #' compute the prediction of a coxph model with splines function
    #' compatible with splines::bs, splines::ns, rms::rcs
  #' @param object: coxph model object
  #' @param varraw: the original variable of interest
  #' @param vartran: the transformed variable of interest
  #' @param ref: reference value of the hazard ratio
  #' @returns
  #' data frame with the following columns:
  #' data.frame$raw: original variable of interest value
  #' data.frame$Value: transformaed variable of interest
  #' data.frame$terms: the linear terms of coxph regression
  #' data.frame$HR: hazard ratio with repsect to reference
  #' data.frame$SE: the standard variance of fitted value
  #' data.frame$CI: Lower and Upper bound of confidence interval
  #' data.frame$label: the model label
  
  # default reference level is the mean
  if (ref == "mean") {
    ref <- mean(vartran, na.rm = TRUE)
    ref <- which.min(abs(vartran - ref))
  } else {
    ref <- which.min(abs(vartran - 0))
  }
  
  tt <- object$assign[[1]]
  x <- model.matrix(object)[,tt,drop = FALSE]
  
  newx <- x - rep(x[ref,], each = nrow(x))
  pred <- as.vector(newx %*% object$coefficient[tt])
  se <- as.vector(sqrt(rowSums((newx %*% object$var[tt,tt]) *newx)))

  res <- data.frame(
    raw   = varraw,
    Value = vartran,
    term  = pred,
    HR    = exp(pred),
    SE    = se,
    CI_L  = exp(pred - qnorm(1 - 0.05/2) * se),
    CI_U  = exp(pred + qnorm(1 - 0.05/2) * se)
  )
  
  return(res)
}