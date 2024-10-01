#' linreg function
#'
#' Fits a linear regression model using QR decomposition.
#'
#' @param formula A formula object describing the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @return An object of class linreg containing the fitted model.
#' @export linreg
#' @importFrom stats model.matrix
linreg <- function(formula, data) {
  
  # Validate the input
  if(!inherits(formula, "formula")) {
    stop("The formula argument must be a formula object.")
  }
  
  if(!is.data.frame(data)) {
    stop("The data argument must be a data frame.")
  }
  
  # Create design matrix X and response vector y
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  
  # QR decomposition of X
  qr_dec <- qr(X)
  Q <- qr.Q(qr_dec)
  R <- qr.R(qr_dec)
  
  # Calculate beta coefficients
  beta <- solve(R, t(Q) %*% y)
  names(beta) <- colnames(X)
  
  # Calculate fits and residuals
  y_hat <- X %*% beta
  resid <- y - y_hat
  
  # Variance of residuals
  n <- nrow(X)
  p <- ncol(X)
  sigma2 <- sum(resid^2) / (n - p)
  
  # Variance of betas
  var_beta <- sigma2 * solve(t(X) %*% X)
  
  # Return S3 object of class linreg
  return(structure(list(
    coefficients = beta,
    residuals = resid,
    fitted_values = y_hat,
    formula = formula,
    data = data,
    name = deparse(substitute(data)),
    sigma2 = sigma2,
    var_beta = var_beta,
    df_residual = n - p
  ), class = "linreg"))
}

#' Print method for linreg class
#' @param x An object of class linreg.
#' @export print.linreg
print.linreg <- function(x) {
  cat(paste0("linreg(formula = ",deparse(x$formula), ", data = ", x$name, ")"))
  cat("\n\nCoefficients:\n")
  cat(names(x$coefficients))
  cat(paste0("\n", t(x$coefficients)))
}

#' Plot method for linreg class
#' @param x An object of class linreg.
#' @export plot.linreg
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 stat_summary
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom stats median

plot.linreg <- function(x) {
  data <- x$data
  y_hat <- x$fitted_values
  residuals <- x$residuals
  
  # Standardized residuals
  std_residuals <- residuals / sqrt(abs(x$sigma2))
  
  # Create data frame for plotting
  plot_data <- data.frame(y_hat, residuals, std_residuals)
  
  # Residuals vs Fitted values
  p1 <- ggplot2::ggplot(plot_data, aes(x = y_hat, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    stat_summary(fun = median, color = "red", geom = "line", group = 1) +
    labs(title = "Residuals vs Fitted", 
         x = paste0("Fitted values\n", "linreg(", deparse(x$formula), ")"), y = "Residuals") +
    theme_minimal()
  
  # Scale-Location
  p2 <- ggplot2::ggplot(plot_data, aes(x = y_hat, y = std_residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    stat_summary(fun = median, color = "red", geom = "line", group = 1) +
    labs(title = "Scale-Location", 
         x = paste0("Fitted values\n", "linreg(", deparse(x$formula), ")"), y = "Standardized residuals") +
    theme_minimal()
  
  # Plot both graphs
  print(p1)
  print(p2)
}

#' Residuals method for linreg class
#' @param x An object of class linreg.
#' @export resid.linreg
resid.linreg <- function(x) {
  return(as.vector(x$residuals))
}

#' Coefficients method for linreg class
#' @param x An object of class linreg.
#' @export coef.linreg
coef.linreg <- function(x) {
  coeff <- as.vector(x$coefficients)
  names(coeff) <- "Coefficients"
  return(coeff)
}

#' Summary method for linreg class
#' @param x An object of class linreg.
#' @export summary.linreg
#' @importFrom stats pt
summary.linreg <- function(x) {
  # Calculate standard errors, t-values, and p-values
  se <- sqrt(diag(x$var_beta))
  t_values <- x$coefficients / se
  p_values <- 2 * pt(-abs(t_values), df = x$df_residual)
  
  # Coefficients table
  coef_table <- data.frame(
    Estimate = x$coefficients,
    SE = se,
    t = t_values,
    P = p_values
  )
  print(round(coef_table, digits = 3))
  
  cat("\nResidual standard error:", round(sqrt(x$sigma2), digits = 3), "on", x$df, "degrees of freedom\n")
}

#' Predicted values method for linreg class
#' @param x An object of class linreg.
#' @export pred.linreg
pred.linreg <- function(x) {
  return(as.vector(x$fitted_values))
}