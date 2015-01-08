#' Compute standardized beta coeffizients for linear regression models
#' @export
#' @param MOD A model.
#' @return beta coeffizients.
#' @examples
#' #load("pisa2012che.RData")
#' data <- pisa2012che
#'
#' ## linear regression models with numerical covariates only
#' fit1 <- lm(MATH ~ ESCS + USEMATH, data)
#' betas(fit1)
#'
#' ## linear regression models with numerical and factorial covariates
#' fit2 <- lm(MATH ~ ESCS + USEMATH + ST04Q01 + FAMSTRUC + ST28Q01, data)
#' betas(fit2)
#'
#' ## weighted linear regression models
#' fit3 <- lm(MATH ~ ESCS + USEMATH, data, weights = W_FSTUWT)
#' betas(fit3)
#'
#' fit4 <- lm(MATH ~ ESCS + USEMATH + ST04Q01 + FAMSTRUC + ST28Q01, data, weights = W_FSTUWT)
#' betas(fit4)

betas <- function (MOD) {
  if(class(MOD) != "lm")
    stop("Object must be of class 'lm'")

  model <- MOD$model

  ## coefficients w/o intercept
  b <- MOD$coefficients[-1]

  ## build dummy variables for factors
  m <- sapply(model, function(X) model.matrix(~X-1), simplify = FALSE)

  ## compute sd w/ and w/o weights
  if(is.null(MOD$weights)) {
    sdm <- lapply(m, function(X) apply(X, 2, sd))
  } else {
    w <- unlist(m[length(m)])
    m <- m[-length(m)]
    sdm <- lapply(m, function(X) apply(X, 2, function(Y) sqrt(my.wtd.var(Y, w))))
  }

  ## remove first element for factors
  sdm1 <- lapply(sdm, function(X) if(length(X)>1) X[-1] else X)
  sd <- unlist(sdm1)

  ## beta = b * sd(x)/sd(y)
  beta <- b * sd[-1]/sd[1]
  return(beta)
}

#' Compute standardized beta coeffizients for robust linear regression models
#'
#' @param object A model.
#' @param classic classic cov.
#' @return beta coeffizients.
#' @importFrom robust covRob
#' @importFrom robust covClassic

my.lmr.beta <- function (object, classic = FALSE) {
  if(class(object) != "lmRob")
    stop("Object must be of class 'lmRob'")
  model <- object$model
  #num <- sapply(model, is.numeric)  # numeric vars only
  b <- object$coefficients[-1]  # final coefficients w/o intercept
  ## compute robust covariance
  covr <- NULL
  try(covr <- diag(covRob(model)$cov), silent = TRUE)
  if(is.null(covr) & classic == FALSE)
    warning("covRob() coud not be computed, instead covClassic() was applied.")
  ## compute classic covariance if robust failed
  if(is.null(covr) | classic == TRUE)
    covr <- diag(covClassic(sapply(model, as.numeric))$cov)
  sx <- sqrt(covr[-1])  # standard deviation of x's
  sy <- sqrt(covr[1])  # standard deviation of y
  beta <- b * sx/sy
  return(beta)
}

#' form package Hmisc
#' @param x a numeric vector
#' @param weights a numeric vector of weights
#' @param normwt specify normwt=TRUE to make weights sum to length(x) after deletion of NAs
#' @param
#' @return beta coeffizients.

my.wtd.var <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE)
{
  if (!length(weights)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(var(x))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt)
    weights <- weights * length(x)/sum(weights)
  sw <- sum(weights)
  xbar <- sum(weights * x)/sw
  sum(weights * ((x - xbar)^2))/(sw - sum(weights^2)/sw)
}
