###############################################################################

#' Bounding to numerical precision
#'
#' Bounds extreme values to a specified tolerance level, for use with sensitive
#' quantities that must be transformed, e.g., via \code{\link[stats]{qlogis}}.
#'
#' @param vals A \code{numeric} vector of values in the interval [0, 1].
#' @param tol A \code{numeric} indicating the tolerance limit to which extreme
#'  values should be truncated. Realizations of \code{val} less than \code{tol}
#'  are truncated to \code{tol} while those greater than (1 - \code{tol}) are
#'  truncated to (1 - \code{tol}).
#'
#' @importFrom assertthat assert_that
#'
#' @keywords internal
bound_precision <- function(vals, tol = 1e-6) {
  vals[vals < tol] <- tol
  vals[vals > 1 - tol] <- 1 - tol
  return(vals)
}

###############################################################################

#' Bounding propensity scores
#'
#' Bounds estimated propensity score values to be within a specified range.
#'
#' @param vals A \code{numeric} vector of values in the interval [0, 1].
#' @param bounds A \code{numeric} vector containing two values, the first being
#'  the minimum allowable value and the second being the maximum allowable for
#'  values appearing in the vector \code{vals} (the previous argument).
#'
#' @importFrom assertthat assert_that
#'
#' @keywords internal
bound_propensity <- function(vals, bounds = c(0.001, 0.999)) {
  assertthat::assert_that(!(max(vals) > 1 || min(vals) < 0))
  vals[vals < bounds[1]] <- bounds[1]
  vals[vals > bounds[2]] <- bounds[2]
  return(vals)
}

###############################################################################

#' Scale values to the unit interval
#'
#' @param vals A \code{numeric} vector of values to be scaled into the closed
#'  interval [0, 1].
#'
#' @keywords internal
scale_to_unit <- function(vals) {
  vals_scaled <- (vals - min(vals)) / (max(vals) - min(vals))
  return(vals_scaled)
}

###############################################################################

#' Scale values from the unit interval to their original scale
#'
#' @param scaled_vals A \code{numeric} vector of values scaled to lie in the
#'  closed interval [0, 1] by use of \code{\link{scale_to_unit}}.
#' @param max_orig The maximum of the values on the original scale.
#' @param min_orig The minimum of the values on the original scale.
#'
#' @keywords internal
scale_from_unit <- function(scaled_vals, max_orig, min_orig) {
  vals_orig <- (scaled_vals * (max_orig - min_orig)) + min_orig
  return(vals_orig)
}
