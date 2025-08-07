#' Compute Maximum of Finite Values Only
#'
#' Returns the maximum of all finite (non-NA, non-NaN, non-Infinite) values in a vector.
#' If no finite values exist, returns the specified fallback value.
#'
#' @param x A numeric vector.
#' @param fallback A value to return if `x` contains no finite values. Defaults to `NA`.
#'
#' @return A single numeric value: the maximum of all finite values in `x`, or `fallback`.
#'
#' @noRd
robust_max <- function(x, fallback = NA) {
  finite_values <- x[is.finite(x)]

  if (length(finite_values) == 0) {
    return(fallback)
  } else {
    max(finite_values)
  }
}
