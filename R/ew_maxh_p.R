# ew_maxh_p(g)
# g is an equal width probability graph.
# So it describes a chance curve.
# Return the (possibly interpolated) value of p for which h, h_left, or h_right
# is highest of all h, h_left and h_right.
#
# For a description of equal width probability graphs see ew_minmaxcumh_p.R.
#
ew_maxh_p <- function(g) {
  h <- ew_get_h(g)
  h_left <- ew_get_h_left(g)
  h_right <- ew_get_h_right(g)

  p <- ew_get_p(g)
  S <- ew_S(g)
  U <- 1/(2*S)

  if (min(h) == max(h) && min(h_left) == max(h_left) && min(h_right) == max(h_right)) {
    # Chance curve ew is a straight horizontal line.
    return(0.5) # Interpolated, i.e. not necessarily a value of h[[i]] for some i.
  }

  i_max_h <- which.max(h)
  i_max_h_left <- which.max(h_left)
  i_max_h_right <- which.max(h_right)

  max_hs <- c(l = h_left[[i_max_h_left]], m = h[[i_max_h]], r = h_right[[i_max_h_right]])
  max_hs_sorted <- sort(max_hs, decreasing = TRUE)
  highest <- names(max_hs_sorted)[1]

  if (highest == "l") { # Interpolated, i.e. not necessarily a value of h[[i]] for some i.
    return(p[[i_max_h_left]] - U)
  }

  if (highest == "m") { # Not interpolated.
    return(p[[i_max_h]])
  }

  if (highest == "r") { # Interpolated, i.e. not necessarily a value of h[[i]] for some i.
    return(p[[i_max_h_right]] + U)
  }

  stop("ew_maxh_p(): fall through; should not come here")
}
