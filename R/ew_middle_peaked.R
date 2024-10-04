# g is an equal width probability graph.
# So it describes a chance curve.
# Return TRUE iff the highest value of h is reached
# neither for the h_left of the first element
# nor for the h_right of the last element.
#
# For a description of equal width probability graphs see ew_minmaxcumh_p.R.
#
ew_middle_peaked <- function(g) {
  # Get h_left.
  h_left <- ew_get_h_left(g)

  # Get index of highest element of h_left.
  i_max_h_left <- which.max(h_left)

  # Get h_right.
  h_right <- ew_get_h_right(g)

  # Get index of highest element of h_right.
  i_max_h_right <- which.max(h_right)

  # Check that neither h_left[[1]] nor h_right[[S]] is the highest value.
  S <- ew_S(g)
  i_max_h_left != 1 && i_max_h_right != S
}
