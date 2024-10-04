# Add surface and cumulative surface to elements of equal width graph.
#
# @param g should be an equal width graph, represented as a tibble with
# vectors p and h, h_left and h_right.
#
# For a description of equal width probability graphs see ew_minmaxcumh_p.R.
#
# @returns
# The augmented graph as a tibble.
#
# @examples
# g <- ew_add_surface(g)
ew_add_surface <- function(g) {
  # Get number of elements, i.e. rows, from g.
  S <- ew_S(g)

  # Create vector to represent surface of the basic elements of the graph.
  surface <- double(S)

  # Compute surfaces.
  h_left <- ew_get_h_left(g)
  h_right <- ew_get_h_right(g)
  h <- ew_get_h(g)
  surface <- h_left + 2 * h + h_right # Normalization is done later.

  # Normalize the chance mass of the pentatrapeziums to 1.
  chance_mass <- sum(surface)
  surface <- surface / chance_mass

  # Add cumulative surfaces.
  cumsurface <- cumsum(surface)

  # Add columns to g.
  g <- tibble::add_column(g, surface = surface, cumsurface = cumsurface)

  g
}
