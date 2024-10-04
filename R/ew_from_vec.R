# Create an equal width probabilty graph from vector v,
# whereby we assume that the elements of the vector
# represent the probabilities 1/S - 1/2S, ..., S/S - 1/2S.
# S is the number of elements, so the length, of the vector.
#
# For a description of equal width probability graphs see ew_minmaxcumh_p.R.
#
ew_from_vec <-
  function(v) {
    S <- length(v)

    # Check v.
    stopifnot(is.numeric(v)) # v should be a numeric vector.
    stopifnot(all(v >= 0)) # v should contain only non negative numbers.
    stopifnot(S >= 1)  # v should contain at least 1 element.

    # Create initial histogram from v by dividing [0,1] into S segments.
    # The total of the surface in the histogram is not yet normalized to 1.
    p <- ew_partition_0_1(S)
    h <- v

    # Normalize the surface of the histogram to 1.
    surface <- sum(h) / S
    h <- h / surface

    # Combine vectors p and h into one tibble.
    r <- tibble(p, h)

    # Add h_left and h_right to each element of the graph.
    # So now each element can be considered a penta trapezium, where
    # h_left and h_right are the heights of the left and right
    # side of the trapezoid, and the middle of the upper line
    # between h_left and h_right has height h, and is located
    # at p.
    r <- ew_add_h_leftright(r)

    # Add surface and cumulative surface of the penta trapezium to each element
    # of the graph.
    # Just as in the histogram, the sum of the surfaces of all the trapeziums
    # is also normalized to 1.
    r <- ew_add_surface(r)

    # Designate r as of class ew_probability_graph.
    class(r) <- c("ew_probability_graph", class(r))

    # Check.
    stopifnot(ew_validate(r))

    r
  }
