# An equal width probability graph, ew_graph, is a finite numeric
# representation of a function c from domain P to codomain H.
# c: P -> H
# P are the real numbers in [0,1].
# H are the real numbers in [0,inf].
# c is interpreted in this context as function that maps from a
# fraction p to the chance of that fraction, h = c(p).
#
# The ew_graph consists of S segments corresponding to S segments of P,
# i.e. of [0,1].
# Let U = 1/(2*S).
# Segment i, i in 1:S, of the ew_graph corresponds to [i/S - U, i/S + U].
# So all segments have equal width, 1/S = 2*U.
# In R we represent the ew_graph as a tibble with S rows.
# Each ith segment of the ew_graph has the following variables:
# - p[[i]] = i/S - U,
#   this means that p[[i]] is the mid point of the ith segment
# - h[[i]] = c(p[[i]])
#   so h[[i]] is the chance of p[[i]]
# - h_left[[i]] which is a linear interpolation of h[[i]] and h[[i-1]]
# - h_right[[i]] which is a linear interpolation of h[[i]] and h[[i+1]]
# - surface[[i]] which is the surface of this ith segment
# - cumsurface[[i]] which is the cumulative of surface[[i]], going from 1 to i
# We can view each segment of the ew_graph as a trapezium, with an extra
# 'spike' in the middle of the top line.
# I call this type of trapezium a pentatrapezium, as it has a fifth vertex,
# the 'spike'. 'penta' is from classic Greek 'five'.
# The base line of the pentatrapezium has length 1/S = 2*U.
# We can view this pentatrapezium as consisting of two trapeziums,
# with a common line, which has as top the spike of the pentatrapezium.
# The base line of these trapeziums has length U.
#
# An equal width graph, ew_graph, can be considered as a refinement
# of a histogram representation of a chance graph.
# The refinement being that the elements of the histogram are
# trapeziums, and not simply bars, only defined by their height and
# width.
# A further refinement is that the trapeziums are penta trapeziums.
# However, the pentatrapezium only might differ for the first and
# last element of the ew graph from a normal trapezium.
# And this difference only occcurs when otherwise the top line of the trapezium
# would hit the vertical axis below 0. For that case we let the top line
# make a corner at p = U, or P is 1 - U.
#
# The disadvantage of this representation is that it adds complexity,
# and makes it more complicated to compute the surface.
# Therefore we do the computation of the surface and the cumulative
# surface beforehand as a service to the user of the ew_graph, and
# we include these in the ew_graph.


# ew_maxcumh_p(g, cert)
# g is an equal width probability graph.
# So it describes a chance curve.
# For a description of equal width probability graphs see above.
#
# Return the lowest (possibly interpolated) value of p for which the cumulative
# chance of h, starting from the first element of g and going to
# the last element of g is >= cert.
ew_maxcumh_p <- function(g, cert) {
  p <- ew_get_p(g)
  h_left <- ew_get_h_left(g)
  h <- ew_get_h(g)
  h_right <- ew_get_h_right(g)
  cumsurface <- ew_get_cumsurface(g)
  surface <- ew_get_surface(g)
  S <- ew_S(g) # Number of segments of ew_graph g.
  U <- 1 / (2 * S) # Length of base line of half segment of ew_graph.

  for (i in 1:S) {
    if (cumsurface[[i]] < cert) {
      # The sought for value of p is more to the right of this element.
      # Continue to next i.
    }
    else if (cumsurface[[i]] == cert) {
      # The  sought for value of p is exactly at the right side of element i.
      return(p[[i]] + U) # Remember: p[[i]] is the middle of the element.
    } else {
      # The  sought for value of p is in this element, element i.
      stopifnot(cumsurface[[i]] > cert)

      # Split surface[[i]] into left and right part.
      # In fact, this is only necessary for the left most and right most element,
      # as these _might_ be penta trapeziums.
      # All other elements are guaranteed to be trapeziums.
      # But we do it for all elements, as this gives cleaner, simpler, code.
      hli <- h_left[[i]]
      hi <- h[[i]]
      hri <- h_right[[i]]
      si <- surface[[i]]
      surface_left <- ((hli + hi) / (hli + 2 * hi + hri)) * si
      surface_right <- ((hri + hi) / (hli + 2 * hi + hri)) * si
      stopifnot(near(surface_left + surface_right, si))

      # Below we need to reference the cumulative surface
      # to the left of element i, cumsurface[[i-1]].
      # However, for S == 1 there is no cumsurface[[i-1]].
      # Therefore we use cumsurface_im1 for cumsurface[[i-1]],
      # and adapt it for the special case where i == 1.
      if (i == 1) {
        cumsurface_im1 <- 0
      } else {
        cumsurface_im1 <- cumsurface[[i - 1]]
      }

      if (cert < cumsurface_im1 + surface_left) {
        # So the sought for value of p is between p[[i]] - U and p[[i]].

        # Surface_delta is the part of surface_left that is left to reach.
        surface_delta <- cert - cumsurface_im1
        stopifnot(0 < surface_delta / surface_left,
                  surface_delta / surface_left < 1)

        # p_delta is the part of p that is left to reach.
        p_delta <- ew_interpolate(
          h1 = hli,
          h2 = hi,
          sd = surface_delta,
          S = S
        )
        return(p[[i]] - U + p_delta)
      } else if (cert == cumsurface_im1 + surface_left) {
        # So the sought for value of p is p[[i]].

        return(p[[i]])
      } else {
        # So the sought for value of p is between p[[i]] and p[[i]] + U.

        # Surface_delta is the part of surface_right that is left to reach.
        surface_delta <- cert - surface_left - cumsurface_im1
        stopifnot(0 < surface_delta / surface_right,
                  surface_delta / surface_right < 1)

        # p_delta is the part of p that is left to reach.
        p_delta <- ew_interpolate(
          h1 = hi,
          h2 = hri,
          sd = surface_delta,
          S = S
        )
        return(p[[i]] + p_delta)
      }
    }
  }
  stop("ew_maxcumh_p(): fall through; should not come here")
}

# ew_mincumh_p(g, cert)
# g is an equal width probability graph.
# So it describes a chance curve.
# For a description of equal width probability graphs see the top of this file.
# Return the highest interpolated value of p for which the cumulative
# chance of h, starting from the last element of g and going to
# the first element of g is <= 1 - cert.
ew_mincumh_p <- function(g, cert) {
  ew_maxcumh_p(g, 1 - cert)
}

# This function, ew_interpolate(),
# computes the error fraction inside the half segments
# of the ew_graph, so for each of the two trapeziums of the pentatrapezium.
# ew_interpolate computes p_delta, alias pd, alias x,
# the length of the part of the base line of the trapezium,
# so that the surface of the trapezium above p_delta equals sd,
# alias surface_delta.
# In a sence this is not interpolation, as the computation is exact, given
# the segment of the ew_graph, and sd.
# However, the ew_graph itself is an approximation of the function c,
# where the top line of the trapezium is the approximation
# as a straight line of a segment of c.
# Therefore I call the function ew_interpolate.
#
# @param h1 The height of the left side of the trapezium.
# @param h2 The height of the right side of the trapezium.
# @param sd Alias for surface_delta:
#           The part of the surface of the chance curve that still needs to be
#           reached. So sd > 0, and smaller than the surface of the
#           trapezium.
# @param S The number of segments, of the equal width graph.
#
# @returns
#   The interpolated value of x, which is the part of the base line of the
#   trapezium that still needs to be reached.
#
# @examples
# x <- ew_interpolate(h1 = 1, h2 = 0, sd = 0.0125, S = 2)
ew_interpolate <- function(h1, h2, sd, S) {
  stopifnot(h1 >= 0, h2 >= 0)
  stopifnot(sd > 0)
  stopifnot(posint(S))

  # U is the length of the base line of the current trapezium.
  # The pentatrapezium consists of two trapeziums, with equal base lines.
  # One of these two trapeziums is the current trapezium.
  # So the lenght of this current trapezium is the length of the
  # base line of the pentatrapezium divided by 2.
  U <- 1 / (2 * S)

  # h1 is left height, h2 is right height, of trapezium.
  # So h1 and h2 >= 0.
  stopifnot(0 <= h1, 0 <= h2)

  # The surface that has to be reached, surface_delta, alias sd,
  # is part of the surface of the trapezium.
  # So it should be > 0, and smaller than that surface.
  stopifnot(0 < sd, sd < U * (h1 + h2) / 2)

  if (h1 == h2) {
    # The trapezium is in fact a rectangle.
    # So the surface, surface_delta, alias sd, that has to be reached by setting
    # x, alias p_delta, alias pd, is computed as sd = h1*x.
    x <- sd / h1
  } else if (h1 < h2) {
    # Using some geometry and algebra, we can solve the equation for x.
    #
    # We have, using some basic geometry:
    # (1) hd/pd = (h2-h1)/U
    # As we have the similar triangles
    # - one with right sides of length hd and pd, and
    # - one with right sides of length h2-h1 and U.
    # (2) sd = pd*h1+pd*hd/2
    # As pd is the sum of:
    # - the surface of the rectangle with sides of length pd and h1
    # - the surface of the right triangle with right sides of length pd and hd
    #
    # hd and pd are unknown.
    # h1, h2, sd, and U are known.
    #
    # Now the algebraic part.
    # Let x = pd.
    # And let r = (h2-h1)/U.
    # We then get:
    # (3) hd/x = r
    # (4) sd = x*(h1+hd/2)
    # We solve for hd in (3).
    # (5) hd = x*r
    # and substitute in (4).
    # We then get a quadratic equation in x:
    # (6) sd = x*(h1 + x*r/2))
    # Which we can normalize to:
    # (7) (r/2)*x^2 + x*h1 - sd = 0
    # We solve this equation for x.
    # We get two solutions:
    # (8) x = (-h1 + sqrt(h1^2 + 2*r*sd))/r
    # (9) x = (-h1 - sqrt(h1^2 + 2*r*sd))/r
    # However, (9) is invalid, as (9) implies that x < 0.
    # So only solution (8) is valid.
    r <- (h2 - h1) / U
    x <- (-h1 + sqrt(h1 ^ 2 + 2 * r * sd)) / r

    # Check whether the original equations are satisfied by the
    # solution found.
    pd <- x
    hd <- x * r
    stopifnot(near(hd / pd, (h2 - h1) / U)) # (1)
    stopifnot(near(sd, pd * (h1 + hd / 2))) # (2)
  } else {
    # h1 > h2
    # For this case we can use the mathematical analyses above for h1 < h2.
    # But we have to take into account that
    # the roles of h1 and h2 are reversed here.
    # So we swap h1 and h2.
    temp <- h1
    h1 <- h2
    h2 <- temp
    rm(temp)

    # And sd can now be seen as the part of the complementary part
    # of the trapezium.
    sd <- (h2 + h1) * U / 2 - sd

    # We can now copy the solution from the case h1 < h2.
    r <- (h2 - h1) / U
    x <- (-h1 + sqrt(h1 ^ 2 + 2 * r * sd)) / r

    # Check whether the original equations are satisfied by the
    # solution found.
    pd <- x
    hd <- x * r
    stopifnot(near(hd / pd, (h2 - h1) / U)) # (1)
    stopifnot(near(sd, pd * (h1 + hd / 2))) # (2)

    # We have computed x for the complementary part of the trapezium.
    # So we have to complement x to get pd for the original part of the trapezium.
    x <- U - x
  }
  stopifnot(0 <= x, x <= U)
  x
}
