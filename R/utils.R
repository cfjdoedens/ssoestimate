nonnegint <- function(i) {
  if (!is.numeric(i)) {
    return(FALSE)
  }
  if (i < 0) {
    return(FALSE)
  }
  if (!(round(i) == i)) {
    return(FALSE)
  }

  TRUE
}

posint <- function(i) {
  nonnegint(i) && i > 0
}

#' Partition \[0,1\] into S equally sized consecutive segments. Each segment is given by its midpoint.
#'
#' @param S An integer >= 1. The number of segments to partition \[0,1\] into.
#' @returns The vector of segment midpoints, c(\code{(1-0.5)/S}, \code{(2-0.5)/S}, ... (\code{(S-0.5)/S}).
#  @examples
#    partition_0_1(S = 5)
#      Returns the vector c(0.1, 0.3, 0.5, 0.7, 0.9).
partition_0_1 <- function(S = 1000) {
  seq(0 + 1 / (2 * S), 1 - 1 / (2 * S), 1 / S)
}

# Transform a vector v to an ew_probability_graph,
# whereby we assume that the elements of the vector
# represent the probabilities 1/S - 1/2S, ..., S/S - 1/2S.
# S is the length of the vector.
vec_to_ew_probability_graph <-
  function(v) {
    S <- length(v)

    # Create initial histogram from v by dividing [0,1] into S segments.
    # The sum of the area in the histogram is not yet normalized to 1.
    p <- partition_0_1(S)
    h <- v

    # Normalize the chance mass of the histogram to 1.
    surface <- sum(h) / S

    # When the surface is 0 it follows that all values of
    # h are 0.
    # We assume that this means that all values of h
    # are equally (more or less) unlikely.
    # So we make each value of h equally likely.
    if (surface == 0) {
      h <- rep(1, S)
      surface <- sum(h) / S
    }
    h <- h / surface

    # Combine vectors p and h into one tibble.
    r <- tibble(p, h)

    # Designate r as of class ew_probability_graph.
    class(r) <- c("ew_probability_graph", class(r))

    # Check.
    stopifnot(validate_ew_probability_graph(r))

    r
  }

ew_get_p <- function(ew) {
  ew %>% pull("p")
}

ew_get_h <- function(ew) {
  ew %>% pull("h")
}

validate_ew_probability_graph <- function(ew, verbose = TRUE) {
  # When we have to do with a list of (supposed) ew_probability_graphs,
  # we call validate_ew_probability_graph() recursively.
  if (class(ew)[[1]] == "list") {
    r <- sapply(ew, validate_ew_probability_graph)
    return(min(r) > 0) # Only TRUE if all values in r are TRUE
  }

  # Single (supposed) ew_probability_graph to validate.
  # ew should be a tibble.
  if (!is_tibble(ew)) {
    if (verbose) {
      message("ew_probability_graph should be a tibble")
    }
    return(FALSE)
  }

  # ew should have columns p and h.
  if (!("p" %in% colnames(ew))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector p")
    }
    return(FALSE)
  }
  if (!("h" %in% colnames(ew))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector h")
    }
    return(FALSE)
  }

  # p and h should have the same number of elements.
  p <- ew_get_p(ew)
  h <- ew_get_h(ew)

  if (!(length(p) == length(h))) {
    if (verbose) {
      message("length of vectors p and h in ew_probability_graph should be equal")
      message(sprintf("found length of p =  %i", length(p)))
      message(sprintf("found length of h =  %i", length(h)))
    }
    return(FALSE)
  }

  # p and h should contain at least 1 element.
  if (!(length(p) > 0)) {
    if (verbose) {
      message("length of vector p and h in ew_probability_graph should be >=1")
      message(sprintf("found length of p =  %i", length(p)))
      message(sprintf("found length of h =  %i", length(h)))
    }
    return(FALSE)
  }

  # p should be equal to partition(S), where S is the number of elements of h.
  S <- length(h)
  p2 <- partition_0_1(S)
  if (!isTRUE(all.equal(p, p2))) {
    if (verbose) {
      message("vector p of ew_probability_graph should be equal to partition_0_1(length(h))")
    }
    return(FALSE)
  }

  # The total area of the rectangles pxh should be 1.
  if (!near(sum(h) / S, 1)) {
    if (verbose) {
      message("total chance mass of ew_probability_graph should be 1")
      message(sprintf("found chance mass =  %1.5f", sum(h) / S))
      message(sprintf("number of rectangles in the chance graph = %i", S))
    }
    return(FALSE)
  }

  return(TRUE)
}

# g is an ew graph.
# So it is a tibble with two columns, h and p which together describe
# a chance curve.
# h describes the height of the chance for p.
# Return TRUE iff the highest value of h
# is not reached for the first or the last element of h.
ew_middle_peeked <- function(g) {
  h <- ew_get_h(g)
  i_max_h <- which.max(h)
  S <- length(h)
  i_max_h != 1 && i_max_h != S
}

# ew_maxh_p(g)
#   g is an ew graph. So it is a tibble with two columns, h and p which together
#   describe a chance curve. h describes the height of the chance for p. Return
#   the value of p for which h is highest.
ew_maxh_p <- function(g) {
  h <- ew_get_h(g)
  p <- ew_get_p(g)
  i_max_h <- which.max(h)
  p[[i_max_h]]
}

# ew_mincumh_p(g, cert)
#   g is an ew graph.
#   So it is a tibble with two columns, h and p which together describe
#   a chance curve.
#   h describes the height of the chance for p.
#   Return the highest interpolated (!) value of p for which the cumulative
#   chance of h, starting from p[[1]] and going to p[[length(p)]]
#   is <= 1 - cert.
ew_mincumh_p <- function(g, cert) {
  h <- ew_get_h(g)
  p <- ew_get_p(g)

  # Create cumulative probability mass vector for h.
  cumh <- cumsum(h) / length(h)

  S <- length(h)
  for (i in 1:S) {
    if (cumh[[i]] < 1 - cert) {
      if (i == S) {
        # This should not happen, as this would mean
        # cumh[[S]] < 1 - cert
        # So 1 < 1- cert, where cert > 0.
        stop("ew_mincumh_p: 1 < 1- cert, should not be possible" )
      } else {
        # Continue to next i.
      }
    } else if (cumh[[i]] == 1 - cert) {
      return(p[[i]])
    } else if (cumh[[i]] > 1 - cert) {
      if (i == 1) {
        return((0 + p[[i]]) / 2) # This is (0 + 1/2S) / 2 = 1/4S.
      } else if (i == S) {
        return((p[[i]] + 1) / 2) # This is ((1 - 1/2S) + 1) / 2 = 1 - 1/4S.
      } else {
        return((p[[i-1]] + p[[i]]) / 2)
        # return(p[[i-1]] + (1/S)*((1 - cert) - cumh[[i-1]])/(cumh[[i]] - cumh[[i-1]]))
      }
    }
  }

  stop("ew_mincumh_p(): fall through; should not come here")
}

# ew_maxcumh_p(g)
#   g is an ew graph.
#   So it is a tibble with two columns, h and p which together describe
#   a chance curve.
#   h describes the height of the chance for p.
#   Return the lowest interpolated (!) value of p for which the cumulative
#   chance of h, starting from p[[length(p)]] and going to p[[1]]
#   is <= cert.
ew_maxcumh_p <-  function(g, cert) {
  h <- ew_get_h(g)
  p <- ew_get_p(g)

  # Create cumulative probability mass vector for h.
  cumh <- cumsum(h) / length(h)

  S <- length(h)
  for (i in S:1) {
    if (cumh[[i]] > cert) {
      if (i == 1) {
        return((0 + p[[i]]) / 2) # This is (0 + 1/2S) / 2 = 1/4S.
      } else {
        # Continue to next i.
      }
    } else if (cumh[[i]] == cert) {
      return(p[[i]])
    } else if (cumh[[i]] < cert) {
      if (i == S) {
        return((p[[i]] + 1) / 2) # This is ((1 - 1/2S) + 1) / 2 = 1 - 1/4S.
      } else if (i == 1) {
        return((0 + p[[i]]) / 2) # This is (0 + 1/2S) / 2 = 1/4S.
      } else {
        return((p[[i]] + p[[i + 1]]) / 2)
        # return(p[[i]] + (1/S)*(cert - cumh[[i]])/(cumh[[i+1]] - cumh[[i]]))
      }
    }
  }

  stop("ew_maxcumh_p(): fall through; should not come here")
}

round_ew_prob <- function(p, S) {
  stopifnot(is.numeric(p))
  stopifnot(posint(S))
  signif <- floor(-log10(1 / (2 * S)))
  format(round(p, signif), nsmall = signif)
}

#
# Below support functions for SSO_estimate0().
#
support_funs_for_SSO_estimate0 <- function() {

}

# Create an ew_probability_graph based on a standard distribution.
ew_eval <- function(k,
                    n,
                    N = 1000,
                    # Only used for hypergeometric distribution.
                    S = 10000,
                    distri = "binom") {
  # Check input.
  stopifnot(nonnegint(k)) # Should be a whole non negative number.
  stopifnot(posint(n)) # Should be a whole positive number.
  stopifnot(posint(N)) # Should be a whole positive number.
  stopifnot(posint(S)) # Should be a whole positive number.
  stopifnot(k >= 0, n >= k)

  if (distri == "hyper") {
    stopifnot(N >= n)
    fun_to_use <- function(p) {
      r <- dhyper(k, round(p * N), round((1 - p) * N), n)

      # Replace any NaN by 0.
      r <- ifelse(is.nan(r), 0, r)
    }
  } else if (distri == "binom") {
    fun_to_use <- function(p) {
      dbinom(k, n, p)
    }
  } else if (distri == "norm") {
    fun_to_use <- function(p) {
      dnorm(k, n * p, sqrt(n * p * (1 - p)))
    }
  } else if (distri == "pois") {
    fun_to_use <- function(p) {
      dpois(k, p * n)
    }
  } else {
    print(distri)
    stop("ew_eval(): unknown distribution")
  }

  new_ew_probability_graph(chance_fun = fun_to_use, S)
}

# Create an ew_probability_graph based on a function chance_fun(x) on [0, 1].
new_ew_probability_graph <- function(chance_fun = unity, S = 10000) {
  # Check input.
  stopifnot(posint(S)) # S Should be a whole positive number.

  # Create initial histogram from f by dividing [0,1] into S segments.
  # The sum of the area in the histogram is not yet normalized to 1.
  p <- partition_0_1(S)
  h <- abs(chance_fun(p))

  # Normalize the chance mass of the histogram to 1.
  surface <- sum(h) / S
  h <- h / surface

  # Combine vectors p and h into one tibble.
  r <- tibble(p, h)

  # Designate r as of class ew_probability_graph.
  class(r) <- c("ew_probability_graph", class(r))

  # Check our output.
  stopifnot(validate_ew_probability_graph(r))

  r
}

unity <- function(x) {
  1
}

#
# Below support functions for SSO_estimate1().
#
support_funs_for_SSO_estimate1 <- function() {

}

# Check if ew is a valid ew_probability_graph.
validate_ew_probability_graph1 <- function(ew, verbose = TRUE) {
  # Ew should be a vector of non negative numbers.
  if (!is.numeric(ew)) {
    if (verbose) {
      message("ew should be a numeric vector")
    }
    return(FALSE)
  }
  if (any(ew < 0)) {
    if (verbose) {
      message("ew should be a vector of non negative numbers")
    }
    return(FALSE)
  }

  # Sum of ew should be 1.
  S <- length(ew)
  if (!near(sum(ew) / S, 1)) {
    if (verbose) {
      message("sum of ew should be 1")
      message(sprintf("found sum of ew =  %1.5f", sum(ew)))
    }
    return(FALSE)
  }

  # ew should contain at least 1 element.
  if (!(S > 0)) {
    if (verbose) {
      message("length of ew_probability_graph should be >=1")
    }
    return(FALSE)
  }

  return(TRUE)
}

# From ew graph h, return value of accompanying p with highest value for h.
ew_maxh_p1 <- function(h) {
  p <- partition_0_1(length(h))
  most_prob_h <- which.max(h)
  p[[most_prob_h]]
}

# From ew graph h, return value of accompanying p with minimum value for h,
# given certainty cert.
ew_mincumh_p1 <- function(h, cert) {
  p <- partition_0_1(length(h))

  at_min <- function(x) {
    x <= 1 - cert
  }

  # Create cumulative probability mass vector for h.
  cum_prob_mass <- cumsum(h) / length(h)

  if (cum_prob_mass[[1]] > 1 - cert) {
    # So for all i cum_prob_mass[[i]] > 1 - cert.
    i_min_p <- 1
  } else {
    # We know that there is an i for which cum_prob_mass[[i]] <= 1 - cert.
    i_min_p <-
      detect_index(cum_prob_mass, at_min, .dir = "backward")
  }
  min_p <- p[[i_min_p]]

  min_p
}

# From ew graph h, return value of accompanying p with maximum value for h,
# given certainty cert.
ew_maxcumh_p1 <- function(h, cert) {
  p <- partition_0_1(length(h))

  at_max <- function(x) {
    x >= cert
  }

  # Create cumulative probability mass vector for h.
  S <- length(h)
  cum_prob_mass <- cumsum(h) / S

  if (cum_prob_mass[[S]] < cert) {
    # So for all i cum_prob_mass[[i]] < cert.
    i_max_p <- S
  } else {
    # We know that there is an i for which cum_prob_mass[[i]] >= cert.
    i_max_p <- detect_index(cum_prob_mass, at_max)
  }
  max_p <- p[[i_max_p]]

  max_p
}

standardize_grid <- function(g) {
  rows <- rowSums(g)
  S <- length(rows)
  surface <- sum(rows)
  g <- g / surface
  g <- g / (S * S)
}
