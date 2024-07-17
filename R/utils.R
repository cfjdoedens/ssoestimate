# dbinom_continuous() is een versie van dbinom() waarbij k een nietgeheel,
# maar wel nietnegatief getal mag zijn.
# Code gebaseerd op code verkregen via ChatGPT 4o, en gecontroleerd op basis van
# https://en.wikipedia.org/wiki/Beta_distribution, en
# https://en.wikipedia.org/wiki/Gamma_function.
dbinom_continuous <- function(k, n, p) {
  if (length(k) > 1) {
     return(map_dbl(k, dbinom_continuous, n, p))
  }

  # Check for valid input values.
  stopifnot(k >=0, k <= n)
  stopifnot(posint(n))
  stopifnot(p >= 0, p <= 1)

  # Use the Beta function to handle non-integer k.
  # This could probably be done more directly by using dbeta()/(n +1) or something like that,
  # but did not invest time into that direction
  beta_part <- exp(lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1))
  prob_part <- p^k * (1 - p)^(n - k)
  beta_part * prob_part
}

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
#' @examples
#'   partition_0_1(S = 5)
#'   #  Returns the vector c(0.1, 0.3, 0.5, 0.7, 0.9).
#' @export
partition_0_1 <- function(S = 1000) {
  seq(0 + 1 / (2 * S), 1 - 1 / (2 * S), 1 / S)
}

# Maak een ew_probability_graph van een vector,
# waarbij we er vanuit gaan dat de elementen van de vector
# de kanswaarden van 1/S - 1/2S, ..., S/S - 1/2S representeren.
# Waarbij S de lengte van de vector is.
vec_to_ew_probability_graph <-
  function(v) {
    S <- length(v)

    # Create initial histogram from f by dividing [0,1] into S segments.
    # The sum of the area in the histogram is not yet normalized to 1.
    p <- partition_0_1(S)
    h <- v

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

get_p_from_ew <- function(ew) {
  ew %>% pull("p")
}

get_h_from_ew <- function(ew) {
  ew %>% pull("h")
}

validate_ew_probability_graph <- function(ew, verbose = TRUE) {
  # When we have to do with a list of (supposed) ew_probability_graphs,
  # we call validate_ew_probability_graph() recursively.
  if (class(ew)[[1]] ==  "list") {
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
  p <- get_p_from_ew(ew)
  h <- get_h_from_ew(ew)

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

unity <- function(x) {
  1
}

# Create an ew_probability_graph based on a function chance_fun(x) on [0, 1].
new_ew_probability_graph <-
  function(chance_fun = unity, S = 10000) {
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

# Create an ew_probability_graph based on a standard distribution.
ew_eval <- function(k,
                    n,
                    N = 1000, # Only used for hypergeometric distribution.
                    S = 10000,
                    distri = "binom") {
  # Check input.
  stopifnot(nonnegint(k)) # Should be a whole non negative number.
  stopifnot(posint(n))    # Should be a whole positive number.
  stopifnot(posint(N))    # Should be a whole positive number.
  stopifnot(posint(S))    # Should be a whole positive number.
  stopifnot(k >= 0, n >= k)

  if (distri == "hyper")  {
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
  } else if (distri == "pois")  {
    fun_to_use <- function(p) {
      dpois(k, p * n)
    }
  } else {
    print(distri)
    stop("ew_eval(): unknown distribution")
  }

  new_ew_probability_graph(chance_fun = fun_to_use, S)
}

# From ew graph g, return value of p with highest value for h.
ew_most <-
  function(g) {
    h <- get_h_from_ew(g)
    p <- get_p_from_ew(g)
    most_prob_h <- which.max(h)
    p[[most_prob_h]]
  }

# From ew graph g, return value of p with minimum value for h,
# given certainty cert.
ew_min <-
  function(g, cert) {
    h <- get_h_from_ew(g)
    p <- get_p_from_ew(g)

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

# From ew graph g, return value of p with maximum value for h,
# given certainty cert.
ew_max <-
  function(g, cert) {
    h <- get_h_from_ew(g)
    p <- get_p_from_ew(g)

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
