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

# Get the number of elements, i.e. number of rows, of an equal width graph.
ew_S <- function(g) {
  length(ew_get_h(g))
}

#' Partition \[0,1\] into S equally sized consecutive segments. Each segment
#' is given by its midpoint.
#'
#' @param S An integer >= 1. The number of segments to partition \[0,1\] into.
#' @returns The vector of segment midpoints,
#'          c(\code{(1-0.5)/S}, \code{(2-0.5)/S}, ... (\code{(S-0.5)/S}).
#  @examples
#  ew_partition_0_1(S = 5)
#    Returns the vector c(0.1, 0.3, 0.5, 0.7, 0.9).
ew_partition_0_1 <- function(S = 1000) {
  seq(0 + 1 / (2 * S), 1 - 1 / (2 * S), 1 / S)
}

ew_get_p <- function(g) {
  g %>% pull("p")
}

ew_get_h <- function(g) {
  g %>% pull("h")
}

ew_get_h_left <- function(g) {
  g %>% pull("h_left")
}

ew_get_h_right <- function(g) {
  g %>% pull("h_right")
}

ew_get_surface <- function(g) {
  g %>% pull("surface")
}

ew_get_cumsurface <- function(g) {
  g %>% pull("cumsurface")
}

ew_validate <- function(g, verbose = TRUE) {
  # When we have to do with a list of (supposed) ew_probability_graphs,
  # we call ew_validate() recursively.
  if (class(g)[[1]] == "list") {
    r <- sapply(g, ew_validate)
    return(min(r) > 0) # Only TRUE if all values in r are TRUE
  }

  # Single (supposed) ew_probability_graph to validate.
  # g should be a tibble.
  if (!is_tibble(g)) {
    if (verbose) {
      message("ew_probability_graph should be a tibble")
    }
    return(FALSE)
  }

  # g should have columns p, h, h_left, h_right, surface, cumsurface.
  if (!("p" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector p")
    }
    return(FALSE)
  }
  if (!("h" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector h")
    }
    return(FALSE)
  }
  if (!("h_left" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector h_left")
    }
    return(FALSE)
  }
  if (!("h_right" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector h_right")
    }
    return(FALSE)
  }
  if (!("surface" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector surface")
    }
    return(FALSE)
  }
  if (!("cumsurface" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector cumsurface")
    }
    return(FALSE)
  }

  # g should contain at least 1 element.
  S <- ew_S(g)
  if (!(S > 0)) {
    if (verbose) {
      message("ew_probability_graph should contain at least one element")
    }
    return(FALSE)
  }

  # p should be equal to partition(S), where S is the number of elements of g.
  p <- ew_get_p(g)
  p2 <- ew_partition_0_1(S)
  if (!isTRUE(all.equal(p, p2))) {
    if (verbose) {
      message("vector p of ew_probability_graph should be equal to
               ew_partition_0_1(ew_S(g))")
    }
    return(FALSE)
  }

  # The total area of the combined penta trapeziums should be 1.
  cumsurface <- g %>% pull("cumsurface")
  if (!near(cumsurface[[S]], 1)) {
    if (verbose) {
      message("total chance mass of ew_probability_graph should be 1")
      message(sprintf("found chance mass =  %1.5f", cumsurface[[S]]))
      message(sprintf("number of elements in the chance graph = %i", S))
    }
    return(FALSE)
  }

  return(TRUE)
}

ew_round_prob <- function(p, S) {
  stopifnot(is.numeric(p))
  stopifnot(posint(S))
  signif <- floor(-log10(1 / (2 * S)))
  format(round(p, signif), nsmall = signif)
}
