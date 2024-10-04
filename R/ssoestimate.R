#' Estimation of error fraction in SSO and client
#'
#' A shared service organization, SSO, performs transactions for various
#' clients.
#' Each transaction can potential be wrong, or partially wrong.
#' To get an estimate of the error rate of all the transactions we can take a
#' random sample of the transactions from the SSO.
#' This random sample is defined by the number of transactions, n_SSO, in the
#' sample, and the sum of partial and full errors found, k_SSO.
#' Given the number of transactions for a specific client, N_client,
#' we can then estimate the error rate for that specific client.
#' This function makes such an estimation, using the assumption that we can
#' consider the transactions of the client to be a random sample of all the
#' transactions performed by the SSO.
#' The function returns the probability graph of the error fraction in the
#' client transactions, and the probability graph of the error fraction in
#' the SSO.
#' For the estimation, the function uses a two dimensional grid of
#' postulated values for the error fractions of client and SSO.
#' Both dimensions of the grid have the same granularity: S;
#' this makes it easier to compare the two resulting probability curves.
#' @param k_SSO The sum of partial and full errors found in the sample from
#'              the SSO.
#'              k_SSO is a non negative real number.
#'              Default 0.
#' @param n_SSO The number of transactions in the sample from the SSO.
#'              n_SSO is a positive integer.
#'              Note that we follow the convention to use a lower case n for
#'              the number of transactions in the sample
#'              (in this case from the SSO transactions).
#'              Default 350.
#' @param N_client The number of transactions of the client.
#'                 N_client is a positive integer.
#'                 Note that we follow the convention to use an upper case N
#'                 for the number of transactions in the mass of all
#'                 transactions
#'                 (in this case of the client).
#'                 Default 1000.
#' @param S The number of segments, represented by their midpoints,
#'          in which is divided each of the dimensions
#'          of the two dimensional grid used for computing the probability
#'          graphs of the error fraction of the transactions in the client,
#'          and the errors in the transactions of the SSO
#'          So S is the granularity of the grid.
#'          S is a positive integer.
#'          Default 2000.
#'
#' @returns
#' A list of named elements. This list can be used as input
#' to SSO_graph_plot(), which transforms it into
#' a form that is more easily interpreted by humans. The list consists of:
#'
#'         - P_client, a numeric representation of the
#'           probability graph of the error fraction in
#'           the client transactions
#'         - P_SSO, a numeric representation of the
#'           probability graph of the error fraction in the SSO transactions
#'         - the input parameters
#'
#'
#' @export
#'
#' @examples
#' x <- SSO_estimate(k_SSO = 1, n_SSO = 3000, N_client = 500, S = 501)
SSO_estimate <- function(k_SSO = 0,
                         n_SSO = 350,
                         N_client = 1000,
                         S = 2000) {
  # Check input parameters.
  stopifnot(k_SSO >= 0) # Need not be a whole number.
  stopifnot(posint(n_SSO))
  stopifnot(n_SSO >= k_SSO)
  stopifnot(posint(N_client))
  stopifnot(posint(S))

  # Create and fill the grid.
  grid <- matrix(NA, nrow = S, ncol = S)
  dim_K <- ew_partition_0_1(S) * N_client
  dim_P <- ew_partition_0_1(S)
  for (i in 1:S) {
    k_i <- dim_K[[i]]
    for (j in 1:S) {
      p_j <- dim_P[[j]]
      grid[[i, j]] <-
        dbinom_continuous(k_i, N_client, p_j) *
        dbinom_continuous(k_SSO, n_SSO, p_j)
    }
  }

  # From the grid, construct P_client,
  # the probability curve for the error fraction in the client transactions.
  {
    # Aggregate over the rows of the grid.
    # This gives the probability curve for the error fraction in
    # the client transactions.
    P_client <- rowSums(grid)

    # Transform P_client into an ew_probability_graph.
    P_client <- ew_from_vec(P_client)
  }

  # For reference of P_client, construct P_SSO,
  # the probability curve for the error fraction in the SSO transactions.
  # We are tempted to do this by aggregating over the columns of the grid.
  # Like so:
  {
    # Aggregate over the columns of the grid.
    # This gives the probability curve for the error fraction in
    # the SSO transactions.
    # P_SSO <- colSums(grid)

    # Transform P_SSO into an ew_probability_graph.
    # P_SSO <- ew_from_vec(P_SSO)
  }
  # However, that does not work very good.
  # This, because slight variations in dbinom_continuous(k_j, N_client, p_i)
  # introduce noise in the computation.
  # So we compute P_SSO directly.
  P_SSO <- dbinom_continuous(k_SSO, n_SSO, dim_P)
  P_SSO <- ew_from_vec(P_SSO)

  # client_p and SSO_p should have the same number of elements.
  stopifnot(ew_S(P_client) == ew_S(P_SSO))

  # Return.
  list(
    # Resulting probability curves of the error fraction in client and
    # SSO transactions.
    "P_client" = P_client,
    "P_SSO" = P_SSO,

    # Input parameters.
    "k_SSO" = k_SSO,
    "n_SSO" = n_SSO,
    "N_client" = N_client,
    "S" = S
  )
}

#' Show the results of a call to SSO_estimate()
#'
#' This function shows, when visual is TRUE, the results of a call to
#' SSO_estimate() in a plot.
#' Shown is the probability curve of the error
#' fraction in the client transactions.
#' For reference, also the probability curve of the error fraction in the SSO
#' transactions is shown.
#' The text in the plot shows the rounded of values of the min, most probable,
#' and max values for both the SSO and the client.
#' When visual is FALSE, a list of the min, most probable,
#' and max values for both the SSO and the client are returned.
#' In that case the raw values of min, most probable, and max are returned,
#' so no rounding is done.
#'
#' @param SSO_out The output of a call to SSO_estimate().
#' @param cert    The one sided certainty level for the min and max values of
#'                the error fraction.
#'                Default 0.95.
#' @param visual  If TRUE, a plot is returned.
#'                If FALSE, a list of specific values is returned.
#'                Default TRUE.
#' @return        If visual is TRUE, a ggplot is returned.
#'                If visual is FALSE, a named list is returned consisting of
#'                - the max, most probable, and min values of
#'                  the error fraction in the client;
#'                - and ditto for the SSO
#' @export
#'
#' @examples
#' SSO_out <- SSO_estimate(k_SSO = 1, n_SSO = 3000, N_client = 500, S = 500)
#' SSO_graph_plot(SSO_out)
SSO_graph_plot <- function(SSO_out,
                           cert = 0.95,
                           visual = TRUE) {
  # Argument check.
  stopifnot(is.list(SSO_out)) # Further checked below.
  stopifnot(0 < cert, cert < 1)
  stopifnot(is.logical(visual))

  # Dissect SSO_out into its parts, and check them.
  client_graph <- SSO_out[["P_client"]]
  stopifnot(ew_validate(client_graph))
  SSO_graph <- SSO_out[["P_SSO"]]
  stopifnot(ew_validate(SSO_graph))
  k_SSO <- SSO_out[["k_SSO"]]
  stopifnot(is.numeric(k_SSO))
  stopifnot(k_SSO >= 0)
  n_SSO <- SSO_out[["n_SSO"]]
  stopifnot(is.numeric(n_SSO))
  stopifnot(n_SSO >= 1)
  stopifnot(n_SSO >= k_SSO)
  N_client <- SSO_out[["N_client"]]
  stopifnot(is.numeric(N_client))
  stopifnot(N_client >= 1)
  S <- SSO_out[["S"]]
  stopifnot(is.numeric(S))
  stopifnot(S >= 1)

  # Get vectors h and p from client_graph.
  client_h <- ew_get_h(client_graph)
  client_p <- ew_get_p(client_graph)

  # Get vectors h and p from SSO_graph.
  SSO_h <- ew_get_h(SSO_graph)
  SSO_p <- ew_get_p(SSO_graph)

  # client_p and SSO_p should be the same vector. Let us call it p.
  stopifnot(isTRUE(all.equal(client_p, SSO_p)))
  p <- client_p

  # Get location, i.e. value of p,
  # for all extreme values for SSO_graph, and client_graph.
  SSO_most_prob <- ew_maxh_p(SSO_graph)
  SSO_min <- ew_mincumh_p(SSO_graph, cert)
  SSO_max <- ew_maxcumh_p(SSO_graph, cert)
  client_most_prob <- ew_maxh_p(client_graph)
  client_min <- ew_mincumh_p(client_graph, cert)
  client_max <- ew_maxcumh_p(client_graph, cert)

  # Check these locations.
  {
    # Per client and per SSO, the min, most probable,
    # and max values of the error fraction should be in the right order.
    #
    # Consider the situation that cert > 0.5.
    # Note that in this situation it is possible that:
    #  - client_min > client_most_prob (when k_SSO = 0, or nearly 0)
    #  - client_most_prob > client_max (when k_SSO = n_SSO, or nearly n_SSO)
    #  - SSO_min > SSO_most_prob (when k_SSO = 0, or nearly 0)
    #  - SSO_most_prob > SSO_max (when k_SSO = n_SSO, or nearly n_SSO)
    # So we should not check on the negation of these conditions.
    #
    # But the following should always be true:
    if (cert > .5) {
      stopifnot(client_min <= client_max)
      stopifnot(SSO_min <= SSO_max)
    } else if (cert < .5) {
      # By symmetry.
      stopifnot(client_min >= client_max)
      stopifnot(SSO_min >= SSO_max)
    }
  }
  {
    # The chance curve of the client is derived from the chance curve of the
    # SSO, but less sharp. However, the following asserts might not be true when
    # the chance graph is totally skewed to left or right. Therefore we assert
    # only when the chance graph is not totally skewed.
    if (ew_middle_peaked(SSO_graph)) {
      stopifnot(client_min <= SSO_min)
      stopifnot(SSO_max <= client_max)
    }
  }

  if (visual) {
    # Make a plot!
    # We start with preparing the data for the plot.
    {
      # Combine graphs of SSO and client into one tibble.
      t <- tibble(p, SSO_h, client_h)

      # Filter away uninteresting, flat part of curves.
      t <- t %>% filter(SSO_h >= 0.001 | client_h >= 0.001)

      #  Make long version of t for plotting.
      t <- gather(t, "who", "prob", SSO_h, client_h)
    }

    # Construct title.
    title <- sprintf("error fractions")

    # Construct subtitle.
    line1 <- sprintf(
      "input k_SSO = %6.f; n_SSO = %d; N_client = %d; cert = %s; S = %d",
      k_SSO,
      n_SSO,
      N_client,
      toString(cert),
      # Remove trailing zeros from cert.
      S
    )
    line2 <- sprintf(
      "client   max %s most probable %s min %s",
      ew_round_prob(client_max, S),
      ew_round_prob(client_most_prob, S),
      ew_round_prob(client_min, S)
    )
    line3 <- sprintf(
      "SSO    max %s most probable %s min %s",
      ew_round_prob(SSO_max, S),
      ew_round_prob(SSO_most_prob, S),
      ew_round_prob(SSO_min, S)
    )
    subtitle <- sprintf("%s\n%s\n%s", line1, line2, line3)

    # Call ggplot() on prepared data, title and subtitle.
    ggplot(data = t) +
      geom_vline(
        mapping = NULL,
        data = NULL,
        xintercept = client_max,
        colour = "red"
      ) +
      geom_vline(
        mapping = NULL,
        data = NULL,
        xintercept = SSO_max,
        colour = "blue"
      ) +
      # We can not use here:
      #   geom_point(mapping = aes(x = p, y = prob, color = who)) +
      # because this will invoke an error message from devtools::check() like:
      #   no visible binding for global variable ‘prob’
      # So we use .data$prob instead of prob.
      # The same for ‘who’.
      geom_point(mapping = aes(
        x = p,
        y = .data$prob,
        color = .data$who
      )) +
      scale_color_manual(
        labels = c("client", "SSO"),
        values = c("client_h" = "red", "SSO_h" = "blue")
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        # caption = " ",
        x = "postulated error fraction",
        y = "probability",
        color = "who"
      )
  } else {
    # Return non-visual information.
    result <- c(client_max,
                client_most_prob,
                client_min,
                SSO_max,
                SSO_most_prob,
                SSO_min)
    names(result) <- c(
      "client_max",
      "client_most_prob",
      "client_min",
      "SSO_max",
      "SSO_most_prob",
      "SSO_min"
    )
    result
  }
}
