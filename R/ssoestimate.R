#' Estimation of error fraction in SSO and client.
#'
#' A shared service organization, SSO, performs transactions for various clients.
#' Each transaction can potential be wrong, or partially wrong.
#' To get an estimate of the error rate of all the transactions we can take a
#' random sample of the transactions from the SSO.
#' This random sample is defined by the number of transactions, n_SSO, in the
#' sample, and the sum of partial and full errors found, k_SSO.
#' Given the number of transactions for a specific client, N_client,
#' we can then estimate the error rate for that specific client.
#' This function makes such an estimation, using the assumption that we can consider
#' the transactions of the client to be a random sample of all the transactions performed
#' by the SSO.
#' The function returns the probability graph of the error fraction in the SSO, and
#' the probability graph of the error fraction in the client transactions
#' For the estimation, the function uses a two dimensional grid of
#' postulated values for the error fractions of SSO, and client.
#' Both dimensions of the grid have the same granularity: S;
#' this makes it easier to compare the two resulting probability curves.
#' @param k_SSO The sum of partial and full errors found in the sample from the SSO.
#'              k_SSO is a non negative real number.
#'              Default 0.
#' @param n_SSO The number of transactions in the sample from the SSO.
#'              n_SSO is a positive integer.
#'              Note that we follow the convention to use a lower case n for the number
#'              of transactions in the sample (in this case from the SSO transactions).
#'              Default 350.
#' @param N_client The number of transactions of the client.
#'                 N_client is a positive integer.
#'                 Note that we follow the convention to use an upper case N for the number
#'                 of transactions in the mass of all transactions (in this case of the client).
#'                 Default 1000.
#' @param S The number of segments, represented by their midpoints,
#'          in which is divided each of the dimensions
#'          of the two dimensional grid used for computing the probability
#'          graphs of the error fraction of the transactions in the SSO,
#'          and the errors in the transactions of the client.
#'          So S is the granularity of the grid.
#'          S is a positive integer.
#'          Default 2000.
#'
#' @returns
#' A list of named elements containing:
#'
#'         - P_SSO, the probability graph of the error fraction in SSO
#'         - P_client, the probability graph of the error fraction in the client transactions
#'         - the input parameters
#' @export
#'
#' @examples
#' y <- SSO_estimate(k_SSO = 1, n_SSO = 3000, N_client = 500, S = 501)
#'
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

  # We divide the postulated sums of errors in the client transactions evenly
  # spaced into S segments, and we assign this to the vector K_client.
  # Each segment is represented by its midpoint.
  # The values in K_client are fixed, thus do not change.
  # They run (approximately) from 0 to the number of client transactions.
  K_client <- partition_0_1(S) * N_client

  # P_client is the vector containing the accompanying probabilities of
  # the elements of K_client,
  # We initialize all elements of P_client to 0.
  P_client <- rep.int(0, S)

  # Also we divide the postulated error rate of the SSO transactions evenly
  # spaced into S segments, and we assign this to the vector E_SSO.
  # Each segment is represented by its midpoint.
  # The values in E_SSO are fixed, thus do not change.
  # They run (approximately) from 0 to 1.
  E_SSO <- partition_0_1(S)
  for (e_i in E_SSO) {
    # e_i is the postulated value of e, the error fraction of
    # the entire mass m of transactions of the SSO, for 1 grid point.

    # This is the essential step in the algorithm: we link the binomial probabilities for both
    # samples using e_i.
    # Given e_i
    # we calculate the probability of
    # the grid points given by e_i and all values of K_client
    # as the product of:
    # - the binomial probability given k_SSO, and n_SSO
    # - the binomial probabilities given, all values of K_client, and N_client.
    # Result in P_client_i.
    P_client_i <-
      dbinom(k_SSO, n_SSO, e_i) * dbinom_continuous(K_client, N_client, e_i)

    # We aggregate the found P_client_i over
    # the grid points given by e_i and all values of K_client
    # with all previously found values for P_client_i.
    # Result in P_client.
    P_client <- P_client + P_client_i
  }

  # Standardize the probability curve so that the area is 1.
  surface <- sum(P_client) / length(P_client)
  P_client <- P_client / surface

  # Transform P_client into an ew_probability_graph.
  # Thereby we interpret P_client as a histogram of the error fraction in the client transactions,
  # making it comparable with the histogram of the error fraction in the SSO transactions.
  P_client <- vec_to_ew_probability_graph(P_client)

  # Also calculate the probability curve of the error fraction in SSO.
  P_SSO <- ew_eval(k = k_SSO,
                   n = n_SSO,
                   S = S,
                   distri = "binom")

  # SSO_p and client_p must be identical.
  SSO_p <- get_p_from_ew(P_SSO)
  client_p <- get_p_from_ew(P_client)
  stopifnot(isTRUE(all.equal(SSO_p, client_p)))

  # Return resulting probability curves and input parameters.
  list(
    "P_SSO" = P_SSO,
    # Resulting probability curve of the error fraction in SSO.
    "P_client" = P_client,
    # Resulting probability curve of the error fraction in the client transactions.
    "k_SSO" = k_SSO,
    # Input parameter.
    "n_SSO" = n_SSO,
    # Input parameter.
    "N_client" = N_client,
    # Input parameter.
    "S" = S # Input parameter.
  )
}

#' Show the results of a call to SSO_estimate().
#'
#' This function shows, when visual is TRUE, the results of a call to SSO_estimate() in a plot
#' combining the probability curves of the error fraction in the SSO transactions,
#' and of the error fraction in the client transactions.
#' The text in the plot shows the rounded of values of the min, most probable, and max values
#' for both the SSO and the client.
#' When visual is FALSE, a list of the min, most probable,
#' and max values for both the SSO and the client are returned.
#' In that case the raw values of min, most probable, and max are returned,
#' so no rounding is done.
#'
#' @param SSO_out The output of a call to SSO_estimate().
#' @param cert    The one sided certainty level for the min and max values of the error fraction.
#'                Default 0.95.
#' @param visual  If TRUE, a plot is returned.
#'                If FALSE, a list of specific values is returned.
#'                Default TRUE.
#' @return        If visual is TRUE, a ggplot is returned.
#'                If visual is FALSE, a named list is returned consisting of
#'                - the max, most probable, and min values of the error fraction in SSO;
#'                - and ditto for the client.
#' @export
#'
#' @examples
#' SSO_out <- SSO_estimate(k_SSO = 1, n_SSO = 3000, N_client = 500, S = 500)
#' SSO_graph_plot(SSO_out)
SSO_graph_plot <-
  function(SSO_out,
           cert = 0.95,
           visual = TRUE) {
    # Dissect SSO_out into its parts.
    SSO_graph <- SSO_out[["P_SSO"]]
    stopifnot(validate_ew_probability_graph(SSO_graph))
    client_graph <- SSO_out[["P_client"]]
    stopifnot(validate_ew_probability_graph(client_graph))
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

    # Get vectors h and p from SSO_graph.
    SSO_h <- get_h_from_ew(SSO_graph)
    SSO_p <- get_p_from_ew(SSO_graph)

    # Get vectors h and p from client_graph.
    client_h <- get_h_from_ew(client_graph)
    client_p <- get_p_from_ew(client_graph)

    # SSO_p and client_p should be the same vector. Let us call it p.
    stopifnot(isTRUE(all.equal(SSO_p, client_p)))
    p <- SSO_p

    # Get location, i.e. value of p, for all extreme values for SSO_graph, and client_graph.
    SSO_most_prob <- ew_most(SSO_graph)
    SSO_min <- ew_min(SSO_graph, cert)
    SSO_max <- ew_max(SSO_graph, cert)
    client_most_prob <- ew_most(client_graph)
    client_min <- ew_min(client_graph, cert)
    client_max <- ew_max(client_graph, cert)

    # Check these locations.
    {
      # Per client and per SSO, the min, most probable, and max values of the error fraction
      # should be in the right order.
      stopifnot(client_min <= client_most_prob)
      stopifnot(client_most_prob <= client_max)
      stopifnot(SSO_min <= SSO_most_prob)
      stopifnot(SSO_most_prob <= SSO_max)
    }
    {
      # The chance curve of the client is derived from the chance curve of the SSO,
      # but less sharp.
      stopifnot(client_min <= SSO_min)
      stopifnot(SSO_max <= client_max)
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
        "input k_SSO = %d; n_SSO = %d; N_client = %d; cert = %s; S = %d",
        k_SSO,
        n_SSO,
        N_client,
        toString(cert), # Remove trailing zeros from cert.
        S
      )
      line2 <- sprintf(
        "SSO    max %s most probable %s min %s",
        round_ew_prob(SSO_max, S),
        round_ew_prob(SSO_most_prob, S),
        round_ew_prob(SSO_min, S)
      )
      line3 <- sprintf(
        "client   max %s most probable %s min %s",
        round_ew_prob(client_max, S),
        round_ew_prob(client_most_prob, S),
        round_ew_prob(client_min, S)
      )
      subtitle <- sprintf("%s\n%s\n%s", line1, line2, line3)

      # Call ggplot() on prepared data, title and subtitle.
      ggplot(data = t) +
        geom_vline(
          mapping = NULL,
          data = NULL,
          xintercept = SSO_max,
          colour = "blue"
        ) +
        geom_vline(
          mapping = NULL,
          data = NULL,
          xintercept = client_max,
          colour = "red"
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
          values = c("SSO_h" = "blue", "client_h" = "red")
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
      result <- c(SSO_max,
                  SSO_most_prob,
                  SSO_min,
                  client_max,
                  client_most_prob,
                  client_min)
      names(result) <- c(
        "SSO_max",
        "SSO_most_prob",
        "SSO_min",
        "client_max",
        "client_most_prob",
        "client_min"
      )
      result
    }
  }
