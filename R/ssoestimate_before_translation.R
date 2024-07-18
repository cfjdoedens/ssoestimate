# Before working with R and Rstudio run ~bin/install_r.
# Also run the R code mentioned as comment in it inside R.
#
# The code in this package depends on the following packages:
# install.packages("tibble")
# install.packages("dplyr")
# install.packages("testthat")
# install.packages("assertthat")
# install.packages("purrr")
# install.packages("tidyr")
#

# library(tibble)
# library(dplyr)
# library(testthat)
# library(assertthat)
# library(purrr)
# library(tidyr)
# library(ggplot2)

#' Estimation of error fraction in SSO and client.
#'
#' @param k_SSO The number of errors found in Steek_SSO. k_SSO >= 0.
#' @param n_SSO The number of posten in Steek_SSO. n_SSO > 0.
#' @param n_client The number of posten in Steek_client. n_client > 0.
#' @param S_p_SSO The number of segments in the rooster for p. S_p_SSO > 0.
#' @param S_k_client The number of segments in the rooster for k_client. S_k_client > 0.
#'
#' @returns
#' A list of named elements containing:
#'
#'         - P_SSO, the probability graph of the error fraction in SSO
#'         - P_client, the probability graph of the error fraction in Steek_client
#'         - the input parameters
#' @export
#'
#' @examples
#' y <- SSO_estimate(
#'   k_SSO = 1, n_SSO = 3000, n_client = 500,
#'   S_p_SSO = 1000, S_k_client = 501
#' )
#'
SSO_estimate <- function(k_SSO,
                                   n_SSO,
                                   n_client,
                                   S_p_SSO = 1000,
                                   S_k_client = 1000) {

  # Check input parameters.
  stopifnot(k_SSO >= 0) # Need not be a whole number.
  stopifnot(posint(n_SSO))
  stopifnot(n_SSO >= k_SSO)
  stopifnot(posint(n_client))
  stopifnot(posint(S_p_SSO))
  stopifnot(posint(S_k_client))

  # K_client bevat het aantal mogelijke fouten in Steek_client.
  # De waarden in K_client zijn vast, veranderen dus niet.
  # Ze lopen (ongeveer) van 0 tot S_k_client.
  K_client <- partition_0_1(S_k_client) * n_client
  # K_client bevat de elementen van 0 tot n_client, met een granulariteit van 1/S_k_client.

  # P_client is de kanskromme die we proberen uit te rekenen met
  # SSO_estimate().
  # P_client moet voor elk element van K_client de kans op dat
  # aantal fouten gaan bevatten.
  # We initialiseren alle elementen van P_client op 0.
  P_client <- rep.int(0, S_k_client)

  # S_p_SSO is de granulariteit van het rooster dat we gebruiken.
  # S_p_SSO geeft het aantal segmenten, gerepresenteerd door hun middens, aan waarin we p,
  # de gepostuleerde foutfractie van m, de massa van alle posten van
  # de SSO verdelen.
  rooster <- partition_0_1(S_p_SSO)
  for (p_i in rooster) {
    # p_i is de gepostuleerde waarde van p, de foutfractie van
    # de gehele massa m, voor 1 roosterpunt.

    # Dit is de essentiele stap in het algoritme: we koppelen de binomiale kansen voor beide
    # steekproeven aan elkaar middels p_i.
    #
    # Gegeven p_i
    # berekenen we voor alle mogelijke waarden van k_client de waarschijnlijkheid van dit
    # roosterpunt als het product van:
    # - de binomiale kans gegeven k_SSO, en n_SSO
    # - de binomiale kans gegeven k_client, en n_client.
    # Resultaat in P_client_i.
    P_client_i <-
      dbinom(k_SSO, n_SSO, p_i) * dbinom_continuous(K_client, n_client, p_i)

    # We aggregeren de gevonden P_client_i over dit roosterpunt, p_i,
    # met alle eerder gevonden waarden voor P_client_i.
    # Resultaat in P_client.
    P_client <- P_client + P_client_i
  }

  # Standaardiseer de kanskromme zodat het oppervlak 1 is.
  surface <- sum(P_client) / length(P_client)
  P_client <- P_client / surface

  # Vorm om tot ew_probability_graph.
  P_client <- vec_to_ew_probability_graph(P_client)

  # Bereken ook kanskromme van foutfractie in SSO.
  P_SSO <- ew_eval(k = k_SSO,
                   n = n_SSO,

                   # De granulariteit moet gelijk zijn aan die voor P_client.
                   S = S_k_client,
                   distri = "binom")

  # SSO_p en client_p moeten identiek zijn.
  SSO_p <- get_p_from_ew(P_SSO)
  client_p <- get_p_from_ew(P_client)
  stopifnot(isTRUE(all.equal(SSO_p, client_p)))

  # Geef resulterende kanskrommes en invoerparameters terug.
  list(
    "P_SSO" = P_SSO,
    "P_client" = P_client,
    "k_SSO" = k_SSO,
    "n_SSO" = n_SSO,
    "n_client" = n_client,
    "S_p_SSO" = S_p_SSO,
    "S_k_client" = S_k_client
  )
}

#' Show the results of a call to SSO_estimate() or SSO_estimate_continous() in a plot.
#'
#' @param SSO_out The output of a call to SSO_estimate() or SSO_estimate_continous().
#' @param cert    The certainty level for the min and max values of the error fraction.
#' @param visual  If TRUE, a plot is returned.
#'                If FALSE, a list of specific values is returned.
#'
#' @return        If visual is TRUE, a ggplot is returned.
#'                If visual is FALSE, a named list is returned consisting of
#'                - the most probable, min, and max values of the error fraction in SSO;
#'                - and ditto for the client.
#' @export
#'
#' @examples
#' SSO_out <- SSO_estimate(k_SSO = 1, n_SSO = 3000, n_client = 500, S_p_SSO = 1000)
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
    n_client <- SSO_out[["n_client"]]
    stopifnot(is.numeric(n_client))
    stopifnot(n_client >= 1)
    S_p_SSO <- SSO_out[["S_p_SSO"]]
    stopifnot(is.numeric(S_p_SSO))
    stopifnot(S_p_SSO >= 1)
    S_k_client <- SSO_out[["S_k_client"]] # If missing, we will see that later on.

    # Get vectors h and p from SSO_graph.
    SSO_h <- get_h_from_ew(SSO_graph)
    SSO_p <- get_p_from_ew(SSO_graph)

    # Get vectors h and p from client_graph.
    client_h <- get_h_from_ew(client_graph)
    client_p <- get_p_from_ew(client_graph)

    # SSO_p and client_p should be the same vector. Let us call it p.
    stopifnot(isTRUE(all.equal(SSO_p, client_p)))
    p <- SSO_p

    # Get location, i.e. value of p, for each extreme value for SSO_graph, and client_graph.
    SSO_most_prob <- ew_most(SSO_graph)
    SSO_min <- ew_min(SSO_graph, cert)
    SSO_max <- ew_max(SSO_graph, cert)
    client_most_prob <- ew_most(client_graph)
    client_min <- ew_min(client_graph, cert)
    client_max <- ew_max(client_graph, cert)

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
        "SSO    most probable %1.5f min %1.5f (at cert %1.2f) max %1.5f (at cert %1.2f)",
        SSO_most_prob,
        SSO_min,
        cert,
        SSO_max,
        cert
      )
      line2 <- sprintf(
        "client  most probable %1.5f min %1.5f (at cert %1.2f) max %1.5f (at cert %1.2f)",
        client_most_prob,
        client_min,
        cert,
        client_max,
        cert
      )
      if (is.na(S_k_client)) {
        # S_k_client is missing.
        line3 <- sprintf(
          "(input k_SSO = %d; n_SSO = %d; n_client = %d; S_p_SSO = %d)",
          k_SSO,
          n_SSO,
          n_client,
          S_p_SSO
        )
      } else {
        line3 <- sprintf(
          "(input k_SSO = %d; n_SSO = %d; n_client = %d; S_p_SSO = %d; S_k_client = %d)",
          k_SSO,
          n_SSO,
          n_client,
          S_p_SSO,
          S_k_client
        )
      }
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
      result <- c(SSO_most_prob,
                  SSO_min,
                  SSO_max,
                  client_most_prob,
                  client_min,
                  client_max)
      names(result) <- c(
        "SSO_most_prob",
        "SSO_min",
        "SSO_max",
        "client_most_prob",
        "client_min",
        "client_max"
      )
      # class(result) <- c("prob_graph_info", class(result))
      return(result)
    }
  }
