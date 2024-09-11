# Continuous version of dbinom(k, n, p, log = FALSE).
#
# However the behaviour of dbinom_continuous is different from dbinom
# in the way that vector arguments of length > 1 are treated.
# Only one of k and n and p can have a length > 1.
# dbinom() allows for a mix of lengths; but I believe that
#
# @param k should be a vector of 1 or more non negative real numbers;
# so k can be a non integer number.
# @param n should be a vector of 1 or more positive real numbers;
# so n can be a non integer number.
# @param p should be a vector of 1 or more real numbers in \[0, 1\].
# @param log TRUE or FALSE.
#
# @returns
# A real number in \[0, 1\]
#
# @examples
# y <- dbinom_continuous(0, 300, 0.01)
#
# Code based on code from ChatGPT 4o, and checked using
# https://en.wikipedia.org/wiki/Beta_distribution, en
# https://en.wikipedia.org/wiki/Gamma_function.
dbinom_continuous <- function(k, n, p, log = FALSE) {
  # We allow that only one of k, n, p has a length > 1.
  if(length(k) > 1) {
    stopifnot(length(n) == 1 && length(p) == 1)
  }  else if(length(n) > 1) {
    stopifnot(length(k) == 1 && length(p) == 1)
  }  else if(length(p) > 1) {
    stopifnot(length(k) == 1 && length(n) == 1)
  }

  # When k is a vector, recur over each element of the vector.
  # if (length(k) > 1) {
  #   return(map_dbl(k, dbinom_continuous, n, p))
  # }
  if (length(k) > 1) {
    r <- double(length(k))
    for (i in 1:length(k)) {
      r[[i]] <- dbinom_continuous(k[[i]], n, p, log)
    }
    return(r)
  }

  # When n is a vector, recur over each element of the vector.
  if (length(n) > 1) {
    r <- double(length(n))
    for (i in 1:length(n)) {
      r[[i]] <- dbinom_continuous(k, n[[i]], p, log)
    }
    return(r)
  }

  # When p is a vector, recur over each element of the vector.
  if (length(p) > 1) {
    r <- double(length(p))
    for (i in 1:length(p)) {
      r[[i]] <- dbinom_continuous(k, n, p[[i]], log)
    }
    return(r)
  }

  # Check input values.
  stopifnot(length(k) == 1)
  stopifnot(length(n) == 1)
  stopifnot(length(p) == 1)

  stopifnot(0 <= k, k <= n)
  stopifnot(0 < n)
  stopifnot(0 <= p, p <= 1)

  # Use the Beta function to handle non-integer k.
  # This could probably be done more directly by using dbeta()*(n +1) or something like that,
  # but I did not invest much time into that direction.

  # We catch the situation where p is 0, or 1,
  # as below we want to be able to use log(p) and log(1-p),
  # and log(p) == -Inf, for p is 0, and log(1-p) == -Inf for p is 1.
  if (p == 0 && k == 0) {
    return(1)
  }
  if (p == 0 && k > 0) {
    return(0)
  }
  if (p == 1 && k == n) {
    return(1)
  }
  if (p == 1 && k < n) {
    return(0)
  }

  beta_part <- lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1)
  stopifnot(!is.nan(beta_part))

  prob_part <- k*log(p) + (n - k)*log(1-p)
  stopifnot(!is.nan(prob_part))

  r <- beta_part + prob_part
  if (! log) {
    r <- exp(r)
  }
  stopifnot(!is.nan(r))

  r
}

# # Function to compute the continuous binomial probability function using dbeta.
# # However, it is wrong, as it often does not return the same values as dbinom in case of integer n and k.
# dbinom_continuous1 <- function(k, n, p) {
#   # When k is a vector, recur over each element of the vector.
#   if (length(k) > 1) {
#     return(map_dbl(k, dbinom_continuous1, n, p))
#   }
#
#   # Check input values.
#   stopifnot(k >= 0, k <= n)
#   stopifnot(posint(n))
#   stopifnot(p >= 0, p <= 1)
#
#   # Use dbeta().
#   dbeta(p, k + 1, n - k + 1) * (n + 1)
# }

# # Example usage
# k <- 2.5
# n <- 5.5
# p <- 0.4
# result <- dbinom_continuous(k, n, p)
# print(result)
