test_that(
  "for integer values of k and n, dbinom and dbinom_continuous
   should produce the same results",
  {
    skip_on_cran()
    for (n in c(1, 300)) {
      for (k in 0:n) {
        for (p in seq(0.00, 1.00, by = 0.01)) {
          expect_equal(dbinom(k, n, p), dbinom_continuous(k, n, p))
        }
      }
    }
  }
)

test_that(
  "dbinom_continuous should be able to handle length(k) > 1",
  {
    k <- 0:3
    n <- 4
    p <- 0.5
    expect_equal(length(k), length(dbinom_continuous(k, n, p)))
  }
)

test_that(
  "dbinom_continuous should be able to handle length(n) > 1",
  {
    k <- 0
    n <- 4:7
    p <- 0.5
    expect_equal(length(n), length(dbinom_continuous(k, n, p)))
  }
)

test_that(
  "dbinom_continuous should be able to handle length(p) > 1",
  {
    k <- 0
    n <- 4
    p <- c(0.1, 0.2, 0.3, 0.4, 0.5)
    expect_equal(length(p), length(dbinom_continuous(k, n, p)))
  }
)

test_that(
  "dbinom_continuous should abort when more than one of k,n,p has length > 1",
  {
    k1 <- 0
    km <- 0:3
    n1 <- 4
    nm <- 4:7
    p1 <- 0.5
    pm <- c(0.1, 0.2, 0.3, 0.4, 0.5)
    expect_error(dbinom_continuous(km, nm, p1))
    expect_error(dbinom_continuous(km, n1, pm))
    expect_error(dbinom_continuous(k1, nm, pm))
    expect_error(dbinom_continuous(km, nm, pm))
  }
)
