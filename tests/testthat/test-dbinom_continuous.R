test_that(
  "for integer values of k and n, dbinom and dbinom_continuous should produce the same results",
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
