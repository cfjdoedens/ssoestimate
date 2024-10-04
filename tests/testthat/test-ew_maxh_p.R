test_that("Case S == 1.", {
  v <- c(1)

  # Make ew graph from v.
  g <- ew_from_vec(v)

  # Test.
  maxh_p <- ew_maxh_p(g)

  # Check result.
  expect_equal(maxh_p, 0.5)
})

test_that("Case S == 2. Equal chances.", {
  v <- c(1, 1)

  # Make ew graph from v.
  g <- ew_from_vec(v)

  # Test.
  maxh_p <- ew_maxh_p(g)

  # Check result.
  expect_equal(maxh_p, 0.5)
})

test_that("Case S == 2. Unequal chances.
          Chance graph is the straight line y = x, i.e. h = p.",
          {
            v <- c(0.25, 0.75)

            # Make ew graph from v.
            g <- ew_from_vec(v)

            # Test.
            maxh_p <- ew_maxh_p(g)

            # Check result.
            expect_equal(maxh_p, 1)
          })

test_that("Case S == 2. Unequal chances.
          Chance graph is the straight line y = 1 - x, i.e. h = 1 - p.",
                      {
                        v <- c(0.75, 0.25)

                        # Make ew graph from v.
                        g <- ew_from_vec(v)

                        # Test.
                        maxh_p <- ew_maxh_p(g)

                        # Check result.
                        expect_equal(maxh_p, 0)
                      })
