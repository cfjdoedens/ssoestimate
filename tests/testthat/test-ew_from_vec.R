test_that("Case S == 1.", {
  v <- c(1)

  # Test.
  g <- ew_from_vec(v)

  # Check result.
  expect_equal(as.numeric(g), c(0.5, 1, 1, 1, 1, 1))
})

test_that("Case S == 2. Equal chances.", {
  v <- c(1, 1)

  # Test.
  g <- ew_from_vec(v)

  # Check result.
  expect_equal(as.numeric(g[1, ]), c(0.25, 1, 1, 1, 0.5, 0.5))
  expect_equal(as.numeric(g[2, ]), c(0.75, 1, 1, 1, 0.5, 1))
})

test_that("Case S == 2. Unequal chances.
          Chance graph is the straight line y = x, i.e. h = p.",
          {
            v <- c(0.25, 0.75)

            # Test.
            g <- ew_from_vec(v)

            # Check result.
            expect_equal(as.numeric(g[1, ]), c(0.25, 0.5, 0, 1, 0.25, 0.25))
            expect_equal(as.numeric(g[2, ]), c(0.75, 1.5, 1, 2, 0.75, 1))
          })

test_that("Case S == 2. Provoked bug: h_left < 0. Should be fine now.", {
  v <- c(0.25, 75)

  # Test.
  g <- ew_from_vec(v)

  # Check result.
  expect_equal(as.numeric(g[1, ]),
               c(0.25,  0.006644518, 0, 1, 0.112754159, 0.112754159))
  expect_equal(as.numeric(g[2, ]),
               c(0.75, 1.993355482, 1, 2.986711, 0.887245841, 1))
})

skip_on_cran()
test_that("Case S == 10,000. Just to see what happens when
          processing a somewhat big ew graph",
          {
            v <- 1:1e5

            # Test.
            g <- ew_from_vec(v)

            # No checking of result.
            expect_equal(TRUE, TRUE)
          })
