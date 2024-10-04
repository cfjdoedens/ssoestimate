test_that("Case S == 1.", {
  v <- c(1)

  # Emulate first part of ew_from_vec(v).
  S <- length(v)
  p <- ew_partition_0_1(S)
  h <- v
  g <- tibble(p, h)
  g <- ew_add_h_leftright(g)

  # Test.
  g <- ew_add_surface(g)

  # Check result.
  expect_equal(as.numeric(g), c(0.5, 1, 1, 1, 1, 1))
})

test_that("Case S == 2. Equal chances.", {
  v <- c(1, 1)

  # Emulate first part of ew_from_vec(v).
  S <- length(v)
  p <- ew_partition_0_1(S)
  h <- v
  g <- tibble(p, h)
  g <- ew_add_h_leftright(g)

  # Test.
  g <- ew_add_surface(g)

  # Check result.
  expect_equal(as.numeric(g[1, ]), c(0.25, 1, 1, 1, 0.5, 0.5))
  expect_equal(as.numeric(g[2, ]), c(0.75, 1, 1, 1, 0.5, 1))
})

test_that("Case S == 2. Unequal chances.
          Chance graph is the straight line y = x, i.e. h = p.",
          {
            v <- c(0.25, 0.75)

            # Emulate first part of ew_from_vec(v).
            S <- length(v)
            p <- ew_partition_0_1(S)
            h <- v
            g <- tibble(p, h)
            g <- ew_add_h_leftright(g)

            # Test.
            g <- ew_add_surface(g)

            # Check result.
            expect_equal(as.numeric(g[1, ]), c(0.25, 0.25, 0, 0.5, 0.25, 0.25))
            expect_equal(as.numeric(g[2, ]), c(0.75, 0.75, 0.5, 1, 0.75, 1))
          })

test_that("Case S == 2. Provoked bug: h_left < 0. Should be fine now.", {
  v <- c(0.25, 75)

  # Emulate first part of ew_from_vec(v).
  S <- length(v)
  p <- ew_partition_0_1(S)
  h <- v
  g <- tibble(p, h)
  g <- ew_add_h_leftright(g)

  # Test.
  g <- ew_add_surface(g)

  # Check result.
  expect_equal(as.numeric(g[1, ]), c(0.25, 0.25, 0, 37.625, 0.112754159, 0.112754159))
  expect_equal(as.numeric(g[2, ]), c(0.75, 75, 37.625, 112.375, 0.887245841, 1))
})

test_that("Case S == 10,000. Just to see what happens when
          processing a somewhat big ew graph",
          {
            skip_on_cran()
            v <- 1:1e5

            # Emulate first part of ew_from_vec(v).
            S <- length(v)
            p <- ew_partition_0_1(S)
            h <- v
            g <- tibble(p, h)
            g <- ew_add_h_leftright(g)

            # Test.
            g <- ew_add_surface(g)

            # No checking of result.
            expect_equal(TRUE, TRUE)
          })
