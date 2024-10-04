test_that("Case S == 1. cert == 0.95.", {
  # Create equal width probability graph g to be used for testing.
  v <- c(1)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.95)
  mincumh_p <- ew_mincumh_p(g, 0.95)

  # Check result.
  expect_equal(maxcumh_p, 0.95)
  expect_equal(mincumh_p, 0.05)
})

test_that("Case S == 1. cert == 0.05.", {
  # Create equal width probability graph g to be used for testing.
  v <- c(1)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.05)
  mincumh_p <- ew_mincumh_p(g, 0.05)

  # Check result.
  expect_equal(maxcumh_p, 0.05)
  expect_equal(mincumh_p, 0.95)
})

test_that("Case S == 2. Equal chances. cert = 0.95", {
  # Create equal width probability graph g to be used for testing.
  v <- c(1, 1)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.95)
  mincumh_p <- ew_mincumh_p(g, 0.95)

  # Check result.
  expect_equal(maxcumh_p, 0.95)
  expect_equal(mincumh_p, 0.05)
})

test_that("Case S == 2. Equal chances. cert = 0.05", {
  # Create equal width probability graph g to be used for testing.
  v <- c(1, 1)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.05)
  mincumh_p <- ew_mincumh_p(g, 0.05)

  # Check result.
  expect_equal(maxcumh_p, 0.05)
  expect_equal(mincumh_p, 0.95)
})

test_that("Case S == 1000. Equal chances. cert = 0.95", {
  # Create equal width probability graph g to be used for testing.
  v <- rep(1, 1000)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.95)
  mincumh_p <- ew_mincumh_p(g, 0.95)

  # Check result.
  expect_equal(maxcumh_p, 0.95)
  expect_equal(mincumh_p, 0.05)
})

test_that("Case S == 1000. Equal chances. cert = 0.05", {
  # Create equal width probability graph g to be used for testing.
  v <- rep(1, 1000)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.05)
  mincumh_p <- ew_mincumh_p(g, 0.05)

  # Check result.
  expect_equal(maxcumh_p, 0.05)
  expect_equal(mincumh_p, 0.95)
})

test_that("Case S == 2. Unequal chances. cert = 0.95", {
  # Create equal width probability graph g to be used for testing.
  # Chance graph is the straight line y = 2x, i.e. h = 2p.
  v <- c(1, 3)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.95)
  mincumh_p <- ew_mincumh_p(g, 0.95)

  # Check result.
  expect_equal(maxcumh_p, 0.97467943448)
  expect_equal(mincumh_p, 0.22360679775)
})

test_that("Case S == 2. Unequal chances. cert = 0.05", {
  # Create equal width probability graph g to be used for testing.
  # Chance graph is the straight line y = 2x, i.e. h = 2p.
  v <- c(1, 3)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.05)
  mincumh_p <- ew_mincumh_p(g, 0.05)

  # Check result.
  expect_equal(maxcumh_p, 0.22360679775)
  expect_equal(mincumh_p, 0.97467943448)
})

test_that("Case S == 2. Unequal chances. cert = 0.95", {
  # Create equal width probability graph g to be used for testing.
  # Chance graph is the straight line y = 2x + 2, i.e. h = -2p + 2.
  v <- c(3, 1)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.95)
  mincumh_p <- ew_mincumh_p(g, 0.95)

  # Check result.
  expect_equal(maxcumh_p, 0.77639320225)
  expect_equal(mincumh_p, 0.0253205655)
})

test_that("Case S == 2. Unequal chances. cert = 0.05", {
  # Create equal width probability graph g to be used for testing.
  # Chance graph is the straight line y = 2x + 2, i.e. h = -2p + 2.
  v <- c(3, 1)
  g <- ew_from_vec(v)

  # Test.
  maxcumh_p <- ew_maxcumh_p(g, 0.05)
  mincumh_p <- ew_mincumh_p(g, 0.05)

  # Check result.
  expect_equal(maxcumh_p, 0.0253205655)
  expect_equal(mincumh_p, 0.77639320225)
})
