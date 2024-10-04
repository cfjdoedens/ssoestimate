test_that("ew_middle_peaked() Case S == 1.", {
  v <- c(1)

  g <- ew_from_vec(v)

  # Test.
  bool <- ew_middle_peaked(g)

  # Check result.
  expect_equal(bool, FALSE)
})

test_that("ew_middle_peaked() Case S == 2.", {
  v <- c(1, 1)

  g <- ew_from_vec(v)

  # Test.
  bool <- ew_middle_peaked(g)

  # Check result.
  expect_equal(bool, FALSE)
})

test_that("ew_middle_peaked() Case S == 3 not middle peeked", {
  v <- c(1, 2, 3)

  g <- ew_from_vec(v)

  # Test.
  bool <- ew_middle_peaked(g)

  # Check result.
  expect_equal(bool, FALSE)
})

test_that("ew_middle_peaked() Case S == 3 middle peeked", {
  v <- c(1, 5, 1)

  g <- ew_from_vec(v)

  # Test.
  bool <- ew_middle_peaked(g)

  # Check result.
  expect_equal(bool, TRUE)
})

test_that("ew_middle_peaked() Case S == 3 also middle peeked", {
  v <- c(1, 1000, 1)

  g <- ew_from_vec(v)

  # Test.
  bool <- ew_middle_peaked(g)

  # Check result.
  expect_equal(bool, TRUE)
})

