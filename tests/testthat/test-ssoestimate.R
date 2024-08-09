test_that("SSO_estimate() works, and uses defaults as expected.", {
  skip_on_cran()
  expect_equal(
    SSO_estimate(),
    SSO_estimate(
      k_SSO = 0,
      n_SSO = 350,
      N_client = 1000,
      S = 2000
    )
  )
})

test_that("SSO_estimate() should still function with small values for S", {
  expect_silent(x <- SSO_estimate(k_SSO = 0, n_SSO = 350, N_client = 1000, S = 1))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(k_SSO = 0, n_SSO = 350, N_client = 1000, S = 2))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(k_SSO = 0, n_SSO = 350, N_client = 1000, S = 3))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(k_SSO = 1, n_SSO = 350, N_client = 1000, S = 1))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(k_SSO = 1, n_SSO = 350, N_client = 1000, S = 2))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(k_SSO = 1, n_SSO = 350, N_client = 1000, S = 3))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(k_SSO = 350, n_SSO = 350, N_client = 1000, S = 1))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(k_SSO = 350, n_SSO = 350, N_client = 1000, S = 2))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(k_SSO = 350, n_SSO = 350, N_client = 1000, S = 3))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))
})
