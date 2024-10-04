test_that("Is bug still present?: SSO_estimate(k_SSO = 0, n_SSO = 1, N_client = 1, S = 53)", {
  skip_on_cran()
  expect_silent(x <- SSO_estimate(k_SSO = 0, n_SSO = 1, N_client = 1, S = 53))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
})

test_that("Is bug still present?: client_min <= client_max is not TRUE", {
  expect_silent(x <- SSO_estimate(
    k_SSO = 350,
    n_SSO = 350,
    N_client = 1000,
    S = 3
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
})

test_that("SSO_estimate() and SSO_graph_plot() can cope with non integer value for k_SSO",
          {
            expect_silent(x <- SSO_estimate(
              k_SSO = 0.7,
              n_SSO = 3,
              N_client = 2,
              S = 1
            ))
            expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
            expect_silent(y <- SSO_graph_plot(x, visual = TRUE))
          })

test_that("SSO_estimate() should function with small values for S", {
  expect_silent(x <- SSO_estimate(
    k_SSO = 0,
    n_SSO = 350,
    N_client = 1000,
    S = 1
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(
    k_SSO = 0,
    n_SSO = 350,
    N_client = 1000,
    S = 2
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(
    k_SSO = 0,
    n_SSO = 350,
    N_client = 1000,
    S = 3
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(
    k_SSO = 1,
    n_SSO = 350,
    N_client = 1000,
    S = 1
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(
    k_SSO = 1,
    n_SSO = 350,
    N_client = 1000,
    S = 2
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(
    k_SSO = 1,
    n_SSO = 350,
    N_client = 1000,
    S = 3
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(
    k_SSO = 350,
    n_SSO = 350,
    N_client = 1000,
    S = 1
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(
    k_SSO = 350,
    n_SSO = 350,
    N_client = 1000,
    S = 2
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))

  expect_silent(x <- SSO_estimate(
    k_SSO = 350,
    n_SSO = 350,
    N_client = 1000,
    S = 3
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_silent(y <- SSO_graph_plot(x, visual = TRUE))
})

test_that("SSO_estimate() works, and uses defaults as expected.", {
  skip_on_cran()
  expect_equal(SSO_estimate(),
               SSO_estimate(
                 k_SSO = 0,
                 n_SSO = 350,
                 N_client = 1000,
                 S = 2000
               ))
})

test_that("check results of SSO_estimate(k_SSO = 0, n_SSO = 400, N_client = 500, S = 2000)", {
  skip_on_cran()
  expect_silent(x <- SSO_estimate(
    k_SSO = 0,
    n_SSO = 400,
    N_client = 500,
    S = 2000
  ))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
  expect_equal(y[["client_max"]], 0.0101153767)
  expect_equal(y[["client_most_prob"]], 0.0000000000)
  expect_equal(y[["client_min"]], 0.0001751760)
  expect_equal(y[["SSO_max"]], 0.0074444373)
  expect_equal(y[["SSO_most_prob"]], 0.0000000000)
  expect_equal(y[["SSO_min"]], 0.0001288988)
})

test_that("Is bug still present?: SSO_estimate(k_SSO = 1, n_SSO = 3000, N_client = 500, S = 501)", {
  skip_on_cran()
  expect_silent(x <- SSO_estimate(k_SSO = 1, n_SSO = 3000, N_client = 500, S = 501))
  expect_silent(y <- SSO_graph_plot(x, visual = FALSE))
})
