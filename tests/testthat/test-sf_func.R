test_that("sf_func works", {
  vals <- c(1, 2, 3, 4) %>% as.matrix()
  ws <- c(5, 6, 7, 8) %>% as.matrix()
  fracs <- c(0.5, 0, 1, 0.25) %>% as.matrix()

  expect_equal(sf_sum_weighted(vals, fracs, ws), 31.5)
  expect_equal(sf_mean_weighted(vals, fracs, ws), 2.7391, tolerance = 1e-4)

  expect_equal(sf_mean(vals, fracs), 2.57, tolerance = 1e-3)
  expect_equal(sf_sum(vals, fracs), 4.5)
})
# system.time(res <- extract2(r, geoms))
