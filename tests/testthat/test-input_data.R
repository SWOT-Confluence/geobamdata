test_that("get_invalid returns a list of invalid nodes and times", {

  # Create observation matrix input
  node_obs <- matrix(1:100, nrow = 10, byrow = TRUE)
  node_obs[1,] <- rep(NA, 10)
  node_obs[3,] <- rep(NA, 10)

  time_obs <- matrix(1:100, nrow = 10, byrow = TRUE)
  time_obs[,6] <- rep(NA, 5)
  time_obs[,8] <- rep(NA, 5)

  invalid_time = colSums(is.na(time_obs)) >= (nrow(time_obs) - 5)

  # Run function and assert results
  actual_nodes <- get_invalid(node_obs)$invalid_nodes
  expected_nodes <- rep(FALSE, 10)
  expected_nodes[1] <- TRUE
  expected_nodes[3] <- TRUE
  expect_equal(actual_nodes, expected_nodes)

  actual_times <- get_invalid(time_obs)$invalid_time
  expected_times <- rep(FALSE, 10)
  expected_times[6] <- TRUE
  expected_times[8] <- TRUE
  expect_equal(actual_times, expected_times)

})

test_that("check_observations returns a list of valid observations", {

  # Create observation matrices for width, dA, and slope
  width <- matrix(1:100, nrow = 10, byrow = TRUE)
  width[5,] <- rep(NA, 10)
  width[1,] <- rep(NA, 10)
  width[,7] <- rep(NA, 10)
  dA <- matrix(1:100, nrow = 10, byrow = TRUE)
  dA[5,] <- rep(NA, 10)
  dA[,7] <- rep(NA, 10)
  slope <- matrix(1:100, nrow = 10, byrow = TRUE)
  slope[5,] <- rep(NA, 10)
  slope[,7] <- rep(NA, 10)

  # Run function and assert result
  actual_obs <- check_observations(width, dA, slope, 2)
  expected_w <- matrix(1:100, nrow = 10, byrow = TRUE)
  expected_w <- expected_w[-5,]
  expected_w <- expected_w[-1,]
  expected_w <- expected_w[,-7]
  expect_equal(actual_obs$width, expected_w)

  expected_dA <- matrix(1:100, nrow = 10, byrow = TRUE)
  expected_dA <- expected_dA[-5,]
  expected_dA <- expected_dA[-1,]
  expected_dA <- expected_dA[,-7]
  expect_equal(actual_obs$d_x_area, expected_dA)

  expected_s <- matrix(1:100, nrow = 10, byrow = TRUE)
  expected_s <- expected_s[-5,]
  expected_s <- expected_s[-1,]
  expected_s <- expected_s[,-7]
  expect_equal(actual_obs$slope2, expected_s)

  expect_equal(actual_obs$qhat, 2)
  expect_equal(actual_obs$invalid_nodes, c(1,5))
  expect_equal(actual_obs$invalid_time, c(7))

})

test_that("check_observations returns a list of invalid observations", {

  # Create observation matrices for width, dA, and slope with invalid data
  width <- matrix(rep(NA, 100), nrow = 10, byrow = TRUE)
  width[1,] <- 1:10
  width[,7] <- 1:10
  dA <- matrix(rep(NA, 100), nrow = 10, byrow = TRUE)
  dA[1,] <- 1:10
  dA[,7] <- 1:10
  slope <- matrix(rep(NA, 100), nrow = 10, byrow = TRUE)
  slope[1,] <- 1:10
  slope[,7] <- 1:10

  # Run function and assert result
  actual_obs <- check_observations(width, dA, slope, 2)
  expected_obs <- vector(mode = "list")
  expect_equal(actual_obs, expected_obs)

})

test_that("get_input_data returns a list of valid observations", {

  # Execute function
  data_dir <- file.path(getwd(), "data")
  reachid <- "001_1"
  reach_data <- get_input_data(reachid, data_dir)

  # Assert result
  expect_equal(reach_data$valid, TRUE)
  expect_equal(reach_data$reachid, "001_1")

  expected_w <-matrix(1:100, nrow = 10, byrow = TRUE)
  expected_w <- expected_w[-2,]
  expected_w <- expected_w[-5,]
  expected_w <- expected_w[,-8]
  expect_equal(reach_data$width, expected_w)

  expected_dA <-matrix(1:100, nrow = 10, byrow = TRUE)
  expected_dA <- expected_dA[-2,]
  expected_dA <- expected_dA[-5,]
  expected_dA <- expected_dA[,-8]
  expect_equal(reach_data$d_x_area, expected_dA)

  expected_s <-matrix(1:100, nrow = 10, byrow = TRUE)
  expected_s <- expected_s[-2,]
  expected_s <- expected_s[-5,]
  expected_s <- expected_s[,-8]
  expect_equal(reach_data$slope2, expected_s)

  expect_equal(reach_data$invalid_nodes, c(6,2))
  expect_equal(reach_data$invalid_time, c(8))
})
