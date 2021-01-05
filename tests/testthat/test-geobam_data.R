test_that("get_mean returns correct value on a 5 x 5 array", {

  # Create array
  a <- array(c(-1.228,	-0.979,	-0.903, 4.001,	3.479,
               3.645, 3.696, 3.764, 3.682, 5.281, 5.04,
               5.62, 5.151, 5.34, 5.846), dim = c (3, 5))

  # Define expected and actual means
  expected_mean <- c(-1.0367, 3.7083, 3.7140, 5.3137, 5.4457)
  actual_mean <- get_mean(a)

  # Assert result
  expect_equal(round(actual_mean, digits = 4), expected_mean)

})

test_that("get_mean returns correct value on a 3 x 1 x 3 array", {

  # Create array
  a <- array(c(3.20,	3.06,	4.18,
               3.38,	3.91,	4.19,
               4.53,	3.82,	4.12),
             dim = c (3, 1, 3))

  # Define expected and actual means
  expected_mean <- c(3.48, 3.8267, 4.1567)
  actual_mean <- get_mean(a)

  # Assert result
  expect_equal(round(actual_mean, digits = 4), expected_mean)

})

test_that("get_sd returns correct value on a 5 x 5 array", {

  # Create array
  a <- array(c(-1.228,	-0.979,	-0.903, 4.001,	3.479,
               3.645, 3.696, 3.764, 3.682, 5.281, 5.04,
               5.62, 5.151, 5.34, 5.846), dim = c (3, 5))

  # Define expected and actual sigmas
  expected_sd <- c(0.1700009804, 0.2667008311, 0.0438634244, 0.2913766177, 0.3593470931)
  actual_sd <- get_sd(a)

  # Assert result
  expect_equal(actual_sd, round(expected_sd, digits = 8))

})

test_that("get_sd returns correct value on a 3 x 1 x 3 array", {

  # Create array
  a <- array(c(3.20,	3.06,	4.18,
               3.38,	3.91,	4.19,
               4.53,	3.82,	4.12),
             dim = c (3, 1, 3))

  # Define expected and actual sigmas
  expected_sd <- c(0.6102458521, 0.4113797921, 0.3564173584)
  actual_sd <- get_sd(a)

  # Assert result
  expect_equal(actual_sd, round(expected_sd, digits = 8))

})

