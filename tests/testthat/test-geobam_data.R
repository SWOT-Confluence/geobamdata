test_that("get_data returns a list of expected values", {

  # Create expected list
  slope2 <- matrix(c(0.0001638646, 0.0001638646, 0.0001643724, 0.0001798677, 0.0001824383,
              0.0001638646, 0.0001638646, 0.0001643724, 0.0001798677, 0.0001824383,
              0.0001638646, 0.0001638646, 0.0001643724, 0.0001798677, 0.0001824383),
              nrow = 3, byrow = TRUE)
  width <- matrix(c(1,  180, 180, 630, 630,
                    1, 180, 180, 540, 540,
                    1, 180, 180, 540, 540),
                  nrow = 3, byrow = TRUE)

  d_x_area <- matrix(c(NA, NA, NA, -271.5525, -282.735,
                       NA, NA, NA, -217.2420, -226.188,
                       NA, NA, NA, -217.2420, -226.188),
                     nrow = 3, byrow = TRUE)
  qhat <- 21.01557
  expected_list <- list(width = width, slope2 = slope2, d_x_area = d_x_area, qhat = qhat)

  # Run get_data
  file <- ncdf4::nc_open("SevernRiver_Reach_2_1.nc")
  actual_list <- get_data(file)

  # Put into a comparable form
  expected_list$d_x_area[is.na(expected_list$d_x_area)] <- 1
  actual_list$d_x_area[is.na(actual_list$d_x_area)] <- 1

  # Compare
  expected <- rep(TRUE, 46)
  actual <- dplyr::near(unlist(unname(expected_list)), unlist(unname(actual_list)), tol = 0.001)

  # Assert result
  expect_equal(actual, expected)

})

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

