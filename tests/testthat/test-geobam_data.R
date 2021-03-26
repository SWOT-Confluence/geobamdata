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

test_that("extract_geobam_posteriors returns a list of posterior sd and mean values", {

  # Get data to work with
  data_dir <- file.path(getwd(), "data")
  reachid <- "001_1"
  reach_data <- get_input_data(reachid, data_dir)

  # Run function
  geobam <- geoBAMr::bam_data(w = reach_data$width, s = reach_data$slope2,
                              dA = reach_data$d_x_area, Qhat = reach_data$qhat,
                              variant = 'manning_amhg', max_xs = nrow(reach_data$width))
  priors <- geoBAMr::bam_priors(bamdata = geobam)
  posteriors <- extract_geobam_posteriors(geobam, priors)

  # Assert result
  expect_equal(length(posteriors[[1]]$mean), 10)
  expect_equal(length(posteriors[[1]]$sd), 10)

})

test_that("update_posteriors updates an empty list", {

  # Get data to work with
  p_file <- file.path(getwd(), "data", "posteriors.yaml")
  posteriors <- yaml::read_yaml(file = p_file)
  posterior_list <- create_posterior_list(10, 10)

  # Run function
  posterior_list <- lapply(list(1, 2, 3), update_posteriors, posteriors = posteriors, posterior_list = posterior_list)

  # Assert result
  expect_equal(length(posterior_list[[1]]), 20)
  expect_equal(length(posterior_list[[2]]), 20)
  expect_equal(length(posterior_list[[3]]), 20)

  expected_vals <- logical(length = 20)
  names(expected_vals) <- c("logQ_mean",  "logn_man_mean", "logn_amhg_mean",
                            "A0_mean", "logWc_mean", "logQc_mean", "b_mean",
                            "logr_mean", "logWb_mean", "logDb_mean", "logQ_sd",
                            "logn_man_sd", "logn_amhg_sd", "A0_sd", "logWc_sd",
                            "logQc_sd", "b_sd", "logr_sd", "logWb_sd", "logDb_sd")
  expect_equal(is.na(posterior_list[[1]]), expected_vals)
  expect_equal(is.na(posterior_list[[2]]), expected_vals)
  expect_equal(is.na(posterior_list[[3]]), expected_vals)

})

