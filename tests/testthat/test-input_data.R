test_that("get_input returns a list of expected values", {

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
  expected_list <- list(data_list = list(width = width, slope2 = slope2, d_x_area = d_x_area, qhat = qhat, reachid = "2_1"))

  # Run get_input
  actual_list <- get_input("SevernRiver_Reach_2_1.nc")

  # Compare reachids
  expect_equal(actual_list$data_list$reachid, expected_list$data_list$reachid)

  # Put into a comparable form for other list values
  expected_list$data_list$d_x_area[is.na(expected_list$data_list$d_x_area)] <- 1
  actual_list$data_list$d_x_area[is.na(actual_list$data_list$d_x_area)] <- 1
  expected_list <- expected_list$data_list[names(expected_list$data_list) != "reachid"]
  actual_list <- actual_list$data_list[names(actual_list$data_list) != "reachid"]

  # Compare
  expected <- rep(TRUE, 46)
  actual <- dplyr::near(unlist(unname(unlist(expected_list))), unlist(unname(unlist(actual_list))), tol = 0.001)

  # Assert result
  expect_equal(actual, expected)

})
