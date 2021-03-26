test_that("concatenate_invalid inserts NA values at appropriate place", {

  # Get data to work with
  posterior_list <- yaml::read_yaml(file = file.path(getwd(), "data", "posterior_list.yaml"))
  invalid_nodes <- c(1,3)
  invalid_time <- c(7)

  # Run function
  posterior_list <- lapply(posterior_list, concatenate_invalid,
                           invalid_nodes = invalid_nodes, invalid_time = invalid_time)

  # Assert result
  expect_equal(length(posterior_list[[1]]$logQ_mean), 11)
  expect_equal(length(posterior_list[[2]]$logQ_mean), 11)
  expect_equal(length(posterior_list[[3]]$logQ_mean), 11)

  expect_equal(length(posterior_list[[1]]$b_sd), 12)
  expect_equal(length(posterior_list[[2]]$b_sd), 12)
  expect_equal(length(posterior_list[[3]]$b_sd), 12)

  expect_true(is.na(posterior_list[[1]]$logQ_mean[7]))
  expect_true(is.na(posterior_list[[2]]$logQ_mean[7]))
  expect_true(is.na(posterior_list[[3]]$logQ_mean[7]))

  expect_true(is.na(posterior_list[[1]]$b_sd[1]))
  expect_true(is.na(posterior_list[[2]]$b_sd[1]))
  expect_true(is.na(posterior_list[[3]]$b_sd[1]))

  expect_true(is.na(posterior_list[[1]]$b_sd[3]))
  expect_true(is.na(posterior_list[[2]]$b_sd[3]))
  expect_true(is.na(posterior_list[[3]]$b_sd[3]))

})

test_that("write_netcdf writes posterior chain sd and mean values as variables", {

  # Get data to work with
  reach_data <- yaml::read_yaml(file = file.path(getwd(), "data", "reach_data.yaml"))
  posterior_list <- yaml::read_yaml(file = file.path(getwd(), "data", "posterior_list.yaml"))
  output_dir <- file.path(getwd(), "data")

  # Run function
  write_netcdf(reach_data, posterior_list, output_dir)

  # Assert result
  result <- ncdf4::nc_open(file.path(getwd(), "data", "002_6683_geobam.nc"))
  expect_equal(ncdf4::ncatt_get(result, 0)$valid[[1]], 1)
  expect_equal(ncdf4::ncatt_get(result, 0)$reachid[[1]], "002_6683")
  expect_equal(length(ncdf4::ncvar_get(result, "nx")), 11)
  expect_equal(length(ncdf4::ncvar_get(result, "nt")), 10)

  expect_equal(length(ncdf4::ncvar_get(result, "logQ/mean_chain1")), 10)
  expect_equal(length(ncdf4::ncvar_get(result, "logQ/mean_chain2")), 10)
  expect_equal(length(ncdf4::ncvar_get(result, "logQ/mean_chain3")), 10)

  expect_equal(length(ncdf4::ncvar_get(result, "A0/mean_chain1")), 11)
  expect_equal(length(ncdf4::ncvar_get(result, "A0/mean_chain2")), 11)
  expect_equal(length(ncdf4::ncvar_get(result, "A0/mean_chain3")), 11)

  expect_true(is.na(ncdf4::ncvar_get(result, "A0/mean_chain1")[8]))
  expect_true(is.na(ncdf4::ncvar_get(result, "A0/mean_chain2")[8]))
  expect_true(is.na(ncdf4::ncvar_get(result, "A0/mean_chain3")[8]))

  expect_equal(length(ncdf4::ncvar_get(result, "logQc/mean_chain1")), 1)
  expect_equal(length(ncdf4::ncvar_get(result, "logQc/mean_chain2")), 1)
  expect_equal(length(ncdf4::ncvar_get(result, "logQc/mean_chain3")), 1)

  ncdf4::nc_close(result)


})
