test_that("write_netcdf writes a properly format netCDF file", {

  # Define parameters for function
  df <- readRDS(file = "test_df.rds")
  output <- file.path(getwd())

  # Run the function and retrieve dataset
  write_netcdf(df, output)
  dataset <- ncdf4::nc_open("geobamr_output_ALL.nc")

  # Assert list of dimensions
  actual_dims <- attributes(dataset$dim)$names
  expected_dims <- c("nt", "nx")
  expect_equal(actual_dims, expected_dims)

  # Assert list of attributes
  actual_vars <- attributes(dataset$var)$names
  expected_vars <- c("2_1/logQ_mean", "2_1/logn_man_mean",
                     "2_1/logn_amhg_mean", "2_1/A0_mean", "2_1/logWc_mean",
                     "2_1/logQc_mean", "2_1/b_mean", "2_1/logr_mean",
                     "2_1/logWb_mean", "2_1/logDb_mean", "2_1/logQ_sigma",
                     "2_1/logn_man_sigma",  "2_1/logn_amhg_sigma",
                     "2_1/A0_sigma", "2_1/logWc_sigma", "2_1/logQc_sigma",
                     "2_1/b_sigma", "2_1/logr_sigma", "2_1/logWb_sigma",
                     "2_1/logDb_sigma")
  expect_equal(actual_vars, expected_vars)

  # Assert random variable lengths
  logn_man_mean <- ncdf4::ncvar_get(dataset, "2_1/logn_man_mean")
  expect_equal(length(logn_man_mean), 5)

  a0_mean <- ncdf4::ncvar_get(dataset, "2_1/A0_mean")
  expect_equal(length(a0_mean), 5)

  logr_mean <- ncdf4::ncvar_get(dataset, "2_1/logr_mean")
  expect_equal(length(logr_mean), 5)

  b_sigma <- ncdf4::ncvar_get(dataset, "2_1/b_sigma")
  expect_equal(length(b_sigma), 5)

  logq_sigma <- ncdf4::ncvar_get(dataset, "2_1/logQ_sigma")
  expect_equal(length(logq_sigma), 10)

  logwc_sigma <- ncdf4::ncvar_get(dataset, "2_1/logWc_sigma")
  expect_equal(length(logwc_sigma), 1)

})


