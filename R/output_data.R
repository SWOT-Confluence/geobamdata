#' Write netCDF
#'
#' Writes netCDF of posterior data to output directory labelled with reach
#' identifier.
#'
#' @param reach_data list of reach data (observations)
#' @param posterior_list list of posterior data
#' @param output_dir string path to output directory
#'
#' @return None
#'
#' @export
write_netcdf <- function(reach_data, posterior_list, output_dir) {

  # Define dimensions
  dim_list <- create_dimensions(reach_data$nx, reach_data$nt)

  # Create list of all variables
  var_list <- create_vars(reach_data$reachid, dim_list$nt_dim, dim_list$nx_dim)

  # Create netcdf file
  nc_out <- create_nc_file(reach_data$reachid, var_list, output_dir)

  # Concatenate invalid nodes back into valid posterior reach data
  if (reach_data$valid == TRUE) {
    posterior_list <- concatenate_invalid(posterior_list, reach_data$invalid_nodes, reach_data$invalid_time)
  }

  # Write all variables to the netcdf file
  write_vars(reach_data$valid, posterior_list, var_list, nc_out)

  # Close nc file
  ncdf4::nc_close(nc_out)
}

#' Create netCDF dimensions
#'
#' @param nx integer vector that represents number of nodes
#' @param nt integer vector that represents time steps
#'
#' @return list ncdim
create_dimensions <- function(nx, nt) {

  # Dimensions
  nx_dim <- ncdf4::ncdim_def(name = "nx", units = "", vals = nx, create_dimvar=TRUE)
  nt_dim <- ncdf4::ncdim_def(name = "nt", units = "", vals = nt, create_dimvar=TRUE)

  return(list(nt_dim = nt_dim, nx_dim = nx_dim))
}

#' Create a list of netcdf4 variables
#'
#' Create list of mean and sigma netcdf variables grouped by reachid.
#'
#' @param reachid string reach identifier
#' @param nt_dim ncdim
#' @param nx_dim ncdim
#'
#' @return list ncvar
create_vars <- function(reachid, nt_dim, nx_dim) {

  # Reach identifier variable
  nchar_dim <- ncdf4::ncdim_def("nchar", "", 1:8, create_dimvar = FALSE)
  id <- ncdf4::ncvar_def(paste(reachid, "reach_id", sep ='/'), units = '', dim = list(nchar_dim), prec = "char")

  # Mean variables
  logq_m <- ncdf4::ncvar_def(paste(reachid, "logQ_mean", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  logn_man_m <- ncdf4::ncvar_def(paste(reachid, "logn_man_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logn_amhg_m <- ncdf4::ncvar_def(paste(reachid, "logn_amhg_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  A0_m <- ncdf4::ncvar_def(paste(reachid, "A0_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWc_m <- ncdf4::ncvar_def(paste(reachid, "logWc_mean", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  logQc_m <- ncdf4::ncvar_def(paste(reachid, "logQc_mean", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  b_m <- ncdf4::ncvar_def(paste(reachid, "b_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logr_m <- ncdf4::ncvar_def(paste(reachid, "logr_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWb_m <- ncdf4::ncvar_def(paste(reachid, "logWb_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logDb_m <- ncdf4::ncvar_def(paste(reachid, "logDb_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")

  # Sigma variables
  logq_sd <- ncdf4::ncvar_def(paste(reachid, "logQ_sigma", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  logn_man_sd <- ncdf4::ncvar_def(paste(reachid, "logn_man_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logn_amhg_sd <- ncdf4::ncvar_def(paste(reachid, "logn_amhg_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  A0_sd <- ncdf4::ncvar_def(paste(reachid, "A0_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWc_sd <- ncdf4::ncvar_def(paste(reachid, "logWc_sigma", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  logQc_sd <- ncdf4::ncvar_def(paste(reachid, "logQc_sigma", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  b_sd <- ncdf4::ncvar_def(paste(reachid, "b_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logr_sd <- ncdf4::ncvar_def(paste(reachid, "logr_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWb_sd <- ncdf4::ncvar_def(paste(reachid, "logWb_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logDb_sd <- ncdf4::ncvar_def(paste(reachid, "logDb_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")

  # List of all variables
  return(list(id = id, logq_m = logq_m, logn_man_m = logn_man_m,
              logn_amhg_m = logn_amhg_m, A0_m = A0_m, logWc_m = logWc_m,
              logQc_m = logQc_m, b_m = b_m, logr_m = logr_m,
              logWb_m = logWb_m, logDb_m = logDb_m, logq_sd = logq_sd,
              logn_man_sd = logn_man_sd, logn_amhg_sd = logn_amhg_sd,
              A0_sd = A0_sd, logWc_sd = logWc_sd, logQc_sd = logQc_sd, b_sd = b_sd,
              logr_sd = logr_sd, logWb_sd = logWb_sd, logDb_sd = logDb_sd))
}

#
#' Create netCDF output file
#'
#' @param reachid string reach identifier
#' @param var_list list ncvar
#' @param output_dir character
#'
#' @return ncdf4
create_nc_file <- function(reachid, var_list, output_dir) {

  nc_filename <- paste(reachid, "_geobam.nc", sep = '')
  nc_file <- paste(output_dir, nc_filename, sep=.Platform$file.sep)
  return(ncdf4::nc_create(nc_file, var_list, force_v4 = TRUE))
}

#' Insert NA values back into posterior list vectors to account for invalid
#' nodes
#'
#' @param posterior_list list of posterior data vectors
#' @param invalid_nodes list of invalid node indexes
#' @param invalid_time list of invalid time indexes
#'
#' @return list of posterior data with NA in place of invalid nodes
concatenate_invalid <- function(posterior_list, invalid_nodes, invalid_time) {

  # Node data
  #nx_vector <- rep(NA, length(invalid_nodes))
  #logn_man_mean <- c(posterior_list$logn_man_mean, nx_vector)
  #v <- R.utils::insert(logn_man_mean, at = invalid_nodes, values = nx_vector)

  # Node-level data
  for (index in invalid_nodes) {
    posterior_list$logn_man_mean <- append(posterior_list$logn_man_mean, NA, after = index - 1)
    posterior_list$logn_man_sd <- append(posterior_list$logn_man_sd, NA, after = index - 1)
    posterior_list$logn_amhg_mean <- append(posterior_list$logn_amhg_mean, NA, after = index - 1)
    posterior_list$logn_amhg_sd <- append(posterior_list$logn_amhg_sd, NA, after = index - 1)
    posterior_list$A0_mean <- append(posterior_list$A0_mean, NA, after = index - 1)
    posterior_list$A0_sd <- append(posterior_list$A0_sd, NA, after = index - 1)
    posterior_list$b_mean <- append(posterior_list$b_mean, NA, after = index - 1)
    posterior_list$b_sd <- append(posterior_list$b_sd, NA, after = index - 1)
    posterior_list$logr_mean <- append(posterior_list$logr_mean, NA, after = index - 1)
    posterior_list$logr_sd <- append(posterior_list$logr_sd, NA, after = index - 1)
    posterior_list$logWb_mean <- append(posterior_list$logWb_mean, NA, after = index - 1)
    posterior_list$logWb_sd <- append(posterior_list$logWb_sd, NA, after = index - 1)
    posterior_list$logDb_mean <- append(posterior_list$logDb_mean, NA, after = index - 1)
    posterior_list$logDb_sd <- append(posterior_list$logDb_sd, NA, after = index - 1)
  }

  # Time-level data
  for (index in invalid_time) {
    posterior_list$logQ_mean <- append(posterior_list$logQ_mean, NA, after = index - 1)
    posterior_list$logQ_sd <- append(posterior_list$logQ_sd, NA, after = index - 1)
  }

  return(posterior_list)

}

#' Write netcdf variables
#'
#' Write reachid, mean and sigma variables to netcdf file. Also includes
#' a valid global attribute to indicate validity of reach data.
#'
#' @param valid boolean value to indicate validity of reach
#' @param posterior_list list of posterior data
#' @param var_list list of ncvar
#' @param nc_out ncdf4
#'
#' @return None
write_vars <- function(valid, posterior_list, var_list, nc_out) {

  # Valid global attribute
  if (valid == TRUE) valid = 1 else valid = 0
  ncdf4::ncatt_put(nc_out, 0, "valid", valid)

  # Reach id
  ncdf4::ncvar_put(nc_out, var_list$id, posterior_list$reachid)

  # Mean variables
  ncdf4::ncvar_put(nc_out, var_list$logq_m, posterior_list$logQ_mean)
  ncdf4::ncvar_put(nc_out, var_list$logn_man_m, posterior_list$logn_man_mean)
  ncdf4::ncvar_put(nc_out, var_list$logn_amhg_m, posterior_list$logn_amhg_mean)
  ncdf4::ncvar_put(nc_out, var_list$A0_m, posterior_list$A0_mean)
  ncdf4::ncvar_put(nc_out, var_list$logWc_m, posterior_list$logWc_mean)
  ncdf4::ncvar_put(nc_out, var_list$logQc_m, posterior_list$logQc_mean)
  ncdf4::ncvar_put(nc_out, var_list$b_m, posterior_list$b_mean)
  ncdf4::ncvar_put(nc_out, var_list$logr_m, posterior_list$logr_mean)
  ncdf4::ncvar_put(nc_out, var_list$logWb_m, posterior_list$logWb_mean)
  ncdf4::ncvar_put(nc_out, var_list$logDb_m, posterior_list$logDb_mean)

  # Sigma variables
  ncdf4::ncvar_put(nc_out, var_list$logq_sd, posterior_list$logQ_sd)
  ncdf4::ncvar_put(nc_out, var_list$logn_man_sd, posterior_list$logn_man_sd)
  ncdf4::ncvar_put(nc_out, var_list$logn_amhg_sd, posterior_list$logn_amhg_sd)
  ncdf4::ncvar_put(nc_out, var_list$A0_sd, posterior_list$A0_sd)
  ncdf4::ncvar_put(nc_out, var_list$logWc_sd, posterior_list$logWc_sd)
  ncdf4::ncvar_put(nc_out, var_list$logQc_sd, posterior_list$logQc_sd)
  ncdf4::ncvar_put(nc_out, var_list$b_sd, posterior_list$b_sd)
  ncdf4::ncvar_put(nc_out, var_list$logr_sd, posterior_list$logr_sd)
  ncdf4::ncvar_put(nc_out, var_list$logWb_sd, posterior_list$logWb_sd)
  ncdf4::ncvar_put(nc_out, var_list$logDb_sd, posterior_list$logDb_sd)
}
