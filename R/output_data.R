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

  # Create list of all posterior mean and sd variables
  var_list <- lapply(list(1,2,3), create_vars, reachid = reach_data$reachid,
                     nt_dim = dim_list$nt_dim, nx_dim = dim_list$nx_dim)

  # Define reachid variable
  nchar_dim <- ncdf4::ncdim_def("nchar", "", 1:10, create_dimvar = FALSE)
  id <- ncdf4::ncvar_def(paste(reach_data$reachid, "reach_id", sep ='/'),
                         units = '', dim = list(nchar_dim), prec = "char")

  # Create netcdf
  all_vars <- append(var_list, list(list(id = id)))
  all_vars <- unlist(all_vars, recursive = FALSE, use.names = FALSE)
  nc_out <- create_nc_file(reach_data$reachid, all_vars, output_dir)

  # Concatenate invalid nodes back into valid posterior reach data
  if (reach_data$valid == TRUE) {
    posterior_list <- lapply(posterior_list, concatenate_invalid,
                                   invalid_nodes = reach_data$invalid_nodes,
                                   invalid_time = reach_data$invalid_time)
  }

  # Write valid global attribute, reach identifier and posteriors to netcdf
  if (reach_data$valid == TRUE) valid = 1 else valid = 0
  ncdf4::ncatt_put(nc_out, 0, "valid", valid)
  ncdf4::ncvar_put(nc_out, id, reach_data$reachid)
  lapply(list(1,2,3), write_vars, posterior_list = posterior_list,
         var_list = var_list, nc_out = nc_out)

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
#' @param chain integer value that indicates chain number
#' @param reachid string reach identifier
#' @param nt_dim ncdim
#' @param nx_dim ncdim
#'
#' @return list ncvar
create_vars <- function(chain, reachid, nt_dim, nx_dim) {

  # Mean variables
  mean <- paste("mean_chain", chain, sep='')
  logq_m <- ncdf4::ncvar_def(paste(reachid, "logQ", mean, sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  logn_man_m <- ncdf4::ncvar_def(paste(reachid, "logn_man", mean, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logn_amhg_m <- ncdf4::ncvar_def(paste(reachid, "logn_amhg", mean, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  A0_m <- ncdf4::ncvar_def(paste(reachid, "A0", mean, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWc_m <- ncdf4::ncvar_def(paste(reachid, "logWc", mean, sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  logQc_m <- ncdf4::ncvar_def(paste(reachid, "logQc", mean, sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  b_m <- ncdf4::ncvar_def(paste(reachid, "b", mean, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logr_m <- ncdf4::ncvar_def(paste(reachid, "logr", mean, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWb_m <- ncdf4::ncvar_def(paste(reachid, "logWb", mean, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logDb_m <- ncdf4::ncvar_def(paste(reachid, "logDb", mean, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")

  # Sigma variables
  sd <- paste("sd_chain", chain, sep='')
  logq_sd <- ncdf4::ncvar_def(paste(reachid, "logQ", sd, sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  logn_man_sd <- ncdf4::ncvar_def(paste(reachid, "logn_man", sd, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logn_amhg_sd <- ncdf4::ncvar_def(paste(reachid, "logn_amhg", sd, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  A0_sd <- ncdf4::ncvar_def(paste(reachid, "A0", sd, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWc_sd <- ncdf4::ncvar_def(paste(reachid, "logWc", sd, sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  logQc_sd <- ncdf4::ncvar_def(paste(reachid, "logQc", sd, sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  b_sd <- ncdf4::ncvar_def(paste(reachid, "b", sd, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logr_sd <- ncdf4::ncvar_def(paste(reachid, "logr", sd, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWb_sd <- ncdf4::ncvar_def(paste(reachid, "logWb", sd, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logDb_sd <- ncdf4::ncvar_def(paste(reachid, "logDb", sd, sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")

  # List of all variables
  return(list(logq_m = logq_m, logn_man_m = logn_man_m,
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
#' @param index integer index number for parameter lists
#' @param posterior_list list of posterior data
#' @param var_list list of ncvar
#' @param nc_out ncdf4
#'
#' @return None
write_vars <- function(index, posterior_list, var_list, nc_out) {

  # Mean variables
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logq_m, posterior_list[[index]]$logQ_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logn_man_m, posterior_list[[index]]$logn_man_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logn_amhg_m, posterior_list[[index]]$logn_amhg_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$A0_m, posterior_list[[index]]$A0_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logWc_m, posterior_list[[index]]$logWc_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logQc_m, posterior_list[[index]]$logQc_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$b_m, posterior_list[[index]]$b_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logr_m, posterior_list[[index]]$logr_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logWb_m, posterior_list[[index]]$logWb_mean)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logDb_m, posterior_list[[index]]$logDb_mean)

  # Sigma variables
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logq_sd, posterior_list[[index]]$logQ_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logn_man_sd, posterior_list[[index]]$logn_man_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logn_amhg_sd, posterior_list[[index]]$logn_amhg_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$A0_sd, posterior_list[[index]]$A0_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logWc_sd, posterior_list[[index]]$logWc_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logQc_sd, posterior_list[[index]]$logQc_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$b_sd, posterior_list[[index]]$b_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logr_sd, posterior_list[[index]]$logr_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logWb_sd, posterior_list[[index]]$logWb_sd)
  ncdf4::ncvar_put(nc_out, var_list[[index]]$logDb_sd, posterior_list[[index]]$logDb_sd)
}
