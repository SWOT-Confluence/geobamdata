#' Write netCDF
#'
#' Writes netcdf for results dataframe to output directory as a single file
#' called 'geobamr_output_ALL.nc' with variables groups by Reach ID.
#'
#' @param result_df data.frame
#' @param output_dir character
#'
#' @return None
#'
#' @export
write_netcdf <- function(result_df, output_dir) {

  # Split dataframe by reachid
  df_slices <- split(result_df, f = result_df$reachid)

  # Dimensions
  dim_list <- create_dimensions(df_slices)

  # Create list of all variables
  var_list <- lapply(df_slices, create_vars, nt_dim = dim_list$nt_dim, nx_dim = dim_list$nx_dim)

  # Create netcdf file
  nc_out <- create_nc_file(unlist(var_list, recursive = FALSE), output_dir)

  # Write all variables to the netcdf file
  lapply(df_slices, write_vars, var_list = var_list, nc_out = nc_out)

  # Close nc file
  ncdf4::nc_close(nc_out)
}

#' Create netCDF dimensions
#'
#' @param df_slices data.frame
#'
#' @return list ncdim
create_dimensions <- function(df_slices) {

  # Dimensions
  n1 <- length(df_slices$'2_1'$logQ_mean)
  x <- df_slices$'2_1'$logn_man_mean
  n2 <- length(x[!is.na(x)])
  nt_dim <- ncdf4::ncdim_def(name = "nt", units = "", vals = c(1:n1), create_dimvar=TRUE)
  nx_dim <- ncdf4::ncdim_def(name = "nx", units = "", vals = c(1:n2), create_dimvar=TRUE)

  return(list(nt_dim = nt_dim, nx_dim = nx_dim))
}

#' Create a list of netcdf4 variables
#'
#' Create list of mean and sigma netcdf variables grouped by reachid.
#'
#' @param df_slice data.frame
#' @param nt_dim ncdim
#' @param nx_dim ncdim
#'
#' @return list ncvar
create_vars <- function(df_slice, nt_dim, nx_dim) {

  # Mean variables
  logq_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logQ_mean", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  logn_man_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logn_man_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logn_amhg_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logn_amhg_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  A0_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "A0_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWc_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logWc_mean", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  logQc_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logQc_mean", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  b_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "b_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logr_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logr_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWb_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logWb_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logDb_m <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logDb_mean", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")

  # Sigma variables
  logq_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logQ_sigma", sep ='/'), units = '', dim = nt_dim, missval = -999999999999, prec = "float")
  logn_man_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logn_man_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logn_amhg_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logn_amhg_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  A0_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "A0_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWc_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logWc_sigma", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  logQc_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logQc_sigma", sep ='/'), units = '', dim = list(), missval = -999999999999, prec = "float")
  b_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "b_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logr_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logr_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logWb_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logWb_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")
  logDb_sd <- ncdf4::ncvar_def(paste(df_slice$reachid[1], "logDb_sigma", sep ='/'), units = '', dim = nx_dim, missval = -999999999999, prec = "float")

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
#' @param var_list list ncvar
#' @param output_dir character
#'
#' @return ncdf4
create_nc_file <- function(var_list, output_dir) {

  nc_filename <- "geobamr_output_ALL.nc"
  nc_file <- paste(output_dir, nc_filename, sep=.Platform$file.sep)
  return(ncdf4::nc_create(nc_file, var_list, force_v4 = TRUE))
}

#' Write netcdf variables
#'
#' Write mean and sigma variables to netcdf file.
#'
#' @param df_slice data.frame
#' @param var_list list ncvar
#' @param nc_out ncdf4
#'
#' @return None
write_vars <- function(df_slice, var_list, nc_out) {

  index <- var_list[[df_slice$reachid[1]]]

  # Mean variables
  ncdf4::ncvar_put(nc_out, index$logq_m, df_slice$logQ_mean)
  ncdf4::ncvar_put(nc_out, index$logn_man_m, df_slice$logn_man_mean[!is.na(df_slice$logn_man_mean)])
  ncdf4::ncvar_put(nc_out, index$logn_amhg_m, df_slice$logn_amhg_mean[!is.na(df_slice$logn_amhg_mean)])
  ncdf4::ncvar_put(nc_out, index$A0_m, df_slice$A0_mean[!is.na(df_slice$A0_mean)])
  ncdf4::ncvar_put(nc_out, index$logWc_m, df_slice$logWc_mean[!is.na(df_slice$logWc_mean)])
  ncdf4::ncvar_put(nc_out, index$logQc_m, df_slice$logQc_mean[!is.na(df_slice$logQc_mean)])
  ncdf4::ncvar_put(nc_out, index$b_m, df_slice$b_mean[!is.na(df_slice$b_mean)])
  ncdf4::ncvar_put(nc_out, index$logr_m, df_slice$logr_mean[!is.na(df_slice$logr_mean)])
  ncdf4::ncvar_put(nc_out, index$logWb_m, df_slice$logWb_mean[!is.na(df_slice$logWb_mean)])
  ncdf4::ncvar_put(nc_out, index$logDb_m, df_slice$logDb_mean[!is.na(df_slice$logDb_mean)])

  # Sigma variables
  ncdf4::ncvar_put(nc_out, index$logq_sd, df_slice$logQ_sd)
  ncdf4::ncvar_put(nc_out, index$logn_man_sd, df_slice$logn_man_sd[!is.na(df_slice$logn_man_sd)])
  ncdf4::ncvar_put(nc_out, index$logn_amhg_sd, df_slice$logn_amhg_sd[!is.na(df_slice$logn_amhg_sd)])
  ncdf4::ncvar_put(nc_out, index$A0_sd, df_slice$A0_sd[!is.na(df_slice$A0_sd)])
  ncdf4::ncvar_put(nc_out, index$logWc_sd, df_slice$logWc_sd[!is.na(df_slice$logWc_sd)])
  ncdf4::ncvar_put(nc_out, index$logQc_sd, df_slice$logQc_sd[!is.na(df_slice$logQc_sd)])
  ncdf4::ncvar_put(nc_out, index$b_sd, df_slice$b_sd[!is.na(df_slice$b_sd)])
  ncdf4::ncvar_put(nc_out, index$logr_sd, df_slice$logr_sd[!is.na(df_slice$logr_sd)])
  ncdf4::ncvar_put(nc_out, index$logWb_sd, df_slice$logWb_sd[!is.na(df_slice$logWb_sd)])
  ncdf4::ncvar_put(nc_out, index$logDb_sd, df_slice$logDb_sd[!is.na(df_slice$logDb_sd)])
}
