#' Get netcdf data from files
#'
#' Retrieve geoBAM input variables and replace fill values with NA.
#'
#' @param reachid string reach identifier
#' @param data_dir directory that contains SWOT and SoS data
#'
#' @return list matrix of reach data (both valid and invalid)
#'
#' @export
get_input_data <- function(reachid, data_dir) {

  # SWOT and SoS files
  swot_file <- file.path(data_dir, "swot", paste0(reachid, "_SWOT.nc"))
  sos_file <- file.path(data_dir, "sos", paste0(reachid, "_SOS.nc"))

  # Open files for reading and get data
  swot_input <- ncdf4::nc_open(swot_file)
  sos_input <- ncdf4::nc_open(sos_file)

  # Track nx and nt
  nx <- ncdf4::ncvar_get(swot_input, "nx")
  if (length(nx) > 10) nx = nx[1:10]
  #nt <- ncdf4::ncvar_get(swot_input, "nt")
  #nt <- ncdf4::ncvar_get(swot_input, "nt")[1:100]
  nt <- ncdf4::ncvar_get(swot_input, "nt")[1:324]

  # Check global attribute for reach validity and return empty list if invalid
  valid <- ncdf4::ncatt_get(sos_input, 0)$valid[[1]]
  if (valid == 0) { return(list(valid = FALSE, reachid = reachid, nx = nx,
                                     nt = nt)) }

  # width
  width <- ncdf4::ncvar_get(swot_input, "node/width")
  width_fill <- ncdf4::ncatt_get(swot_input, "node/width", "_FillValue")
  width[width == width_fill$value] <- NA
  width = t(width)
  if (length(nx) == 10) width = width[1:10, 4000:4323]

  #d_x_area ?? set 0 values to NA
  d_x_area <- ncdf4::ncvar_get(swot_input, "node/d_x_area")
  da_fill <- ncdf4::ncatt_get(swot_input, "node/d_x_area", "_FillValue")
  d_x_area[d_x_area == da_fill$value] <- NA
  d_x_area = t(d_x_area)
  if (length(nx) == 10) d_x_area = d_x_area[1:10, 4000:4323]

  # slope2
  slope2 <- ncdf4::ncvar_get(swot_input, "node/slope2")
  slope_fill <- ncdf4::ncatt_get(swot_input, "node/slope2", "_FillValue")
  slope2[slope2 == slope_fill$value] <- NA
  slope2 = t(slope2)
  if (length(nx) == 10) slope2 = slope2[1:10, 4000:4323]

  # Qhat
  qhat <- ncdf4::ncvar_get(sos_input, "reach/Qhat")
  qhat_fill <- ncdf4::ncatt_get(sos_input, "reach/Qhat", "_FillValue")
  qhat[qhat == qhat_fill$value] <- NA
  qhat = rep(qhat, times = length(nt))

  # Check validity of observation data
  obs_data <- check_observations(width, d_x_area, slope2, qhat)
  if (length(obs_data) == 0) { return(list(valid = FALSE,
                                                reachid = reachid, nx = nx,
                                                nt = nt)) }

  # Create a list of data with reach identifier
  return(list(valid = TRUE, reachid = reachid, nx = nx, nt = nt,
                   width = obs_data$width, slope2 = obs_data$slope2,
                   d_x_area = obs_data$d_x_area, qhat = obs_data$qhat,
                   invalid_nodes = obs_data$invalid_nodes,
                   invalid_time = obs_data$invalid_time))

}

#' Checks if observation data is valid.
#'
#' @param width matrix
#' @param d_x_area matrix
#' @param slope2 matrix
#' @param qhat vector
#'
#' @return list of valid observations or an empty list if there are none
#'
#' @export
check_observations <- function(width, d_x_area, slope2, qhat) {
  # Test for negative data
  width[width < 0] <- NA
  slope2[slope2 < 0] <- NA
  qhat[qhat < 0] <- NA

  # Qhat
  if (is.na(qhat[[1]])) { return(vector(mode = "list")) }

  # Get invalid width indexes and remove from observation data; test if enough data is present
  invalid_width <- get_invalid(width)
  width <- width[!invalid_width$invalid_nodes, !invalid_width$invalid_time]
  d_x_area <- d_x_area[!invalid_width$invalid_nodes, !invalid_width$invalid_time]
  slope2 <- slope2[!invalid_width$invalid_nodes, !invalid_width$invalid_time]
  if (is.null(dim(width)) || nrow(width) < 5 || ncol(width) < 5 ) { return(vector(mode = "list")) }

  # Get invalid d_x_area indexes and remove from observation data; test if enough data is present
  invalid_d_x_area <- get_invalid(d_x_area)
  d_x_area <- d_x_area[!invalid_d_x_area$invalid_nodes, !invalid_d_x_area$invalid_time]
  width <- width[!invalid_d_x_area$invalid_nodes, !invalid_d_x_area$invalid_time]
  slope2 <- slope2[!invalid_d_x_area$invalid_nodes, !invalid_d_x_area$invalid_time]
  if (is.null(dim(d_x_area)) || nrow(d_x_area) < 5 || ncol(d_x_area) < 5 ) { return(vector(mode = "list")) }

  # Get invalid slope2 indexes and remove from observation data; test if enough data is present
  invalid_slope2 <- get_invalid(slope2)
  slope2 <- slope2[!invalid_slope2$invalid_nodes, !invalid_slope2$invalid_time]
  d_x_area <- d_x_area[!invalid_slope2$invalid_nodes, !invalid_slope2$invalid_time]
  width <- width[!invalid_slope2$invalid_nodes, !invalid_slope2$invalid_time]
  if (is.null(dim(slope2)) || nrow(slope2) < 5 || ncol(slope2) < 5 ) { return(vector(mode = "list")) }

  # Concatenate lists
  invalid_width_n <- which(invalid_width$invalid_nodes == TRUE)
  invalid_d_x_area_n <- which(invalid_d_x_area$invalid_nodes == TRUE)
  invalid_slope2_n <- which(invalid_slope2$invalid_nodes == TRUE)
  invalid_nodes <- unique(c(invalid_width_n, invalid_d_x_area_n, invalid_slope2_n))

  invalid_width_t <- which(invalid_width$invalid_time == TRUE)
  invalid_d_x_area_t <- which(invalid_d_x_area$invalid_time == TRUE)
  invalid_slope2_t <- which(invalid_slope2$invalid_time == TRUE)
  invalid_time <- unique(c(invalid_width_t, invalid_d_x_area_t, invalid_slope2_t))

  # Return list of valid observation data
  return(list(width = width, d_x_area = d_x_area, slope2 = slope2, qhat = qhat, invalid_nodes = invalid_nodes, invalid_time = invalid_time))

}

#' Checks if observation parameter has valid nx (nodes) and valid nt (time steps)
#'
#' @param obs matrix
#'
#' @return list of invalid nodes and invalid time steps
#'
#' @export
get_invalid <- function(obs) {

  # Determine invalid nx and nt for obs
  invalid_nodes = rowSums(is.na(obs)) >= (ncol(obs) - 5)
  invalid_time = colSums(is.na(obs)) >= (nrow(obs) - 5)
  return(list(invalid_nodes = invalid_nodes, invalid_time = invalid_time))

}
