#' Get netcdf data from files
#'
#' Retrieve geoBAM input variables and replace fill values with NA.
#'
#' @param input_dir file directory
#'
#' @return list matrix
#'
#' @export
get_input_data <- function(input_dir) {

  # Get a list of SWOT and SWORD files
  swot_dir = file.path(input_dir, "swot")
  swot_list <- list.files(path = swot_dir, pattern = "*.nc", full.names = TRUE)
  sword_dir = file.path(input_dir, "sword")
  sword_list <- list.files(path = sword_dir, pattern = "*.nc", full.names = TRUE)

  # Setup cluster and register do parallel operator
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)

  # Obtain a list of reach data from swot and sword files
  swot_file <- NULL
  sword_file <- NULL
  reach_list <- foreach::foreach(swot_file = swot_list, sword_file = sword_list,
                                 .combine = 'c', .export = c("parse_reach_data"),
                                 .packages = c("ncdf4")) %dopar% parse_reach_data(swot_file, sword_file)

  # Close cluster connections
  parallel::stopCluster(cl)

  return(reach_list)

}

#' Get netcdf data from files
#'
#' Retrieve geoBAM input variables and replace fill values with NA.
#'
#' @param swot_file netcdf
#' @param sword_file netcdf
#'
#' @return list matrix
parse_reach_data <- function(swot_file, sword_file) {

  # Open files for reading and get data
  swot_input <- ncdf4::nc_open(swot_file)
  sword_input <- ncdf4::nc_open(sword_file)

  # width ?? set width NAs to 1
  width <- ncdf4::ncvar_get(swot_input, "swot_node/width")
  width_fill <- ncdf4::ncatt_get(swot_input, "swot_node/width", "_FillValue")
  width[width == width_fill$value] <- NA
  width[width == 0] <- NA
  width[is.na(width)] <- 1

  #d_x_area
  d_x_area <- ncdf4::ncvar_get(swot_input, "swot_node/d_x_area")
  da_fill <- ncdf4::ncatt_get(swot_input, "swot_node/d_x_area", "_FillValue")
  d_x_area[d_x_area == da_fill$value] <- NA
  d_x_area[d_x_area == 0] <- NA

  # slope2 ?? 0 values to 0.0001
  slope2_array <- ncdf4::ncvar_get(swot_input, "swot_reach/slope2")
  slope_fill <- ncdf4::ncatt_get(swot_input, "swot_reach/slope2", "_FillValue")
  slope2_array[slope2_array == slope_fill$value] <- NA
  slope2 <- matrix(slope2_array, nrow = nrow(d_x_area), ncol = ncol(d_x_area), byrow = TRUE)
  slope2[slope2 == 0] <- 0.00001

  # Qhat ?? check that Qhat is not negative
  qhat <- ncdf4::ncvar_get(sword_input, "Qhat")
  qhat_fill <- ncdf4::ncatt_get(sword_input, "Qhat", "_FillValue")
  qhat[qhat == qhat_fill$value] <- NA
  qhat[qhat < 0] <- qhat * -1

  # Reach ID
  file_vec <- unlist(strsplit(swot_file, split = "/"))
  file_name <- strsplit(file_vec[length(file_vec)], split = ".nc")
  name <- strsplit(as.character(file_name[1]), split = "_")
  reachid = paste0(name[[1]][2], "_", name[[1]][3])

  return(list(data_list = list(width = width, slope2 = slope2, d_x_area = d_x_area, qhat = qhat, reachid = reachid)))

}
