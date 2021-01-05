#' Get netcdf data from file
#'
#' Retrieve geoBAM input variables and replace fill values with NA.
#'
#' @param file netcdf
#'
#' @return list matrix
#'
#' @export
get_input <- function(file) {

  # Open file for reading and get data
  input <- ncdf4::nc_open(file)

  # width ?? set width NAs to 1
  width <- ncdf4::ncvar_get(input, "swot_node/width")
  width_fill <- ncdf4::ncatt_get(input, "swot_node/width", "_FillValue")
  width[width == width_fill$value] <- NA
  width[width == 0] <- NA
  width[is.na(width)] <- 1

  #d_x_area
  d_x_area <- ncdf4::ncvar_get(input, "swot_node/d_x_area")
  da_fill <- ncdf4::ncatt_get(input, "swot_node/d_x_area", "_FillValue")
  d_x_area[d_x_area == da_fill$value] <- NA
  d_x_area[d_x_area == 0] <- NA

  # slope2 ?? 0 values to 0.0001
  slope2_array <- ncdf4::ncvar_get(input, "swot_reach/slope2")
  slope_fill <- ncdf4::ncatt_get(input, "swot_reach/slope2", "_FillValue")
  slope2_array[slope2_array == slope_fill$value] <- NA
  slope2 <- matrix(slope2_array, nrow = nrow(d_x_area), ncol = ncol(d_x_area), byrow = TRUE)
  slope2[slope2 == 0] <- 0.00001

  # Qhat ?? check that Qhat is not negative
  qhat <- ncdf4::ncvar_get(input, "sword/Qhat")
  qhat_fill <- ncdf4::ncatt_get(input, "sword/Qhat", "_FillValue")
  qhat[qhat == qhat_fill$value] <- NA
  qhat[qhat < 0] <- qhat * -1

  # Reach ID
  reachid = as.character(get_reachid(file))

  return(list(data_list = list(width = width, slope2 = slope2, d_x_area = d_x_area, qhat = qhat, reachid = reachid)))

}

#' Get Reach ID
#'
#' Extract reach id from file name.
#'
#' @param file netcdf file
#'
#' @return character vector
get_reachid <- function(file) {

  file_vec <- unlist(strsplit(file, split = "/"))
  file_name <- file_vec[length(file_vec)]
  return(strsplit(substring(file_name, first = 19), split = ".nc"))
}
