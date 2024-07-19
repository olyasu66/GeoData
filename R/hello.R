# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Calculate Distance Between Two Geographical Points
#'
#' This function calculates the distance between two geographical points specified by their longitude and latitude.
#' The result is given in kilometers.
#'
#' @param lon1 Longitude of the first point.
#' @param lat1 Latitude of the first point.
#' @param lon2 Longitude of the second point.
#' @param lat2 Latitude of the second point.
#' @return The distance between the two points in kilometers.
#' @examples
#' calculate_distance(-74.0060, 40.7128, -118.2437, 34.0522)
calculate_distance <- function(lon1, lat1, lon2, lat2) {
  # Radius of the Earth in kilometers
  R <- 6371

  # Convert degrees to radians
  lon1 <- deg2rad(lon1)
  lat1 <- deg2rad(lat1)
  lon2 <- deg2rad(lon2)
  lat2 <- deg2rad(lat2)

  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  # Distance in kilometers
  distance <- R * c
  return(distance)
}

#' Calculate Midpoint Between Two Geographical Points
#'
#' This function calculates the midpoint between two geographical points specified by their longitude and latitude.
#'
#' @param lon1 Longitude of the first point.
#' @param lat1 Latitude of the first point.
#' @param lon2 Longitude of the second point.
#' @param lat2 Latitude of the second point.
#' @return A named vector with the longitude and latitude of the midpoint.
#' @examples
#' calculate_midpoint(-74.0060, 40.7128, -118.2437, 34.0522)
calculate_midpoint <- function(lon1, lat1, lon2, lat2) {
  # Convert degrees to radians
  lon1 <- deg2rad(lon1)
  lat1 <- deg2rad(lat1)
  lon2 <- deg2rad(lon2)
  lat2 <- deg2rad(lat2)

  # Calculate the midpoint
  dlon <- lon2 - lon1
  Bx <- cos(lat2) * cos(dlon)
  By <- cos(lat2) * sin(dlon)
  lat_mid <- atan2(sin(lat1) + sin(lat2), sqrt((cos(lat1) + Bx)^2 + By^2))
  lon_mid <- lon1 + atan2(By, cos(lat1) + Bx)

  # Convert radians to degrees
  lon_mid <- rad2deg(lon_mid)
  lat_mid <- rad2deg(lat_mid)

  return(c(lon = lon_mid, lat = lat_mid))
}

# Utility functions to convert degrees to radians and vice versa
deg2rad <- function(deg) {
  return(deg * pi / 180)
}

rad2deg <- function(rad) {
  return(rad * 180 / pi)
}

calculate_distance(-74.0060, 40.7128, -118.2437, 34.0522)
calculate_midpoint(-74.0060, 40.7128, -118.2437, 34.0522)
