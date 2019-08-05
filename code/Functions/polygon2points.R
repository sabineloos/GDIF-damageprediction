# Function: polygon2points.R
# By: Sabine Loos
# Latest Update: 07.31.2019

# DESCRIPTION: Function used to turn SpatialPolygonsDataFrame (or SpatialPixelsDataFrame) into SpatialPointsDataFrame

# FUNCTIONS: 
## nscore - normal score transformation function

# INPUTS:
## poly_spdf = SpatialPolygonsDataFrame to be converted into points

# OUTPUTS: 
## point_spdf = SpatialPointsDataFrame with points located at centroid of polygons

# polygon2points ------------------------------------------------------------------
polygon2points <- function(poly_spdf){
  point_spdf <- SpatialPointsDataFrame(coordinates(poly_spdf), data = poly_spdf@data, proj4string = CRS(proj4string(poly_spdf)))
  return(point_spdf)
}