# Function: sp2gg.R
# By: Sabine Loos
# Latest Update: 07.25.2019

# DESCRIPTION: sp2gg takes a SpatialPolygonsdataframe (sp) and transforms it into 
#              a dataframe that ggplot recognizes so it can be mapped as as a geom_polygon

# INPUTS: 
## shp = SpatialPolygonsDataFrame that you want transferred 

# OUTPUTS: 
## shp.df = a dataframe indicating the locations of each element of a polygon
##          so that ggplot can plot it as a geom_polygon


# sp2gg -------------------------------------------------------------------
sp2gg <- function(shp){
  require(ggplot2)
  shp$id = rownames(shp@data)
  shp.pts <- fortify(shp, region = "id")
  shp.df <- merge(shp.pts, shp@data, by = "id", type = "left")
  return(shp.df)
}
