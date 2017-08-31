#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  Points Over Polygons using Rgdal library                               |
#   |                                                                         |
#   |  By:                                                                    |
#   |  Yifang Zhang                                                           |                            
#   |  University of Illinois at Urbana Chamapaign                            |
#   |                                                                         |
#     ------------------------------------------------------------------------

## state GA is not avaliable

##################################################################
## Preliminaries
#library(parallel)

#num_cores <- detectCores() - 1
#cl <- makeCluster(num_cores)

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, repos = "http://cran.us.r-project.org", dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("readxl", "data.table", "rgdal", "sp", "rgeos", "tools")
lapply(packages, pkgTest)

## reading the points file
points <- points_raw[which(!is.na(points_raw$lat)),]
points <- points[which(!is.na(points$lon)),]

## reading state shapefile 
#shapefile_path_full = path.expand(shapefile_path)
#ogrInfo(dsn = shapefile_path_full, layer = layer_name)
#shp_poly <- readOGR(shapefile_path_full, layer_name)

## adding the indexes in front of precincts 
#shp_poly$addedID <- seq.int(nrow(shp_poly))
shp_poly = readRDS(shape_object_path)

## performing the projection for points
coordinates(points) <- ~ lon + lat
proj4string(points) <- CRS("+proj=longlat")

## if the shapefile does not have projection, we will defaulting it to WGS 84
if(is.na(proj4string(shp_poly))){
  proj4string(shp_poly) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
}

## transform the projection from points to the projection of the shapefile
points <- spTransform(points, proj4string(shp_poly))
proj4string(points) <- proj4string(shp_poly)

## perform the over function
execution_start_time <- proc.time()
res <- over(points, shp_poly)
print(proc.time() - execution_start_time)

#execution_start_time <- proc.time()
#chunks = split(points, (seq(nrow(points))-1) %/% 1500)
#processed_chunks = parLapply(cl, chunks, over, y=shp_poly)
#res = Reduce(rbind, processed_chunks, processed_chunks[[1]][c(),])
#print(proc.time() - execution_start_time)

## optional: plotting the data in R
#plot(shp_poly)
#plot(points_res$PropertyAddressLatitude ~ points_res$PropertyAddressLongitude, col = "red", cex = 1)

## Appending the result information after the Hedonics Data
points_res <- points_raw
points_res <- points_res[which(!is.na(points_res$lat)),]
points_res <- points_res[which(!is.na(points_res$lon)),]

aug_points <- cbind(points_res, res)



#saveRDS(aug_points, paste0(shape_object_path,"_res.rds"))


#######################
## for running on Roger
#It works for me if I use a mirror that has the package for our version 
#(I use OH 1, #135 for me, but that can change for other users). 
#You also need to:
#	module load R
#	module load gdal-stack
#	install.packages("rgdal", configure.args=c(rgdal = "proj_include_path=/sw/geosoft/proj4/include 
#					proj_lib_path=/sw/geosoft/proj4/lib --with-proj-share=/sw/geosoft/proj4/share/proj"))

#temp <- read.csv("~/share/projects/VotingBehavior/production/MN_matching.csv")
#saveRDS(temp, "~/share/projects/VotingBehavior/production/MN_matching.rds")
