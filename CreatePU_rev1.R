# Author: Matt Watts
# Date: 12 March 2015
# Purpose: Create a planning unit layer for Marxan. This R code is to be abstracted and embedded in a Kepler component.
# Data: Study area outline as a polygon
#       Animal trajectory lines as a polygon
#       Granularity

library(raster)
library(maptools)
library(rgeos)
library(rgdal)
library(tools)

base_dir <- "/Users/matt/Documents/CoESRA"
base_inputdir <- "/Users/matt/Documents/CoESRA/input_data"
base_outputdir <- "/Users/matt/Documents/CoESRA/output"

#location of all detection animal dirs
trajectorypaths <- paste0(base_dir,"/wml_output/TrajectoryPaths")
#studyarea_file <- paste0(base_outputdir,"/studyarea.shp")
studyarea_file <- paste0(base_outputdir,"#studyarea")
gridsize <- 50

CreatePU <- function(sTrajectoryPath,sStudyArea,iGridSize,sOutdir)
{
  # read the vector which is the outline of the study area
  mylist <- strsplit(studyarea_file, "#")[[1]]
  lpath = mylist[1]
  lfile = mylist[2]
  v.outline <- readOGR(lpath,lfile)
  iGranularity <- gridsize

  # create vector grid
  r.pugrid <- raster(ncol=iGranularity,nrow=iGranularity)
  projection(r.pugrid) <- projection(v.outline)
  extent(r.pugrid) <- extent(v.outline)
  v.pugrid <- rasterToPolygons(r.pugrid)

  # clip the vector grid
  v.clip <- gIntersection(v.pugrid,v.outline,byid=TRUE)

  # recursively loop through trajectory shapes in a given subdirectory
  tshapes <- list.files(path = sTrajectoryPath, recursive = TRUE, pattern = "*.shp", ignore.case = TRUE, full.names = TRUE)
  x <- FALSE
  for (j in 1:(iGranularity*iGranularity))
  {
    x[j] <- FALSE
  }
  for (i in 1:length(tshapes))
  {
    v.trajectory <- readOGR(dirname(tshapes[i]),file_path_sans_ext(basename(tshapes[i])))
    # select subset of v.clip that intersects v.trajectory
    for (j in 1:(iGranularity*iGranularity))
    {
      if (x[j] == FALSE)
      {
        x[j] <- gIntersects(v.clip[j],v.trajectory)
      }
    }
  }

  v.pulayer <- v.clip[x]

  # create the planning unit ID's as one-based integers
  row.names(v.pulayer) <- as.character(rep(1:length(row.names(v.pulayer))))
  PUID <- as.integer(sapply(slot(v.pulayer, "polygons"), function(x) slot(x, "ID")))
  PUID <- as.data.frame(PUID)
  v.pulayer_shp <- SpatialPolygonsDataFrame(v.pulayer,data=PUID)
  
  # write the planning unit layer to disk
  writeOGR(v.pulayer_shp,sOutdir,"pulayer",driver="ESRI Shapefile",overwrite_layer=TRUE)

  return(paste0(sOutdir, "/pulayer.shp"))
}

outfile <- CreatePU(trajectorypaths,studyarea_file,gridsize,base_outputdir)
