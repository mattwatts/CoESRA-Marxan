
library(raster)
library(maptools)
library(rgeos)
library(rgdal)

base_inputdir <- "/Users/matt/Documents/CoESRA/input_data"
base_outputdir <- "/Users/matt/Documents/CoESRA/output"

CreateCostLayer <- function(sRaster, sPoints, iGranularity, sOutdir)
{
  # read the raster whose snap is the domain of the study area
  r.outline <- raster(sRaster)
  
  # make a new pugrid raster
  r.pugrid <- raster(ncol=iGranularity,nrow=iGranularity)
  projection(r.pugrid) <- projection(r.outline)
  extent(r.pugrid) <- extent(r.outline)
  
  # read latitude,longitude for the boat ramps
  boat_ramps <- read.csv(sPoints)
  xy <- boat_ramps[,3:2]

  # make the cost raster
  sOutputRaster <- paste0(sOutdir,"/Cost.tif")
  writeRaster((1 - distanceFromPoints(r.pugrid, xy)),filename=sOutputRaster,overwrite=TRUE)
  return(sOutputRaster)
}

outfile <- CreateCostLayer(paste0(base_inputdir,"/GIS/Wenlock.tif"),
                           paste0(base_inputdir,"/GIS/boat_ramps.csv"),
                           100,
                           base_outputdir)


