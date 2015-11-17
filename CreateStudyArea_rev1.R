
library(raster)
library(maptools)
library(rgeos)
library(rgdal)

base_inputdir <- "/Users/matt/Documents/CoESRA/input_data"
base_outputdir <- "/Users/matt/Documents/CoESRA/output"

CreateStudyArea <- function(sRaster, sOutdir)
{
  # read the vector which is the outline of the study area
  r.outline <- raster(raster_file)

  # create vector grid
  r.studyarea <- raster(ncol=1,nrow=1)
  projection(r.studyarea) <- projection(r.outline)
  extent(r.studyarea) <- extent(r.outline)
  v.studyarea <- rasterToPolygons(r.studyarea)

  # write the study area layer to disk
  writeOGR(v.studyarea,outdir,"studyarea",driver="ESRI Shapefile",overwrite=TRUE)
  return(paste0(outdir, "/studyarea.shp"))
}

study_shp_file <- CreateStudyArea(paste0(base_inputdir,"/GIS/Wenlock.tif"),
                                  base_outputdir)

