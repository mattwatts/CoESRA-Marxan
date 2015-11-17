library(marxan)

base_outputdir <- "/Users/matt/Documents/CoESRA/output"

pu_layer_file <- paste0(base_outputdir,"/pulayer.shp")

CreateBoundDat <- function(sPuLayer,sOutdir)
{
  # prepare output directory
  dir.create(paste0(sOutdir,"/marxan"))
  sBoundDat <- paste0(sOutdir,"/marxan/bound.dat")
  
  # read the planning units
  sf <- shapefile(sPuLayer)
  
  # create the boundary length file
  blf <- calcBoundaryData(sf)
  write.table(blf, sBoundDat,row.names=F,sep=",")
  
  return(sBoundDat)
}

bound_dat_file <- CreateBoundDat(pu_layer_file,base_outputdir)

