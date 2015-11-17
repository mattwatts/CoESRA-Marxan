library(raster)
library(maptools)
library(rgeos)
library(tools)

base_dir <- "/Users/matt/Documents/CoESRA"
base_inputdir <- "/Users/matt/Documents/CoESRA/input_data"
base_outputdir <- "/Users/matt/Documents/CoESRA/output"

pu_layer_file <- paste0(base_outputdir,"/pulayer.shp")
cost_layer_file <- paste0(base_outputdir,"/Cost.tif")
nvis_layer_file <- paste0(base_dir,"/wml_output/NVIS.tif")
capad_layer_file <- paste0(base_dir,"/wml_output/CAPAD.tif")
trajectorypaths <- paste0(base_dir,"/wml_output/TrajectoryPaths")

IntersectPU <- function(sPuLayer,sCostLayer,sNvisLayer,sCapadLayer,sTrajectoryPaths,sOutdir)
{
  sRdataOutputDir <- paste0(sOutdir,"/Rdata/")
  dir.create(sRdataOutputDir)
  
  # read the planning units, VNIS, CAPAD, cost
  v.pulayer_shp <- readShapeSpatial(pu_layer_file)
  r.nvis <- raster(nvis_layer_file)
  r.capad <- raster(capad_layer_file)
  r.cost <- raster(cost_layer_file)
  
  # make raster stack with one raster for each veg type
  rastSTK=list()
  uniVEC=unique(r.nvis)
  for (i in seq_along(uniVEC)) {
    rastSTK[[i]]=Which(r.nvis==uniVEC[i])
  }
  rastSTK=stack(rastSTK)
  names(rastSTK)=paste0("VEG_",uniVEC)
  
  # intersect the nvis raster stack
  values.nvis=extract(rastSTK, v.pulayer_shp, sum, na.rm=TRUE)
  save(values.nvis,file=paste0(sRdataOutputDir,"values.nvis.Rdata"))
  
  # calculate cost of each pu
  values.cost <- extract(r.cost, v.pulayer_shp, fun=mean, na.rm=TRUE)
  save(values.cost,file=paste0(sRdataOutputDir,"values.cost.Rdata"))
  
  # calculate amount of reserve in each pu
  values.reserve <- extract(r.capad, v.pulayer_shp, sum, na.rm=TRUE)
  save(values.reserve,file=paste0(sRdataOutputDir,"values.reserve.Rdata"))
  
  # recursively loop through animal rasters in a given subdirectory
  rastermaps <- list.files(path = trajectorypaths, recursive = TRUE, pattern = "*_dur.tif", ignore.case = TRUE, full.names = TRUE)
  for (i in 1:length(rastermaps))
  {
    r.rastermap <- raster(rastermaps[i])
    r.rastermap[is.na(r.rastermap[])] <- 0 
    sName <- file_path_sans_ext(basename(rastermaps[i]))
    # calculate the amount of each animal in each pu
    values.rastermap <- extract(r.rastermap, v.pulayer_shp, sum, na.rm=TRUE)
    save(values.rastermap,file=paste0(sRdataOutputDir,"values.rastermap.",sName,".Rdata"))  
  }
  
  return(sRdataOutputDir)
}

outpath <- IntersectPU(pu_layer_file,
                       cost_layer_file,
                       nvis_layer_file,
                       capad_layer_file,
                       trajectorypaths,
                       base_outputdir)


