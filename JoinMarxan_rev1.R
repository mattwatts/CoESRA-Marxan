library(raster)
library(maptools)
library(rgeos)
library(tools)

base_outputdir <- "/Users/matt/Documents/CoESRA/output"

pu_layer_file <- paste0(base_outputdir,"/pulayer.shp")
r_datadir <- paste0(base_outputdir,"/Rdata/")

JoinMarxan <- function(sPuLayer,sRdataDir,sOutdir)
{
  # prepare output directory
  dir.create(paste0(sOutdir,"/marxan"))
  sBoundDat <- paste0(sOutdir,"/marxan/bound.dat")
  
  # read the planning unit layer
  v.pulayer_shp <- readShapeSpatial(sPuLayer)
  
  # load tables for nvis, cost, reserve
  load(paste0(sRdataDir,"values.nvis.Rdata"))
  load(paste0(sRdataDir,"values.cost.Rdata"))
  load(paste0(sRdataDir,"values.reserve.Rdata"))
  
  # find tables for rastermaps
  rastertables <- list.files(path = sRdataDir, recursive = TRUE, pattern = "*_dur.Rdata", ignore.case = TRUE, full.names = TRUE)
  # generate animal names
  animalnames <- c()
  for (i in 1:length(rastertables))
  {
    load(rastertables[i])
    sFileName <- file_path_sans_ext(basename(rastertables[i]))
    iRastermap <- regexpr("rastermap.",sFileName)
    iMovedur <- regexpr("_move_dur",sFileName)
    sName <- substr(sFileName,iRastermap[1]+10,iMovedur[1]-1)
    animalnames <- c(animalnames,sName)
  }
  
  # clean up the NA's in the values which are artifacts from geoprocessing
  for (i in 1:length(v.pulayer_shp$PUID))
  {
    for (j in 1:dim(values.nvis)[2])
    {
      if (is.na(values.nvis[i,j]))
      {
        values.nvis[i,j] <- 0  
      }    
    }
    if (is.na(values.cost[i]))
    {
      values.cost[i] <- 0.0001 # make the smallest cost a bit larger than zero
    }
    if (is.na(values.reserve[i]))
    {
      values.reserve[i] <- 0  
    }
  }
  
  # compute pu status
  pu.status <- values.reserve
  reserved <- max(values.reserve) * 2
  for (i in 1:length(v.pulayer_shp$PUID))
  {
    if (values.reserve[i] > reserved)
    {
      pu.status[i] <- 2
    } else {
      pu.status[i] <- 0
    }
  }
  
  # generate pu.dat
  pu_dat <- paste0(sOutdir,"/marxan/pu.dat")
  write('id,cost,status',file=pu_dat)
  for (i in 1:length(v.pulayer_shp$PUID))
  {
    write(paste(v.pulayer_shp$PUID[i],values.cost[i],pu.status[i],sep=","),
          file=pu_dat,append=TRUE)
  }
  
  # generate spec.dat
  spec_dat <- paste0(sOutdir,"/marxan/spec.dat")
  write('id,prop,spf,name',file=spec_dat)
  # veg types are first
  for (i in 1:ncol(values.nvis))
  {
    write(paste(i,0.3,1,colnames(values.nvis)[i],sep=","),
          file=spec_dat,append=TRUE)
  }
  # animals are second
  for (i in 1:length(animalnames))
  {
    write(paste((i+ncol(values.nvis)),0.3,1,paste0("ANIMAL_",animalnames[i]),sep=","),
          file=spec_dat,append=TRUE)
  }
  
  # generate sporder.dat
  sporder_dat <- paste0(sOutdir,"/marxan/sporder.dat")
  write('species,pu,amount',sporder_dat)
  # veg types are first
  for (j in 1:ncol(values.nvis))
  {
    for (i in 1:length(v.pulayer_shp$PUID))
    {
      if (values.nvis[i,j] > 0)
      {
        write(paste(j,v.pulayer_shp$PUID[i],values.nvis[i,j],sep=","),
              sporder_dat,append=TRUE)
      }
    }
  }
  # animals are second
  for (j in 1:length(rastertables))
  {
    load(rastertables[j])
    for (i in 1:length(v.pulayer_shp$PUID))
    {
      # clean up the NA's in the values which are artifacts from geoprocessing
      if (is.na(values.rastermap[i]))
      {
        values.rastermap[i] <- 0
      }
      # write non-zero values to the file
      if (values.rastermap[i] > 0)
      {
        write(paste((j+ncol(values.nvis)),v.pulayer_shp$PUID[i],values.rastermap[i],sep=","),
              sporder_dat,append=TRUE)
      }
    }
  }
  
  # generate puorder.dat
  puvsp_dat <- paste0(sOutdir,"/marxan/puorder.dat")
  spordertable <- read.csv(sporder_dat)
  puordertable <- spordertable[order(spordertable$pu),]
  write.csv(puordertable,puvsp_dat,quote=FALSE,row.names=FALSE)
}

JoinMarxan(pu_layer_file,r_datadir,base_outputdir)



