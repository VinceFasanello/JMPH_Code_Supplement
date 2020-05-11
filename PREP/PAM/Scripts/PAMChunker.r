# clear workspace and load all required packages -------------------------------
rm(list=ls())
require(rgdal)
require(raster)
require(dplyr)
require(sp)
wdin <-  "/Users/Angela/Desktop/other/BOX_15march/Rebuild_PAMs"
wdout <- "/Users/Angela/Desktop/other/BOX_15march/Rebuild_PAMs"

# Load the UNMANIPULATED BirdPAM file ------------------------------------------
setwd(wdin) 
load(file="BirdPAM.rdata")

# split off the raster and save separately (we want all the data for this raster file). -----
LonLat_BirdPAM_raster <- myPAM$Richness_Raster
setwd(wdout); save(LonLat_BirdPAM_raster, file = "LonLat_BirdPAM_raster.rdata")


# Now, work with the PAM itself ------------------------------------------------
PAM <- myPAM$Presence_and_Absence_Matrix # get just the PAM
myPAM <- NULL # get rid of the myPAM file, its huge.

nchunks <- 8 # set the number of chunks to produce
breaks <- seq(1, ncol(PAM), length.out = nchunks + 1) # assign breaks so we can grab and save those chunks.
breaks <- floor(breaks)

# build each chunk and save as an individually named PAM -----------------------
for (i in 1:nchunks) { # for each of our chunks....
  assign(paste0("LonLat_bPAM_", i), PAM[,c("Longitude(x)", "Latitude(y)", colnames(PAM)[(breaks[i]+1):breaks[i+1]])]) # get just the current chunk
  print(paste0("THIS IS i, ", i));print(paste0("Start_index = ", breaks[i]+1)); print(paste0("End_index = ", breaks[i+1])) # print some info to prove to yourself that this is working as intended
  setwd(wdout); save(list=paste0("LonLat_bPAM_", i), file = paste0("LonLat_bPAM_", i, '.rdata')) # save the current chunk
  rm(list=paste0("LonLat_bPAM_", i)) # clear up some memory space for the next chunk
}