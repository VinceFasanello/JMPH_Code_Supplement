# clear workspace and load all required packages -------------------------------
rm(list=ls())
require(rgdal)
require(raster)
require(dplyr)
require(movecost)
require(sp)
require(raster)
require(gdistance)
wdscripts <- "/Users/boterolab1/Box Sync/JMPH_2020/scripts/Batch_Run_THERMAL"
wddata <-  "/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run_THERMAL"

# get sources and define functions  ------------------------------------------------------------------
setwd(wdscripts); source("THERMAL_SOURCE.R")

# Load and handle the PAM ------------------------------------------------------
setwd(wddata); load(file="BirdPAM.rdata")
rasterpam <- myPAM$Richness_Raster; save(rasterpam, file = "rasterpam.rdata") # get the richness raster and save it raster
pamMAT <- myPAM$Presence_and_Absence_Matrix; rm(myPAM) # get the pressence absence matrix


# Load and handle the TEMPERATURE data -------------------------------------------
setwd(wddata)
load(file = "tas_rasters_VJF.rdata")
load(file = "tasmin_rasters_VJF.rdata") # no scaling on these b/c we are doing calculations across multiple rasters. 
load(file = "tasmax_rasters_VJF.rdata")
load(file = "tasrng_rasters_VJF.rdata")


# Load and handle the Elevation data -------------------------------------------
setwd(wddata); load("elevRaster_use.rdata") # we want elev data for our starting points. 
elev_raster <- projectRaster(from = Elev_raster, to = rasterpam); rm(Elev_raster) # project elevation as raster pam - so coordinates match


# Load and handle the pairs data -----------------------------------------------
# cooney dataset ---------------------------------
setwd(wddata); cooneyd <- read.csv("Cooney_Pair_Data.csv", stringsAsFactors = F)
cooneyd <- cooneyd[cooneyd$Tree == "mcc",]; cooneyd <- cooneyd[,which(colnames(cooneyd) != "Tree")] # just use the mcc tree
cooneyd$pairID <- seq(1, nrow(cooneyd)) # set pair ids
cooneydi <- cooneyd; cooneydi$Species.1 <- cooneyd$Species.2; cooneydi$Species.2 <- cooneyd$Species.1; cooneyd <- rbind(cooneyd, cooneydi); rm(cooneydi) # convert from 1 row per pair to 1 row per species to append species specific data below.

# name matching ----------------------------------
setwd(wddata); load(file="Matched_Names.Rdata")
matchednames <- matchednames[matchednames$dup == F,] # remove entries where two cooney species map to the same latin name / names
matchednames <- matchednames[is.na(matchednames$a_BL_latin_2),] # remove entries where there are two names in birdlife (latin names) for a single cooney name
cooneyd <- cooneyd[cooneyd$Species.1 %in% matchednames$cooney_name & cooneyd$Species.2 %in% matchednames$cooney_name,] # subset cooneyd to only those entries where both names in pair are in the clean dataset
cooneyd$Species.1bl <- NA; cooneyd$Species.2bl <- NA # add columns to house the new bl names for the cooneyd species 1, and cooneyd species 2 on each row
for (i in 1:nrow(cooneyd)) { # populate the new names columns with the bl names that match the pam
  cooneyd$Species.1bl[i] <- matchednames$a_BL_latin_1[matchednames$cooney_name == cooneyd$Species.1[i]]
  cooneyd$Species.2bl[i] <- matchednames$a_BL_latin_1[matchednames$cooney_name == cooneyd$Species.2[i]]
}
cooneyd[,"Species.1bl"] <- gsub("_", " ", cooneyd[,"Species.1bl"]) # replace underscores with spaces for the bl names that match the pam
cooneyd[,"Species.2bl"] <- gsub("_", " ", cooneyd[,"Species.2bl"])
cooneyd <- cooneyd[cooneyd$Species.1bl %in% colnames(pamMAT) & cooneyd$Species.2bl %in% colnames(pamMAT),] # subset cooneyd to only those entries with name matches in pamMAT
rm(matchednames, i)



cooneyd$n_pam_cells_sp1 <- NA; cooneyd$n_pam_cells_sp2 <- NA

cooneyd$min_ele_sp1 <- NA; cooneyd$max_ele_sp1 <- NA; cooneyd$ele_range_sp1 <- NA
cooneyd$min_ele_sp2 <- NA; cooneyd$max_ele_sp2 <- NA; cooneyd$ele_range_sp2 <- NA
cooneyd$ele_maxofmins <- NA; cooneyd$ele_minofmaxs <- NA; cooneyd$ele_ov_range <- NA; cooneyd$ele_ov_perc_smrnge <- NA

cooneyd$min_lat_sp1 <- NA; cooneyd$max_lat_sp1 <- NA; cooneyd$lat_range_sp1 <- NA
cooneyd$min_lat_sp2 <- NA; cooneyd$max_lat_sp2 <- NA; cooneyd$lat_range_sp2 <- NA
cooneyd$lat_maxofmins <- NA; cooneyd$lat_minofmaxs <- NA; cooneyd$lat_ov_range <- NA; cooneyd$lat_ov_perc_smrnge <- NA

cooneyd[, c("tasmax_sp1_1", "tasmax_sp1_2", "tasmax_sp1_3", "tasmax_sp1_4", "tasmax_sp1_5", "tasmax_sp1_6", "tasmax_sp1_7", "tasmax_sp1_8", "tasmax_sp1_9", "tasmax_sp1_10", "tasmax_sp1_11", "tasmax_sp1_12")] <- NA
cooneyd[, c("tasmin_sp1_1", "tasmin_sp1_2", "tasmin_sp1_3", "tasmin_sp1_4", "tasmin_sp1_5", "tasmin_sp1_6", "tasmin_sp1_7", "tasmin_sp1_8", "tasmin_sp1_9", "tasmin_sp1_10", "tasmin_sp1_11", "tasmin_sp1_12")] <- NA
cooneyd[, c("tasrng_sp1_1", "tasrng_sp1_2", "tasrng_sp1_3", "tasrng_sp1_4", "tasrng_sp1_5", "tasrng_sp1_6", "tasrng_sp1_7", "tasrng_sp1_8", "tasrng_sp1_9", "tasrng_sp1_10", "tasrng_sp1_11", "tasrng_sp1_12")] <- NA
cooneyd[, c("tasmax_sp2_1", "tasmax_sp2_2", "tasmax_sp2_3", "tasmax_sp2_4", "tasmax_sp2_5", "tasmax_sp2_6", "tasmax_sp2_7", "tasmax_sp2_8", "tasmax_sp2_9", "tasmax_sp2_10", "tasmax_sp2_11", "tasmax_sp2_12")] <- NA
cooneyd[, c("tasmin_sp2_1", "tasmin_sp2_2", "tasmin_sp2_3", "tasmin_sp2_4", "tasmin_sp2_5", "tasmin_sp2_6", "tasmin_sp2_7", "tasmin_sp2_8", "tasmin_sp2_9", "tasmin_sp2_10", "tasmin_sp2_11", "tasmin_sp2_12")] <- NA
cooneyd[, c("tasrng_sp2_1", "tasrng_sp2_2", "tasrng_sp2_3", "tasrng_sp2_4", "tasrng_sp2_5", "tasrng_sp2_6", "tasrng_sp2_7", "tasrng_sp2_8", "tasrng_sp2_9", "tasrng_sp2_10", "tasrng_sp2_11", "tasrng_sp2_12")] <- NA
cooneyd[, c("tmp_v_1", "tmp_v_2", "tmp_v_3", "tmp_v_4", "tmp_v_5", "tmp_v_6", "tmp_v_7", "tmp_v_8", "tmp_v_9", "tmp_v_10", "tmp_v_11", "tmp_v_12")] <- NA
cooneyd$tmp_v_sum <- NA

# Calculate lat, ele, and overlap for all pairs --------------------------------
for (i in 1:nrow(cooneyd)) {
  mytemp <- pamMAT[,c("Longitude(x)", "Latitude(y)" ,paste0(cooneyd[i,"Species.1bl"]), paste0(cooneyd[i,"Species.2bl"]))] # subset pam
  range1<-subset(mytemp, mytemp[,3]==1) # get species ranges
  range2<-subset(mytemp, mytemp[,4]==1); rm(mytemp)
  
  coords1<-data.frame(lon=range1[,1], lat=range1[,2]); coordinates(coords1)<-c("lon","lat"); crs(coords1)<-crs(rasterpam) # get coordinates
  coords2<-data.frame(lon=range2[,1], lat=range2[,2]); coordinates(coords2)<-c("lon","lat"); crs(coords2)<-crs(rasterpam)
  
  # Elevation ----------------------------------------------
  elev_1<-as.data.frame(extract(x=elev_raster, y=coords1)); elev_1$sp <- "sp1"; colnames(elev_1) <- c("elev", "sp") # get dfs -----
  elev_2<-as.data.frame(extract(x=elev_raster, y=coords2)); elev_2$sp <- "sp2"; colnames(elev_2) <- c("elev", "sp")
  cooneyd$min_ele_sp1[i] <- min(elev_1$elev, na.rm = T); cooneyd$max_ele_sp1[i] <- max(elev_1$elev, na.rm = T) # max and min -----
  cooneyd$min_ele_sp2[i] <- min(elev_2$elev, na.rm = T); cooneyd$max_ele_sp2[i] <- max(elev_2$elev, na.rm = T)
  cooneyd$ele_range_sp1[i] <- abs(cooneyd$min_ele_sp1[i] - cooneyd$max_ele_sp1[i]) # range -----
  cooneyd$ele_range_sp2[i] <- abs(cooneyd$min_ele_sp2[i] - cooneyd$max_ele_sp2[i])
  cooneyd$ele_maxofmins[i] <- max(cooneyd$min_ele_sp1[i], cooneyd$min_ele_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooneyd$ele_minofmaxs[i] <- min(cooneyd$max_ele_sp1[i], cooneyd$max_ele_sp2[i], na.rm = T)
  cooneyd$ele_ov_range[i] <- cooneyd$ele_minofmaxs[i] - cooneyd$ele_maxofmins[i]
  cooneyd$ele_ov_perc_smrnge[i] <- cooneyd$ele_ov_range[i] / min(cooneyd$ele_range_sp1[i], cooneyd$ele_range_sp2[i], na.rm = T)
  
  # Latitude -----------------------------------------------
  lat_1<-as.data.frame(coords1$lat); lat_1$sp <- "sp1"; colnames(lat_1) <- c("latitude", "sp") # get dfs -----
  lat_2<-as.data.frame(coords2$lat); lat_2$sp <- "sp2"; colnames(lat_2) <- c("latitude", "sp")
  cooneyd$min_lat_sp1[i] <- min(lat_1$latitude, na.rm = T); cooneyd$max_lat_sp1[i] <- max(lat_1$latitude, na.rm = T) # max and min -----
  cooneyd$min_lat_sp2[i] <- min(lat_2$latitude, na.rm = T); cooneyd$max_lat_sp2[i] <- max(lat_2$latitude, na.rm = T)
  cooneyd$lat_range_sp1[i] <- abs(cooneyd$min_lat_sp1[i] - cooneyd$max_lat_sp1[i]) # range
  cooneyd$lat_range_sp2[i] <- abs(cooneyd$min_lat_sp2[i] - cooneyd$max_lat_sp2[i])
  cooneyd$lat_maxofmins[i] <- max(cooneyd$min_lat_sp1[i], cooneyd$min_lat_sp2[i], na.rm = T) # overlap % = Union range  / smaller range
  cooneyd$lat_minofmaxs[i] <- min(cooneyd$max_lat_sp1[i], cooneyd$max_lat_sp2[i], na.rm = T)
  cooneyd$lat_ov_range[i] <- cooneyd$lat_minofmaxs[i] - cooneyd$lat_maxofmins[i]
  cooneyd$lat_ov_perc_smrnge[i] <- cooneyd$lat_ov_range[i] / min(cooneyd$lat_range_sp1[i], cooneyd$lat_range_sp2[i], na.rm = T)
  
  # basic info --------------------------------------------
  cooneyd$n_pam_cells_sp1[i] <- length(!is.na(lat_1$latitude)); cooneyd$n_pam_cells_sp2[i] <- length(!is.na(lat_2$latitude)) # range size
  
  # temperature --------------------------------------------
  tasmax_1 <- colMeans(extract(x=tasmax, y = coords1), na.rm = T) # species 1
  tasmin_1 <- colMeans(extract(x=tasmin, y = coords1), na.rm = T)
  tasrng_1 <- tasmax_1 - tasmin_1
  cooneyd[i, c("tasmax_sp1_1", "tasmax_sp1_2", "tasmax_sp1_3", "tasmax_sp1_4", "tasmax_sp1_5", "tasmax_sp1_6", "tasmax_sp1_7", "tasmax_sp1_8", "tasmax_sp1_9", "tasmax_sp1_10", "tasmax_sp1_11", "tasmax_sp1_12")] <- tasmax_1
  cooneyd[i, c("tasmin_sp1_1", "tasmin_sp1_2", "tasmin_sp1_3", "tasmin_sp1_4", "tasmin_sp1_5", "tasmin_sp1_6", "tasmin_sp1_7", "tasmin_sp1_8", "tasmin_sp1_9", "tasmin_sp1_10", "tasmin_sp1_11", "tasmin_sp1_12")] <- tasmin_1
  cooneyd[i, c("tasrng_sp1_1", "tasrng_sp1_2", "tasrng_sp1_3", "tasrng_sp1_4", "tasrng_sp1_5", "tasrng_sp1_6", "tasrng_sp1_7", "tasrng_sp1_8", "tasrng_sp1_9", "tasrng_sp1_10", "tasrng_sp1_11", "tasrng_sp1_12")] <- tasrng_1
  
  
  tasmax_2 <- colMeans(extract(x=tasmax, y = coords2), na.rm = T) # species 2
  tasmin_2 <- colMeans(extract(x=tasmin, y = coords2), na.rm = T)
  tasrng_2 <- tasmax_2 - tasmin_2
  cooneyd[i, c("tasmax_sp2_1", "tasmax_sp2_2", "tasmax_sp2_3", "tasmax_sp2_4", "tasmax_sp2_5", "tasmax_sp2_6", "tasmax_sp2_7", "tasmax_sp2_8", "tasmax_sp2_9", "tasmax_sp2_10", "tasmax_sp2_11", "tasmax_sp2_12")] <- tasmax_2
  cooneyd[i, c("tasmin_sp2_1", "tasmin_sp2_2", "tasmin_sp2_3", "tasmin_sp2_4", "tasmin_sp2_5", "tasmin_sp2_6", "tasmin_sp2_7", "tasmin_sp2_8", "tasmin_sp2_9", "tasmin_sp2_10", "tasmin_sp2_11", "tasmin_sp2_12")] <- tasmin_2
  cooneyd[i, c("tasrng_sp2_1", "tasrng_sp2_2", "tasrng_sp2_3", "tasrng_sp2_4", "tasrng_sp2_5", "tasrng_sp2_6", "tasrng_sp2_7", "tasrng_sp2_8", "tasrng_sp2_9", "tasrng_sp2_10", "tasrng_sp2_11", "tasrng_sp2_12")] <- tasrng_2
  
  tmp_maxofmins <- pmax(tasmin_1, tasmin_2) # overlap
  tmp_minofmaxs <- pmin(tasmax_1, tasmax_2)
  tmp_o <- tmp_minofmaxs - tmp_maxofmins
  tmp_v <- tmp_o / sqrt(tasrng_1*tasrng_2)
  cooneyd[i, c("tmp_v_1", "tmp_v_2", "tmp_v_3", "tmp_v_4", "tmp_v_5", "tmp_v_6", "tmp_v_7", "tmp_v_8", "tmp_v_9", "tmp_v_10", "tmp_v_11", "tmp_v_12")] <- tmp_v
  cooneyd$tmp_v_sum[i] <- sum(tmp_v, na.rm = T)
  rm(tasmax_1, tasmax_2, tasmin_1, tasmin_2, tasrng_1, tasrng_2, tmp_maxofmins, tmp_minofmaxs, tmp_o, tmp_v, elev_1, elev_2, lat_1, lat_2, range1, range2, coords1, coords2)

  
}
cooneyd$sortcol <- rowMeans(cbind(cooneyd$n_pam_cells_sp1, cooneyd$n_pam_cells_sp2), na.rm = T) # calculate mean pair range size as the column to sort by
cooneyd <- cooneyd[order(cooneyd$sortcol, cooneyd$pairID),] # sort by mean pair range size, smallest first
cooneyd$sortorder <- seq(1, nrow(cooneyd)) # preserve the sort order
cooneyd_hold <- cooneyd # store unmanipulated frame before subsetting and further operations.
setwd(wddata); save(cooneyd, file = "cooneyd.Rdata"); save(cooneyd_hold, file = "cooneyd_hold.Rdata")

# subset to specific datasets --------------------------------------------------
# 1 criteria --------------------------
# cooneyd <- cooneyd_hold
# nrow(cooneyd[which(cooneyd$ele_ov_perc_smrnge > 0.5 & cooneyd$lat_ov_perc_smrnge > 0.5 & cooneyd$Range.overlap < 0.30),])
# cooneyd <- cooneyd[which(cooneyd$ele_ov_perc_smrnge > 0.5 & cooneyd$lat_ov_perc_smrnge > 0.5 & cooneyd$Range.overlap < 0.30),]
# mydata <- cooneyd
# setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); save(cooneyd, file = "cooneyd.Rdata")


# # 2 Criteria --------------------------
# nrow(cooneyd[which(cooneyd$ele_ov_perc_smrnge > 0.99 & cooneyd$lat_ov_perc_smrnge > 0.99 & cooneyd$Range.overlap < 0.05),])
# cooneyd <- cooneyd[which(cooneyd$ele_ov_perc_smrnge > 0.99 & cooneyd$lat_ov_perc_smrnge > 0.99 & cooneyd$Range.overlap < 0.05),]
# mydata <- cooneyd


# break things up for batching --------------------------------------------------------------
# Split into groups of approximately 50
nchunks <- ceiling(nrow(cooneyd) / 50)
mybreaks <- seq(0, nrow(cooneyd), length.out = nchunks + 1)
for (i in 1:length(mybreaks)) { # go to nearest even to keep pairs together
  mybreaks[i] <- FindNearestEven(floor(mybreaks[i]), ud = "d")
}
setwd(wddata)
save(mybreaks, file = "mybreaks.rdata")

# save the chunks
setwd(wddata)
pamMAT <- pamMAT[,c("Longitude(x)", "Latitude(y)", cooneyd$Species.1bl)]; save(pamMAT, file = "pamMAT.rdata")
for (i in 1:nchunks) {
  assign(paste0("pamMAT", i), pamMAT[,c("Longitude(x)", "Latitude(y)", cooneyd$Species.1bl[(mybreaks[i]+1):mybreaks[i+1]])]);
  save(list=paste0("pamMAT", i), file = paste0("pamMAT", i, '.rdata'))
  rm(list=paste0("pamMAT", i))
}
