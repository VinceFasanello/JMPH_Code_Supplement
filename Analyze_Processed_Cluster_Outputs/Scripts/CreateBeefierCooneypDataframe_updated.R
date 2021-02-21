# clear workspace and load all required packages -------------------------------
rm(list=ls()); gc(); memory.size();memory.limit()
require(rgdal)
require(dplyr)
require(movecost)
require(sp)
require(raster)
require(gdistance)
require(rgeos)

# directory paths --------------------------------------------------------------
wdtopo <- "/Users/boterolab1/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/Topo/Data"
wdclimate <- "/Users/boterolab1/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/Climate/Data"
wdPAM <- "/Users/boterolab1/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/PAM/Data"
wdSpeciesNames <- "/Users/boterolab1/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/SpeciesNames/Data"
wdMainDataFrame <- "/Users/boterolab1/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/MainDataFrame/Data"

# helper -----------------------------------------------------------------------
FindNearestEven <- function(x, ud){ # this function finds the nearest even integer, rounding up or down as specified
  if(ud == "d"){
    if(x %% 2 != 0){x <- x - 1}
  } else if(ud == "u"){
    if(x %% 2 != 0){x <- x + 1}
  }
  return(x)
}


# PAM actual cbPAM file --------------------------------------------------------
setwd(wdPAM); load("cbPAM.rdata")


# PAM diversity raster data ----------------------------------------------------
# !!! This is the holy grail -- format everything to match this files crs !!!
setwd(wdPAM); load("LonLat_BirdPAM_raster.rdata")


# Topo data --------------------------------------------------------------------
setwd(wdtopo)
load("Elev_raster.rdata")
load("Slope_raster.rdata")
load("Aspect_raster.rdata")


# Climate data -----------------------------------------------------------------
setwd(wdclimate)
# temperature --------------------------
load(file = "tas_rasters_VJF.rdata")
load(file = "tasmin_rasters_VJF.rdata")
load(file = "tasmax_rasters_VJF.rdata")
load(file = "tasrng_rasters_VJF.rdata")
load(file = "MAT_raster_VJF.rdata")
load(file = "MATr_raster_VJF.rdata")
load(file = "VarT_raster_VJF.rdata")
# Precipitation ------------------------
load(file = "pcp_rasters_VJF.rdata")
load(file = "pcpmin_rasters_VJF.rdata")
load(file = "pcpmax_rasters_VJF.rdata")
load(file = "pcprng_rasters_VJF.rdata")
load(file = "PCP_raster_VJF_BASIC.rdata")
load(file = "PCPr_raster_VJF_BASIC.rdata")
load(file = "VarP_raster_VJF_BASIC.rdata")

# Load Pair Data (COONEY) ------------------------------------------------------
setwd(wdSpeciesNames); load("cooney.rdata")


# Populate species & pair level data -------------------------------------------
# create empty fields -----
# range size
cooney$n_pam_cells_sp1 <- NA; cooney$n_pam_cells_sp2 <- NA

# centrod
cooney$centroid_lon_sp1 <- NA; cooney$centroid_lat_sp1 <- NA 
cooney$centroid_lon_sp2 <- NA; cooney$centroid_lat_sp2 <- NA
cooney$centroid_distance <- NA

# latitude fields
cooney$min_lat_sp1 <- NA; cooney$max_lat_sp1 <- NA; cooney$lat_range_sp1 <- NA; cooney$mean_lat_sp1 <- NA; cooney$med_lat_sp1 <- NA
cooney$min_lat_sp2 <- NA; cooney$max_lat_sp2 <- NA; cooney$lat_range_sp2 <- NA; cooney$mean_lat_sp2 <- NA; cooney$med_lat_sp2 <- NA
cooney$lat_maxofmins <- NA; cooney$lat_minofmaxs <- NA; cooney$lat_ov_range <- NA; cooney$lat_ov_perc_smrnge <- NA

# elevation fields
cooney$min_ele_sp1 <- NA; cooney$max_ele_sp1 <- NA; cooney$ele_range_sp1 <- NA; cooney$mean_ele_sp1 <- NA; cooney$med_ele_sp1 <- NA
cooney$min_ele_sp2 <- NA; cooney$max_ele_sp2 <- NA; cooney$ele_range_sp2 <- NA; cooney$mean_ele_sp2 <- NA; cooney$med_ele_sp2 <- NA
cooney$ele_maxofmins <- NA; cooney$ele_minofmaxs <- NA; cooney$ele_ov_range <- NA; cooney$ele_ov_perc_smrnge <- NA

# MAT fields
cooney$min_MAT_sp1 <- NA; cooney$max_MAT_sp1 <- NA; cooney$MAT_range_sp1 <- NA; cooney$mean_MAT_sp1 <- NA; cooney$med_MAT_sp1 <- NA
cooney$min_MAT_sp2 <- NA; cooney$max_MAT_sp2 <- NA; cooney$MAT_range_sp2 <- NA; cooney$mean_MAT_sp2 <- NA; cooney$med_MAT_sp2 <- NA
cooney$MAT_maxofmins <- NA; cooney$MAT_minofmaxs <- NA; cooney$MAT_ov_range <- NA; cooney$MAT_ov_perc_smrnge <- NA

# MATr fields
cooney$min_MATr_sp1 <- NA; cooney$max_MATr_sp1 <- NA; cooney$MATr_range_sp1 <- NA; cooney$mean_MATr_sp1 <- NA; cooney$med_MATr_sp1 <- NA
cooney$min_MATr_sp2 <- NA; cooney$max_MATr_sp2 <- NA; cooney$MATr_range_sp2 <- NA; cooney$mean_MATr_sp2 <- NA; cooney$med_MATr_sp2 <- NA
cooney$MATr_maxofmins <- NA; cooney$MATr_minofmaxs <- NA; cooney$MATr_ov_range <- NA; cooney$MATr_ov_perc_smrnge <- NA

# VarT fields
cooney$min_VarT_sp1 <- NA; cooney$max_VarT_sp1 <- NA; cooney$VarT_range_sp1 <- NA; cooney$mean_VarT_sp1 <- NA; cooney$med_VarT_sp1 <- NA
cooney$min_VarT_sp2 <- NA; cooney$max_VarT_sp2 <- NA; cooney$VarT_range_sp2 <- NA; cooney$mean_VarT_sp2 <- NA; cooney$med_VarT_sp2 <- NA
cooney$VarT_maxofmins <- NA; cooney$VarT_minofmaxs <- NA; cooney$VarT_ov_range <- NA; cooney$VarT_ov_perc_smrnge <- NA

# pcp fields
cooney$min_pcp_sp1 <- NA; cooney$max_pcp_sp1 <- NA; cooney$pcp_range_sp1 <- NA; cooney$mean_pcp_sp1 <- NA; cooney$med_pcp_sp1 <- NA
cooney$min_pcp_sp2 <- NA; cooney$max_pcp_sp2 <- NA; cooney$pcp_range_sp2 <- NA; cooney$mean_pcp_sp2 <- NA; cooney$med_pcp_sp2 <- NA
cooney$pcp_maxofmins <- NA; cooney$pcp_minofmaxs <- NA; cooney$pcp_ov_range <- NA; cooney$pcp_ov_perc_smrnge <- NA

# pcpr fields
cooney$min_pcpr_sp1 <- NA; cooney$max_pcpr_sp1 <- NA; cooney$pcpr_range_sp1 <- NA; cooney$mean_pcpr_sp1 <- NA; cooney$med_pcpr_sp1 <- NA
cooney$min_pcpr_sp2 <- NA; cooney$max_pcpr_sp2 <- NA; cooney$pcpr_range_sp2 <- NA; cooney$mean_pcpr_sp2 <- NA; cooney$med_pcpr_sp2 <- NA
cooney$pcpr_maxofmins <- NA; cooney$pcpr_minofmaxs <- NA; cooney$pcpr_ov_range <- NA; cooney$pcpr_ov_perc_smrnge <- NA

# VarP fields
cooney$min_VarP_sp1 <- NA; cooney$max_VarP_sp1 <- NA; cooney$VarP_range_sp1 <- NA; cooney$mean_VarP_sp1 <- NA; cooney$med_VarP_sp1 <- NA
cooney$min_VarP_sp2 <- NA; cooney$max_VarP_sp2 <- NA; cooney$VarP_range_sp2 <- NA; cooney$mean_VarP_sp2 <- NA; cooney$med_VarP_sp2 <- NA
cooney$VarP_maxofmins <- NA; cooney$VarP_minofmaxs <- NA; cooney$VarP_ov_range <- NA; cooney$VarP_ov_perc_smrnge <- NA

# temperature fields
cooney[, c("tasmax_sp1_1", "tasmax_sp1_2", "tasmax_sp1_3", "tasmax_sp1_4", "tasmax_sp1_5", "tasmax_sp1_6", "tasmax_sp1_7", "tasmax_sp1_8", "tasmax_sp1_9", "tasmax_sp1_10", "tasmax_sp1_11", "tasmax_sp1_12")] <- NA
cooney[, c("tasmin_sp1_1", "tasmin_sp1_2", "tasmin_sp1_3", "tasmin_sp1_4", "tasmin_sp1_5", "tasmin_sp1_6", "tasmin_sp1_7", "tasmin_sp1_8", "tasmin_sp1_9", "tasmin_sp1_10", "tasmin_sp1_11", "tasmin_sp1_12")] <- NA
cooney[, c("tasrng_sp1_1", "tasrng_sp1_2", "tasrng_sp1_3", "tasrng_sp1_4", "tasrng_sp1_5", "tasrng_sp1_6", "tasrng_sp1_7", "tasrng_sp1_8", "tasrng_sp1_9", "tasrng_sp1_10", "tasrng_sp1_11", "tasrng_sp1_12")] <- NA
cooney[, c("tasmax_sp2_1", "tasmax_sp2_2", "tasmax_sp2_3", "tasmax_sp2_4", "tasmax_sp2_5", "tasmax_sp2_6", "tasmax_sp2_7", "tasmax_sp2_8", "tasmax_sp2_9", "tasmax_sp2_10", "tasmax_sp2_11", "tasmax_sp2_12")] <- NA
cooney[, c("tasmin_sp2_1", "tasmin_sp2_2", "tasmin_sp2_3", "tasmin_sp2_4", "tasmin_sp2_5", "tasmin_sp2_6", "tasmin_sp2_7", "tasmin_sp2_8", "tasmin_sp2_9", "tasmin_sp2_10", "tasmin_sp2_11", "tasmin_sp2_12")] <- NA
cooney[, c("tasrng_sp2_1", "tasrng_sp2_2", "tasrng_sp2_3", "tasrng_sp2_4", "tasrng_sp2_5", "tasrng_sp2_6", "tasrng_sp2_7", "tasrng_sp2_8", "tasrng_sp2_9", "tasrng_sp2_10", "tasrng_sp2_11", "tasrng_sp2_12")] <- NA
cooney[, c("tmp_v_1", "tmp_v_2", "tmp_v_3", "tmp_v_4", "tmp_v_5", "tmp_v_6", "tmp_v_7", "tmp_v_8", "tmp_v_9", "tmp_v_10", "tmp_v_11", "tmp_v_12")] <- NA
cooney$tmp_v_sum <- NA

# precipitation fields
cooney[, c("pcpmax_sp1_1", "pcpmax_sp1_2", "pcpmax_sp1_3", "pcpmax_sp1_4", "pcpmax_sp1_5", "pcpmax_sp1_6", "pcpmax_sp1_7", "pcpmax_sp1_8", "pcpmax_sp1_9", "pcpmax_sp1_10", "pcpmax_sp1_11", "pcpmax_sp1_12")] <- NA
cooney[, c("pcpmin_sp1_1", "pcpmin_sp1_2", "pcpmin_sp1_3", "pcpmin_sp1_4", "pcpmin_sp1_5", "pcpmin_sp1_6", "pcpmin_sp1_7", "pcpmin_sp1_8", "pcpmin_sp1_9", "pcpmin_sp1_10", "pcpmin_sp1_11", "pcpmin_sp1_12")] <- NA
cooney[, c("pcprng_sp1_1", "pcprng_sp1_2", "pcprng_sp1_3", "pcprng_sp1_4", "pcprng_sp1_5", "pcprng_sp1_6", "pcprng_sp1_7", "pcprng_sp1_8", "pcprng_sp1_9", "pcprng_sp1_10", "pcprng_sp1_11", "pcprng_sp1_12")] <- NA
cooney[, c("pcpmax_sp2_1", "pcpmax_sp2_2", "pcpmax_sp2_3", "pcpmax_sp2_4", "pcpmax_sp2_5", "pcpmax_sp2_6", "pcpmax_sp2_7", "pcpmax_sp2_8", "pcpmax_sp2_9", "pcpmax_sp2_10", "pcpmax_sp2_11", "pcpmax_sp2_12")] <- NA
cooney[, c("pcpmin_sp2_1", "pcpmin_sp2_2", "pcpmin_sp2_3", "pcpmin_sp2_4", "pcpmin_sp2_5", "pcpmin_sp2_6", "pcpmin_sp2_7", "pcpmin_sp2_8", "pcpmin_sp2_9", "pcpmin_sp2_10", "pcpmin_sp2_11", "pcpmin_sp2_12")] <- NA
cooney[, c("pcprng_sp2_1", "pcprng_sp2_2", "pcprng_sp2_3", "pcprng_sp2_4", "pcprng_sp2_5", "pcprng_sp2_6", "pcprng_sp2_7", "pcprng_sp2_8", "pcprng_sp2_9", "pcprng_sp2_10", "pcprng_sp2_11", "pcprng_sp2_12")] <- NA
cooney[, c("pcp_v_1", "pcp_v_2", "pcp_v_3", "pcp_v_4", "pcp_v_5", "pcp_v_6", "pcp_v_7", "pcp_v_8", "pcp_v_9", "pcp_v_10", "pcp_v_11", "pcp_v_12")] <- NA
cooney$pcp_v_sum <- NA


# populate the fields one pair at a time -----
for (i in 1:nrow(cooney)) {
  print(paste0("starting species ", i, " out of ", nrow(cooney)))
  mytemp <- cbPAM[,c("Longitude(x)", "Latitude(y)" ,paste0(cooney[i,"Species.1bl"]), paste0(cooney[i,"Species.2bl"]))] # subset pam
  range1<-subset(mytemp, mytemp[,3]==1) # get species ranges
  range2<-subset(mytemp, mytemp[,4]==1); rm(mytemp)
  
  coords1<-data.frame(lon=range1[,1], lat=range1[,2]); coordinates(coords1)<-c("lon","lat"); crs(coords1)<-crs(LonLat_BirdPAM_raster) # get coordinates
  coords2<-data.frame(lon=range2[,1], lat=range2[,2]); coordinates(coords2)<-c("lon","lat"); crs(coords2)<-crs(LonLat_BirdPAM_raster)
  
  # centroid -----------------------------------------------
  centroid1 <- rgeos::gCentroid(coords1)
  cooney$centroid_lon_sp1[i] <- centroid1@coords[1]
  cooney$centroid_lat_sp1[i] <- centroid1@coords[2]
  centroid2 <- rgeos::gCentroid(coords2)
  cooney$centroid_lon_sp2[i] <- centroid2@coords[1]
  cooney$centroid_lat_sp2[i] <- centroid2@coords[2]
  cooney$centroid_distance[i] <- raster::pointDistance(centroid1, centroid2, lonlat = T)
  
  # Latitude -----------------------------------------------
  lat_1<-as.data.frame(coords1$lat); lat_1$sp <- "sp1"; colnames(lat_1) <- c("latitude", "sp") # get dfs -----
  lat_2<-as.data.frame(coords2$lat); lat_2$sp <- "sp2"; colnames(lat_2) <- c("latitude", "sp")
  cooney$mean_lat_sp1[i] <- mean(lat_1$latitude, na.rm = T)
  cooney$mean_lat_sp2[i] <- mean(lat_2$latitude, na.rm = T)
  cooney$med_lat_sp1[i] <- median(lat_1$latitude, na.rm = T)
  cooney$med_lat_sp2[i] <- median(lat_2$latitude, na.rm = T)
  cooney$min_lat_sp1[i] <- min(lat_1$latitude, na.rm = T); cooney$max_lat_sp1[i] <- max(lat_1$latitude, na.rm = T) # max and min -----
  cooney$min_lat_sp2[i] <- min(lat_2$latitude, na.rm = T); cooney$max_lat_sp2[i] <- max(lat_2$latitude, na.rm = T)
  cooney$lat_range_sp1[i] <- abs(cooney$min_lat_sp1[i] - cooney$max_lat_sp1[i]) + 0.5 # range; 0.5 added to fix inf values -- species with one cell have a range of 0.5, not 0!
  cooney$lat_range_sp2[i] <- abs(cooney$min_lat_sp2[i] - cooney$max_lat_sp2[i]) + 0.5
  cooney$lat_maxofmins[i] <- max(cooney$min_lat_sp1[i], cooney$min_lat_sp2[i], na.rm = T) # overlap % = Union range  / smaller range
  cooney$lat_minofmaxs[i] <- min(cooney$max_lat_sp1[i], cooney$max_lat_sp2[i], na.rm = T)
  cooney$lat_ov_range[i] <- cooney$lat_minofmaxs[i] - cooney$lat_maxofmins[i]
  cooney$lat_ov_perc_smrnge[i] <- cooney$lat_ov_range[i] / min(cooney$lat_range_sp1[i], cooney$lat_range_sp2[i], na.rm = T)
  
  # Elevation ----------------------------------------------
  elev_1<-as.data.frame(extract(x=Elev_raster, y=coords1)); elev_1$sp <- "sp1"; colnames(elev_1) <- c("elev", "sp") # get dfs -----
  elev_2<-as.data.frame(extract(x=Elev_raster, y=coords2)); elev_2$sp <- "sp2"; colnames(elev_2) <- c("elev", "sp")
  cooney$mean_ele_sp1[i] <- mean(elev_1$elev, na.rm = T)
  cooney$mean_ele_sp2[i] <- mean(elev_2$elev, na.rm = T)
  cooney$med_ele_sp1[i] <- median(elev_1$elev, na.rm = T)
  cooney$med_ele_sp2[i] <- median(elev_2$elev, na.rm = T)
  cooney$min_ele_sp1[i] <- min(elev_1$elev, na.rm = T); cooney$max_ele_sp1[i] <- max(elev_1$elev, na.rm = T) # max and min -----
  cooney$min_ele_sp2[i] <- min(elev_2$elev, na.rm = T); cooney$max_ele_sp2[i] <- max(elev_2$elev, na.rm = T)
  cooney$ele_range_sp1[i] <- abs(cooney$min_ele_sp1[i] - cooney$max_ele_sp1[i]) + 1E-24 # range ----- # 1e-12 added to fix calcs for sp with a single occurance point.
  cooney$ele_range_sp2[i] <- abs(cooney$min_ele_sp2[i] - cooney$max_ele_sp2[i]) + 1E-24
  cooney$ele_maxofmins[i] <- max(cooney$min_ele_sp1[i], cooney$min_ele_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooney$ele_minofmaxs[i] <- min(cooney$max_ele_sp1[i], cooney$max_ele_sp2[i], na.rm = T)
  cooney$ele_ov_range[i] <- cooney$ele_minofmaxs[i] - cooney$ele_maxofmins[i]
  cooney$ele_ov_perc_smrnge[i] <- cooney$ele_ov_range[i] / min(cooney$ele_range_sp1[i], cooney$ele_range_sp2[i], na.rm = T)
  
  # MAT ----------------------------------------------------
  MAT_1<-as.data.frame(extract(x=MAT, y=coords1)); MAT_1$sp <- "sp1"; colnames(MAT_1) <- c("MAT", "sp") # get dfs -----
  MAT_2<-as.data.frame(extract(x=MAT, y=coords2)); MAT_2$sp <- "sp2"; colnames(MAT_2) <- c("MAT", "sp")
  cooney$mean_MAT_sp1[i] <- mean(MAT_1$MAT, na.rm = T)
  cooney$mean_MAT_sp2[i] <- mean(MAT_2$MAT, na.rm = T)
  cooney$med_MAT_sp1[i] <- median(MAT_1$MAT, na.rm = T)
  cooney$med_MAT_sp2[i] <- median(MAT_2$MAT, na.rm = T)
  cooney$min_MAT_sp1[i] <- min(MAT_1$MAT, na.rm = T); cooney$max_MAT_sp1[i] <- max(MAT_1$MAT, na.rm = T) # max and min -----
  cooney$min_MAT_sp2[i] <- min(MAT_2$MAT, na.rm = T); cooney$max_MAT_sp2[i] <- max(MAT_2$MAT, na.rm = T)
  cooney$MAT_range_sp1[i] <- abs(cooney$min_MAT_sp1[i] - cooney$max_MAT_sp1[i]) + 1E-24 # range -----
  cooney$MAT_range_sp2[i] <- abs(cooney$min_MAT_sp2[i] - cooney$max_MAT_sp2[i]) + 1E-24
  cooney$MAT_maxofmins[i] <- max(cooney$min_MAT_sp1[i], cooney$min_MAT_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooney$MAT_minofmaxs[i] <- min(cooney$max_MAT_sp1[i], cooney$max_MAT_sp2[i], na.rm = T)
  cooney$MAT_ov_range[i] <- cooney$MAT_minofmaxs[i] - cooney$MAT_maxofmins[i]
  cooney$MAT_ov_perc_smrnge[i] <- cooney$MAT_ov_range[i] / min(cooney$MAT_range_sp1[i], cooney$MAT_range_sp2[i], na.rm = T)
  
  # MATr ----------------------------------------------------
  MATr_1<-as.data.frame(extract(x=MATr, y=coords1)); MATr_1$sp <- "sp1"; colnames(MATr_1) <- c("MATr", "sp") # get dfs -----
  MATr_2<-as.data.frame(extract(x=MATr, y=coords2)); MATr_2$sp <- "sp2"; colnames(MATr_2) <- c("MATr", "sp")
  cooney$mean_MATr_sp1[i] <- mean(MATr_1$MATr, na.rm = T)
  cooney$mean_MATr_sp2[i] <- mean(MATr_2$MATr, na.rm = T)
  cooney$med_MATr_sp1[i] <- median(MATr_1$MATr, na.rm = T)
  cooney$med_MATr_sp2[i] <- median(MATr_2$MATr, na.rm = T)
  cooney$min_MATr_sp1[i] <- min(MATr_1$MATr, na.rm = T); cooney$max_MATr_sp1[i] <- max(MATr_1$MATr, na.rm = T) # max and min -----
  cooney$min_MATr_sp2[i] <- min(MATr_2$MATr, na.rm = T); cooney$max_MATr_sp2[i] <- max(MATr_2$MATr, na.rm = T)
  cooney$MATr_range_sp1[i] <- abs(cooney$min_MATr_sp1[i] - cooney$max_MATr_sp1[i]) + 1E-24 # range -----
  cooney$MATr_range_sp2[i] <- abs(cooney$min_MATr_sp2[i] - cooney$max_MATr_sp2[i]) + 1E-24
  cooney$MATr_maxofmins[i] <- max(cooney$min_MATr_sp1[i], cooney$min_MATr_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooney$MATr_minofmaxs[i] <- min(cooney$max_MATr_sp1[i], cooney$max_MATr_sp2[i], na.rm = T)
  cooney$MATr_ov_range[i] <- cooney$MATr_minofmaxs[i] - cooney$MATr_maxofmins[i]
  cooney$MATr_ov_perc_smrnge[i] <- cooney$MATr_ov_range[i] / min(cooney$MATr_range_sp1[i], cooney$MATr_range_sp2[i], na.rm = T)
  
  # VarT ----------------------------------------------------
  VarT_1<-as.data.frame(extract(x=VarT, y=coords1)); VarT_1$sp <- "sp1"; colnames(VarT_1) <- c("VarT", "sp") # get dfs -----
  VarT_2<-as.data.frame(extract(x=VarT, y=coords2)); VarT_2$sp <- "sp2"; colnames(VarT_2) <- c("VarT", "sp")
  cooney$mean_VarT_sp1[i] <- mean(VarT_1$VarT, na.rm = T)
  cooney$mean_VarT_sp2[i] <- mean(VarT_2$VarT, na.rm = T)
  cooney$med_VarT_sp1[i] <- median(VarT_1$VarT, na.rm = T)
  cooney$med_VarT_sp2[i] <- median(VarT_2$VarT, na.rm = T)
  cooney$min_VarT_sp1[i] <- min(VarT_1$VarT, na.rm = T); cooney$max_VarT_sp1[i] <- max(VarT_1$VarT, na.rm = T) # max and min -----
  cooney$min_VarT_sp2[i] <- min(VarT_2$VarT, na.rm = T); cooney$max_VarT_sp2[i] <- max(VarT_2$VarT, na.rm = T)
  cooney$VarT_range_sp1[i] <- abs(cooney$min_VarT_sp1[i] - cooney$max_VarT_sp1[i]) + 1E-24 # range -----
  cooney$VarT_range_sp2[i] <- abs(cooney$min_VarT_sp2[i] - cooney$max_VarT_sp2[i]) + 1E-24
  cooney$VarT_maxofmins[i] <- max(cooney$min_VarT_sp1[i], cooney$min_VarT_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooney$VarT_minofmaxs[i] <- min(cooney$max_VarT_sp1[i], cooney$max_VarT_sp2[i], na.rm = T)
  cooney$VarT_ov_range[i] <- cooney$VarT_minofmaxs[i] - cooney$VarT_maxofmins[i]
  cooney$VarT_ov_perc_smrnge[i] <- cooney$VarT_ov_range[i] / min(cooney$VarT_range_sp1[i], cooney$VarT_range_sp2[i], na.rm = T)
  
  # pcp ----------------------------------------------------
  pcp_1<-as.data.frame(extract(x=PCP, y=coords1)); pcp_1$sp <- "sp1"; colnames(pcp_1) <- c("pcp", "sp") # get dfs -----
  pcp_2<-as.data.frame(extract(x=PCP, y=coords2)); pcp_2$sp <- "sp2"; colnames(pcp_2) <- c("pcp", "sp")
  cooney$mean_pcp_sp1[i] <- mean(pcp_1$pcp, na.rm = T)
  cooney$mean_pcp_sp2[i] <- mean(pcp_2$pcp, na.rm = T)
  cooney$med_pcp_sp1[i] <- median(pcp_1$pcp, na.rm = T)
  cooney$med_pcp_sp2[i] <- median(pcp_2$pcp, na.rm = T)
  cooney$min_pcp_sp1[i] <- min(pcp_1$pcp, na.rm = T); cooney$max_pcp_sp1[i] <- max(pcp_1$pcp, na.rm = T) # max and min -----
  cooney$min_pcp_sp2[i] <- min(pcp_2$pcp, na.rm = T); cooney$max_pcp_sp2[i] <- max(pcp_2$pcp, na.rm = T)
  cooney$pcp_range_sp1[i] <- abs(cooney$min_pcp_sp1[i] - cooney$max_pcp_sp1[i]) + 1E-24 # range -----
  cooney$pcp_range_sp2[i] <- abs(cooney$min_pcp_sp2[i] - cooney$max_pcp_sp2[i]) + 1E-24
  cooney$pcp_maxofmins[i] <- max(cooney$min_pcp_sp1[i], cooney$min_pcp_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooney$pcp_minofmaxs[i] <- min(cooney$max_pcp_sp1[i], cooney$max_pcp_sp2[i], na.rm = T)
  cooney$pcp_ov_range[i] <- cooney$pcp_minofmaxs[i] - cooney$pcp_maxofmins[i]
  cooney$pcp_ov_perc_smrnge[i] <- cooney$pcp_ov_range[i] / min(cooney$pcp_range_sp1[i], cooney$pcp_range_sp2[i], na.rm = T)
  
  # pcpr ----------------------------------------------------
  pcpr_1<-as.data.frame(extract(x=PCPr, y=coords1)); pcpr_1$sp <- "sp1"; colnames(pcpr_1) <- c("pcpr", "sp") # get dfs -----
  pcpr_2<-as.data.frame(extract(x=PCPr, y=coords2)); pcpr_2$sp <- "sp2"; colnames(pcpr_2) <- c("pcpr", "sp")
  cooney$mean_pcpr_sp1[i] <- mean(pcpr_1$pcpr, na.rm = T)
  cooney$mean_pcpr_sp2[i] <- mean(pcpr_2$pcpr, na.rm = T)
  cooney$med_pcpr_sp1[i] <- median(pcpr_1$pcpr, na.rm = T)
  cooney$med_pcpr_sp2[i] <- median(pcpr_2$pcpr, na.rm = T)
  cooney$min_pcpr_sp1[i] <- min(pcpr_1$pcpr, na.rm = T); cooney$max_pcpr_sp1[i] <- max(pcpr_1$pcpr, na.rm = T) # max and min -----
  cooney$min_pcpr_sp2[i] <- min(pcpr_2$pcpr, na.rm = T); cooney$max_pcpr_sp2[i] <- max(pcpr_2$pcpr, na.rm = T)
  cooney$pcpr_range_sp1[i] <- abs(cooney$min_pcpr_sp1[i] - cooney$max_pcpr_sp1[i]) + 1E-24  # range -----
  cooney$pcpr_range_sp2[i] <- abs(cooney$min_pcpr_sp2[i] - cooney$max_pcpr_sp2[i]) + 1E-24
  cooney$pcpr_maxofmins[i] <- max(cooney$min_pcpr_sp1[i], cooney$min_pcpr_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooney$pcpr_minofmaxs[i] <- min(cooney$max_pcpr_sp1[i], cooney$max_pcpr_sp2[i], na.rm = T)
  cooney$pcpr_ov_range[i] <- cooney$pcpr_minofmaxs[i] - cooney$pcpr_maxofmins[i]
  cooney$pcpr_ov_perc_smrnge[i] <- cooney$pcpr_ov_range[i] / min(cooney$pcpr_range_sp1[i], cooney$pcpr_range_sp2[i], na.rm = T)
  
  # VarP ----------------------------------------------------
  VarP_1<-as.data.frame(extract(x=VarP, y=coords1)); VarP_1$sp <- "sp1"; colnames(VarP_1) <- c("VarP", "sp") # get dfs -----
  VarP_2<-as.data.frame(extract(x=VarP, y=coords2)); VarP_2$sp <- "sp2"; colnames(VarP_2) <- c("VarP", "sp")
  cooney$mean_VarP_sp1[i] <- mean(VarP_1$VarP, na.rm = T)
  cooney$mean_VarP_sp2[i] <- mean(VarP_2$VarP, na.rm = T)
  cooney$med_VarP_sp1[i] <- median(VarP_1$VarP, na.rm = T)
  cooney$med_VarP_sp2[i] <- median(VarP_2$VarP, na.rm = T)
  cooney$min_VarP_sp1[i] <- min(VarP_1$VarP, na.rm = T); cooney$max_VarP_sp1[i] <- max(VarP_1$VarP, na.rm = T) # max and min -----
  cooney$min_VarP_sp2[i] <- min(VarP_2$VarP, na.rm = T); cooney$max_VarP_sp2[i] <- max(VarP_2$VarP, na.rm = T)
  cooney$VarP_range_sp1[i] <- abs(cooney$min_VarP_sp1[i] - cooney$max_VarP_sp1[i]) + 1E-24 # range -----
  cooney$VarP_range_sp2[i] <- abs(cooney$min_VarP_sp2[i] - cooney$max_VarP_sp2[i]) + 1E-24
  cooney$VarP_maxofmins[i] <- max(cooney$min_VarP_sp1[i], cooney$min_VarP_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooney$VarP_minofmaxs[i] <- min(cooney$max_VarP_sp1[i], cooney$max_VarP_sp2[i], na.rm = T)
  cooney$VarP_ov_range[i] <- cooney$VarP_minofmaxs[i] - cooney$VarP_maxofmins[i]
  cooney$VarP_ov_perc_smrnge[i] <- cooney$VarP_ov_range[i] / min(cooney$VarP_range_sp1[i], cooney$VarP_range_sp2[i], na.rm = T)
  
  # basic info --------------------------------------------
  cooney$n_pam_cells_sp1[i] <- length(!is.na(lat_1$latitude)); cooney$n_pam_cells_sp2[i] <- length(!is.na(lat_2$latitude)) # range size
  
  # temperature --------------------------------------------
  tasmax_1 <- colMeans(extract(x=tasmax, y = coords1), na.rm = T) # species 1
  tasmin_1 <- colMeans(extract(x=tasmin, y = coords1), na.rm = T)
  tasrng_1 <- tasmax_1 - tasmin_1
  cooney[i, c("tasmax_sp1_1", "tasmax_sp1_2", "tasmax_sp1_3", "tasmax_sp1_4", "tasmax_sp1_5", "tasmax_sp1_6", "tasmax_sp1_7", "tasmax_sp1_8", "tasmax_sp1_9", "tasmax_sp1_10", "tasmax_sp1_11", "tasmax_sp1_12")] <- tasmax_1
  cooney[i, c("tasmin_sp1_1", "tasmin_sp1_2", "tasmin_sp1_3", "tasmin_sp1_4", "tasmin_sp1_5", "tasmin_sp1_6", "tasmin_sp1_7", "tasmin_sp1_8", "tasmin_sp1_9", "tasmin_sp1_10", "tasmin_sp1_11", "tasmin_sp1_12")] <- tasmin_1
  cooney[i, c("tasrng_sp1_1", "tasrng_sp1_2", "tasrng_sp1_3", "tasrng_sp1_4", "tasrng_sp1_5", "tasrng_sp1_6", "tasrng_sp1_7", "tasrng_sp1_8", "tasrng_sp1_9", "tasrng_sp1_10", "tasrng_sp1_11", "tasrng_sp1_12")] <- tasrng_1
  
  tasmax_2 <- colMeans(extract(x=tasmax, y = coords2), na.rm = T) # species 2
  tasmin_2 <- colMeans(extract(x=tasmin, y = coords2), na.rm = T)
  tasrng_2 <- tasmax_2 - tasmin_2
  cooney[i, c("tasmax_sp2_1", "tasmax_sp2_2", "tasmax_sp2_3", "tasmax_sp2_4", "tasmax_sp2_5", "tasmax_sp2_6", "tasmax_sp2_7", "tasmax_sp2_8", "tasmax_sp2_9", "tasmax_sp2_10", "tasmax_sp2_11", "tasmax_sp2_12")] <- tasmax_2
  cooney[i, c("tasmin_sp2_1", "tasmin_sp2_2", "tasmin_sp2_3", "tasmin_sp2_4", "tasmin_sp2_5", "tasmin_sp2_6", "tasmin_sp2_7", "tasmin_sp2_8", "tasmin_sp2_9", "tasmin_sp2_10", "tasmin_sp2_11", "tasmin_sp2_12")] <- tasmin_2
  cooney[i, c("tasrng_sp2_1", "tasrng_sp2_2", "tasrng_sp2_3", "tasrng_sp2_4", "tasrng_sp2_5", "tasrng_sp2_6", "tasrng_sp2_7", "tasrng_sp2_8", "tasrng_sp2_9", "tasrng_sp2_10", "tasrng_sp2_11", "tasrng_sp2_12")] <- tasrng_2
  
  tmp_maxofmins <- pmax(tasmin_1, tasmin_2) # overlap
  tmp_minofmaxs <- pmin(tasmax_1, tasmax_2)
  tmp_o <- tmp_minofmaxs - tmp_maxofmins
  tmp_v <- tmp_o / sqrt(tasrng_1*tasrng_2)
  cooney[i, c("tmp_v_1", "tmp_v_2", "tmp_v_3", "tmp_v_4", "tmp_v_5", "tmp_v_6", "tmp_v_7", "tmp_v_8", "tmp_v_9", "tmp_v_10", "tmp_v_11", "tmp_v_12")] <- tmp_v
  cooney$tmp_v_sum[i] <- sum(tmp_v, na.rm = T)
  
  # Precipitation --------------------------------------------
  pcpmax_1 <- colMeans(extract(x=pcpmax, y = coords1), na.rm = T) # species 1
  pcpmin_1 <- colMeans(extract(x=pcpmin, y = coords1), na.rm = T)
  pcprng_1 <- pcpmax_1 - pcpmin_1
  cooney[i, c("pcpmax_sp1_1", "pcpmax_sp1_2", "pcpmax_sp1_3", "pcpmax_sp1_4", "pcpmax_sp1_5", "pcpmax_sp1_6", "pcpmax_sp1_7", "pcpmax_sp1_8", "pcpmax_sp1_9", "pcpmax_sp1_10", "pcpmax_sp1_11", "pcpmax_sp1_12")] <- pcpmax_1
  cooney[i, c("pcpmin_sp1_1", "pcpmin_sp1_2", "pcpmin_sp1_3", "pcpmin_sp1_4", "pcpmin_sp1_5", "pcpmin_sp1_6", "pcpmin_sp1_7", "pcpmin_sp1_8", "pcpmin_sp1_9", "pcpmin_sp1_10", "pcpmin_sp1_11", "pcpmin_sp1_12")] <- pcpmin_1
  cooney[i, c("pcprng_sp1_1", "pcprng_sp1_2", "pcprng_sp1_3", "pcprng_sp1_4", "pcprng_sp1_5", "pcprng_sp1_6", "pcprng_sp1_7", "pcprng_sp1_8", "pcprng_sp1_9", "pcprng_sp1_10", "pcprng_sp1_11", "pcprng_sp1_12")] <- pcprng_1
  
  pcpmax_2 <- colMeans(extract(x=pcpmax, y = coords2), na.rm = T) # species 2
  pcpmin_2 <- colMeans(extract(x=pcpmin, y = coords2), na.rm = T)
  pcprng_2 <- pcpmax_2 - pcpmin_2
  cooney[i, c("pcpmax_sp2_1", "pcpmax_sp2_2", "pcpmax_sp2_3", "pcpmax_sp2_4", "pcpmax_sp2_5", "pcpmax_sp2_6", "pcpmax_sp2_7", "pcpmax_sp2_8", "pcpmax_sp2_9", "pcpmax_sp2_10", "pcpmax_sp2_11", "pcpmax_sp2_12")] <- pcpmax_2
  cooney[i, c("pcpmin_sp2_1", "pcpmin_sp2_2", "pcpmin_sp2_3", "pcpmin_sp2_4", "pcpmin_sp2_5", "pcpmin_sp2_6", "pcpmin_sp2_7", "pcpmin_sp2_8", "pcpmin_sp2_9", "pcpmin_sp2_10", "pcpmin_sp2_11", "pcpmin_sp2_12")] <- pcpmin_2
  cooney[i, c("pcprng_sp2_1", "pcprng_sp2_2", "pcprng_sp2_3", "pcprng_sp2_4", "pcprng_sp2_5", "pcprng_sp2_6", "pcprng_sp2_7", "pcprng_sp2_8", "pcprng_sp2_9", "pcprng_sp2_10", "pcprng_sp2_11", "pcprng_sp2_12")] <- pcprng_2
  
  pcp_maxofmins <- pmax(pcpmin_1, pcpmin_2) # overlap
  pcp_minofmaxs <- pmin(pcpmax_1, pcpmax_2)
  pcp_o <- pcp_minofmaxs - pcp_maxofmins
  pcp_v <- pcp_o / sqrt(pcprng_1*pcprng_2)
  cooney[i, c("pcp_v_1", "pcp_v_2", "pcp_v_3", "pcp_v_4", "pcp_v_5", "pcp_v_6", "pcp_v_7", "pcp_v_8", "pcp_v_9", "pcp_v_10", "pcp_v_11", "pcp_v_12")] <- pcp_v
  cooney$pcp_v_sum[i] <- sum(pcp_v, na.rm = T)
  
  rm(elev_1, elev_2, lat_1, lat_2, range1, range2, coords1, coords2, MAT_1, MAT_2, VarT_1, VarT_2,
     tasmax_1, tasmax_2, tasmin_1, tasmin_2, tasrng_1, tasrng_2, tmp_maxofmins, tmp_minofmaxs, tmp_o, tmp_v, 
     pcpmax_1, pcpmax_2, pcpmin_1, pcpmin_2, pcprng_1, pcprng_2, pcp_maxofmins, pcp_minofmaxs, pcp_o, pcp_v)
  
}
cooney <- cooney[is.finite(cooney$min_ele_sp1) & is.finite(cooney$min_ele_sp2),] # remove any pairs that are missing elev data (should be none)
cooney$sortcol <- rowMeans(cbind(cooney$n_pam_cells_sp1, cooney$n_pam_cells_sp2), na.rm = T) # calculate mean pair range size as the column to sort by
cooney <- cooney[order(cooney$sortcol, cooney$uniquePairId),] # sort by mean pair range size, smallest first
cooney$sortorder <- seq(1, nrow(cooney)) # preserve the sort order
cooneyp <- cooney


setwd(wdMainDataFrame); save(cooneyp, file = "cooneyp_beefy.Rdata")





















