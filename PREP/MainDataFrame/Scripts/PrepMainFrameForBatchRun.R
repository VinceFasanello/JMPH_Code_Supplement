# clear workspace and load all required packages -------------------------------
rm(list=ls()); gc(); memory.size();memory.limit()
require(rgdal)
require(dplyr)
require(movecost)
require(sp)
require(raster)
require(gdistance)

# directory paths --------------------------------------------------------------
wdtopo <- "~/Box Sync/JMPH/PREP/Topo/Data"
wdclimate <- "~/Box Sync/JMPH/PREP/Climate/Data"
wdPAM <- "~/Box Sync/JMPH/PREP/PAM/Data"
wdSpeciesNames <- "~/Box Sync/JMPH/PREP/SpeciesNames/Data"
wdMainDataFrame <- "~/Box Sync/JMPH/PREP/MainDataFrame/Data"

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
# Precipitation ------------------------
load(file = "pcp_rasters_VJF.rdata")
load(file = "pcpmin_rasters_VJF.rdata")
load(file = "pcpmax_rasters_VJF.rdata")
load(file = "pcprng_rasters_VJF.rdata")


# Load Pair Data (COONEY) ------------------------------------------------------
setwd(wdSpeciesNames); load("cooney.rdata")


# Populate species & pair level data -------------------------------------------
# create empty fields -----
# range size
cooney$n_pam_cells_sp1 <- NA; cooney$n_pam_cells_sp2 <- NA

# latitude fields
cooney$min_lat_sp1 <- NA; cooney$max_lat_sp1 <- NA; cooney$lat_range_sp1 <- NA
cooney$min_lat_sp2 <- NA; cooney$max_lat_sp2 <- NA; cooney$lat_range_sp2 <- NA
cooney$lat_maxofmins <- NA; cooney$lat_minofmaxs <- NA; cooney$lat_ov_range <- NA; cooney$lat_ov_perc_smrnge <- NA

# elevation fields
cooney$min_ele_sp1 <- NA; cooney$max_ele_sp1 <- NA; cooney$ele_range_sp1 <- NA
cooney$min_ele_sp2 <- NA; cooney$max_ele_sp2 <- NA; cooney$ele_range_sp2 <- NA
cooney$ele_maxofmins <- NA; cooney$ele_minofmaxs <- NA; cooney$ele_ov_range <- NA; cooney$ele_ov_perc_smrnge <- NA

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
  
  # Latitude -----------------------------------------------
  lat_1<-as.data.frame(coords1$lat); lat_1$sp <- "sp1"; colnames(lat_1) <- c("latitude", "sp") # get dfs -----
  lat_2<-as.data.frame(coords2$lat); lat_2$sp <- "sp2"; colnames(lat_2) <- c("latitude", "sp")
  cooney$min_lat_sp1[i] <- min(lat_1$latitude, na.rm = T); cooney$max_lat_sp1[i] <- max(lat_1$latitude, na.rm = T) # max and min -----
  cooney$min_lat_sp2[i] <- min(lat_2$latitude, na.rm = T); cooney$max_lat_sp2[i] <- max(lat_2$latitude, na.rm = T)
  cooney$lat_range_sp1[i] <- abs(cooney$min_lat_sp1[i] - cooney$max_lat_sp1[i]) # range
  cooney$lat_range_sp2[i] <- abs(cooney$min_lat_sp2[i] - cooney$max_lat_sp2[i])
  cooney$lat_maxofmins[i] <- max(cooney$min_lat_sp1[i], cooney$min_lat_sp2[i], na.rm = T) # overlap % = Union range  / smaller range
  cooney$lat_minofmaxs[i] <- min(cooney$max_lat_sp1[i], cooney$max_lat_sp2[i], na.rm = T)
  cooney$lat_ov_range[i] <- cooney$lat_minofmaxs[i] - cooney$lat_maxofmins[i]
  cooney$lat_ov_perc_smrnge[i] <- cooney$lat_ov_range[i] / min(cooney$lat_range_sp1[i], cooney$lat_range_sp2[i], na.rm = T)
  
  # Elevation ----------------------------------------------
  elev_1<-as.data.frame(extract(x=Elev_raster, y=coords1)); elev_1$sp <- "sp1"; colnames(elev_1) <- c("elev", "sp") # get dfs -----
  elev_2<-as.data.frame(extract(x=Elev_raster, y=coords2)); elev_2$sp <- "sp2"; colnames(elev_2) <- c("elev", "sp")
  cooney$min_ele_sp1[i] <- min(elev_1$elev, na.rm = T); cooney$max_ele_sp1[i] <- max(elev_1$elev, na.rm = T) # max and min -----
  cooney$min_ele_sp2[i] <- min(elev_2$elev, na.rm = T); cooney$max_ele_sp2[i] <- max(elev_2$elev, na.rm = T)
  cooney$ele_range_sp1[i] <- abs(cooney$min_ele_sp1[i] - cooney$max_ele_sp1[i]) # range -----
  cooney$ele_range_sp2[i] <- abs(cooney$min_ele_sp2[i] - cooney$max_ele_sp2[i])
  cooney$ele_maxofmins[i] <- max(cooney$min_ele_sp1[i], cooney$min_ele_sp2[i], na.rm = T) # overlap % = Union range  / smaller range -----
  cooney$ele_minofmaxs[i] <- min(cooney$max_ele_sp1[i], cooney$max_ele_sp2[i], na.rm = T)
  cooney$ele_ov_range[i] <- cooney$ele_minofmaxs[i] - cooney$ele_maxofmins[i]
  cooney$ele_ov_perc_smrnge[i] <- cooney$ele_ov_range[i] / min(cooney$ele_range_sp1[i], cooney$ele_range_sp2[i], na.rm = T)
  
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
  
  rm(elev_1, elev_2, lat_1, lat_2, range1, range2, coords1, coords2,
     tasmax_1, tasmax_2, tasmin_1, tasmin_2, tasrng_1, tasrng_2, tmp_maxofmins, tmp_minofmaxs, tmp_o, tmp_v, 
     pcpmax_1, pcpmax_2, pcpmin_1, pcpmin_2, pcprng_1, pcprng_2, pcp_maxofmins, pcp_minofmaxs, pcp_o, pcp_v)
  
}
cooney <- cooney[is.finite(cooney$min_ele_sp1) & is.finite(cooney$min_ele_sp2),] # remove any pairs that are missing elev data (should be none)
cooney$sortcol <- rowMeans(cbind(cooney$n_pam_cells_sp1, cooney$n_pam_cells_sp2), na.rm = T) # calculate mean pair range size as the column to sort by
cooney <- cooney[order(cooney$sortcol, cooney$uniquePairId),] # sort by mean pair range size, smallest first
cooney$sortorder <- seq(1, nrow(cooney)) # preserve the sort order
cooneyp <- cooney

setwd(wdMainDataFrame); save(cooneyp, file = "cooneyp.Rdata")




# Pregenerate empty matrices ---------------------------------------------------
n_origins <- 5000 # number of cells in the origin range, A, to use as origin points
m_a_ids <- matrix(nrow = nrow(cooneyp), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(cooneyp), 1))), c(paste0("a", seq(1, n_origins, 1)))))
rownames(m_a_ids) <- cooneyp$Species.1bl

# create matrices to hold origin info
m_a_lons <- m_a_ids 
m_a_lats <- m_a_ids

m_a_eles <- m_a_ids # topography
m_a_slop <- m_a_ids
m_a_aspe <- m_a_ids

m_a_tas_M1 <- m_a_ids; m_a_tasmax_M1 <- m_a_ids; m_a_tasmin_M1 <- m_a_ids; m_a_tasrng_M1 <- m_a_ids # Temperature
m_a_tas_M2 <- m_a_ids; m_a_tasmax_M2 <- m_a_ids; m_a_tasmin_M2 <- m_a_ids; m_a_tasrng_M2 <- m_a_ids
m_a_tas_M3 <- m_a_ids; m_a_tasmax_M3 <- m_a_ids; m_a_tasmin_M3 <- m_a_ids; m_a_tasrng_M3 <- m_a_ids
m_a_tas_M4 <- m_a_ids; m_a_tasmax_M4 <- m_a_ids; m_a_tasmin_M4 <- m_a_ids; m_a_tasrng_M4 <- m_a_ids
m_a_tas_M5 <- m_a_ids; m_a_tasmax_M5 <- m_a_ids; m_a_tasmin_M5 <- m_a_ids; m_a_tasrng_M5 <- m_a_ids
m_a_tas_M6 <- m_a_ids; m_a_tasmax_M6 <- m_a_ids; m_a_tasmin_M6 <- m_a_ids; m_a_tasrng_M6 <- m_a_ids
m_a_tas_M7 <- m_a_ids; m_a_tasmax_M7 <- m_a_ids; m_a_tasmin_M7 <- m_a_ids; m_a_tasrng_M7 <- m_a_ids
m_a_tas_M8 <- m_a_ids; m_a_tasmax_M8 <- m_a_ids; m_a_tasmin_M8 <- m_a_ids; m_a_tasrng_M8 <- m_a_ids
m_a_tas_M9 <- m_a_ids; m_a_tasmax_M9 <- m_a_ids; m_a_tasmin_M9 <- m_a_ids; m_a_tasrng_M9 <- m_a_ids
m_a_tas_M10 <- m_a_ids; m_a_tasmax_M10 <- m_a_ids; m_a_tasmin_M10 <- m_a_ids; m_a_tasrng_M10 <- m_a_ids
m_a_tas_M11 <- m_a_ids; m_a_tasmax_M11 <- m_a_ids; m_a_tasmin_M11 <- m_a_ids; m_a_tasrng_M11 <- m_a_ids
m_a_tas_M12 <- m_a_ids; m_a_tasmax_M12 <- m_a_ids; m_a_tasmin_M12 <- m_a_ids; m_a_tasrng_M12 <- m_a_ids

m_a_pcp_P1 <- m_a_ids; m_a_pcpmax_P1 <- m_a_ids; m_a_pcpmin_P1 <- m_a_ids; m_a_pcprng_P1 <- m_a_ids # precipitation
m_a_pcp_P2 <- m_a_ids; m_a_pcpmax_P2 <- m_a_ids; m_a_pcpmin_P2 <- m_a_ids; m_a_pcprng_P2 <- m_a_ids
m_a_pcp_P3 <- m_a_ids; m_a_pcpmax_P3 <- m_a_ids; m_a_pcpmin_P3 <- m_a_ids; m_a_pcprng_P3 <- m_a_ids
m_a_pcp_P4 <- m_a_ids; m_a_pcpmax_P4 <- m_a_ids; m_a_pcpmin_P4 <- m_a_ids; m_a_pcprng_P4 <- m_a_ids
m_a_pcp_P5 <- m_a_ids; m_a_pcpmax_P5 <- m_a_ids; m_a_pcpmin_P5 <- m_a_ids; m_a_pcprng_P5 <- m_a_ids
m_a_pcp_P6 <- m_a_ids; m_a_pcpmax_P6 <- m_a_ids; m_a_pcpmin_P6 <- m_a_ids; m_a_pcprng_P6 <- m_a_ids
m_a_pcp_P7 <- m_a_ids; m_a_pcpmax_P7 <- m_a_ids; m_a_pcpmin_P7 <- m_a_ids; m_a_pcprng_P7 <- m_a_ids
m_a_pcp_P8 <- m_a_ids; m_a_pcpmax_P8 <- m_a_ids; m_a_pcpmin_P8 <- m_a_ids; m_a_pcprng_P8 <- m_a_ids
m_a_pcp_P9 <- m_a_ids; m_a_pcpmax_P9 <- m_a_ids; m_a_pcpmin_P9 <- m_a_ids; m_a_pcprng_P9 <- m_a_ids
m_a_pcp_P10 <- m_a_ids; m_a_pcpmax_P10 <- m_a_ids; m_a_pcpmin_P10 <- m_a_ids; m_a_pcprng_P10 <- m_a_ids
m_a_pcp_P11 <- m_a_ids; m_a_pcpmax_P11 <- m_a_ids; m_a_pcpmin_P11 <- m_a_ids; m_a_pcprng_P11 <- m_a_ids
m_a_pcp_P12 <- m_a_ids; m_a_pcpmax_P12 <- m_a_ids; m_a_pcpmin_P12 <- m_a_ids; m_a_pcprng_P12 <- m_a_ids

# create matrices to hold only the most basic info for the best destination. Can always get other info LATER! + create matrices for the lcp info
m_a_bblons_ele <- m_a_ids # elevational
m_a_bblats_ele <- m_a_ids
m_aB_ele_mcosts <- m_a_ids
m_aB_ele_plengths <- m_a_ids

m_a_bblons_tmp <- m_a_ids  # thermal
m_a_bblats_tmp <- m_a_ids
m_aB_tmp_mcosts <- m_a_ids
m_aB_tmp_plengths <- m_a_ids

m_a_bblons_pcp <- m_a_ids  # precipitation
m_a_bblats_pcp <- m_a_ids
m_aB_pcp_mcosts <- m_a_ids
m_aB_pcp_plengths <- m_a_ids



# Populate the matrices --------------------------------------------------------
for (i in 1:nrow(cooneyp)) {
  print(paste0("starting species ", i, " out of ", nrow(cooney)))
  
  # coords for species i -----
  coordsA <- cbPAM[,c("Longitude(x)", "Latitude(y)", paste0(cooneyp[i, "Species.1bl"]))]
  coordsA <- subset(coordsA, coordsA[, 3] == 1)
  coordsA <- data.frame(lon = coordsA[ , 1], lat = coordsA[ , 2])
  coordinates(coordsA) <- c("lon", "lat")
  crs(coordsA) <- crs(LonLat_BirdPAM_raster)
  
  # origin ids for species i -----
  origin_ids <- sample(x = cooneyp$n_pam_cells_sp1[i], size = min(n_origins, cooneyp$n_pam_cells_sp1[i]), replace = F) # sample origins indices in range A
  m_a_ids[i,c(1:length(origin_ids))] <- origin_ids
  
  # get origin coords for species i -----
  coordsO <- data.frame(lon = coordinates(coordsA)[origin_ids, 1], lat = coordinates(coordsA)[origin_ids, 2]) # get origins coords
  
  # populate matrices with info for origins for species i ----------
  m_a_lons[i,c(1:length(origin_ids))] <- coordsO[,"lon"] # lon lat info. 
  m_a_lats[i,c(1:length(origin_ids))] <- coordsO[,"lat"]
  
  m_a_eles[i,c(1:length(origin_ids))] <- extract(x = Elev_raster, y = coordsO) # topography
  m_a_slop[i,c(1:length(origin_ids))] <- extract(x = Slope_raster, y = coordsO)
  m_a_aspe[i,c(1:length(origin_ids))] <- extract(x = Aspect_raster, y = coordsO)
  
  # Thermal
  m_a_tas_M1[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,1] # tas
  m_a_tas_M2[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,2] 
  m_a_tas_M3[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,3] 
  m_a_tas_M4[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,4] 
  m_a_tas_M5[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,5] 
  m_a_tas_M6[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,6] 
  m_a_tas_M7[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,7] 
  m_a_tas_M8[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,8] 
  m_a_tas_M9[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,9] 
  m_a_tas_M10[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,10] 
  m_a_tas_M11[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,11] 
  m_a_tas_M12[i,c(1:length(origin_ids))] <- extract(x = tas, y = coordsO)[,12] 
  
  m_a_tasmax_M1[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,1] # tasmax
  m_a_tasmax_M2[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,2] 
  m_a_tasmax_M3[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,3] 
  m_a_tasmax_M4[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,4] 
  m_a_tasmax_M5[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,5] 
  m_a_tasmax_M6[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,6] 
  m_a_tasmax_M7[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,7] 
  m_a_tasmax_M8[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,8] 
  m_a_tasmax_M9[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,9] 
  m_a_tasmax_M10[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,10] 
  m_a_tasmax_M11[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,11] 
  m_a_tasmax_M12[i,c(1:length(origin_ids))] <- extract(x = tasmax, y = coordsO)[,12] 
  
  m_a_tasmin_M1[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,1] # tasmin
  m_a_tasmin_M2[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,2] 
  m_a_tasmin_M3[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,3] 
  m_a_tasmin_M4[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,4] 
  m_a_tasmin_M5[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,5] 
  m_a_tasmin_M6[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,6] 
  m_a_tasmin_M7[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,7] 
  m_a_tasmin_M8[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,8] 
  m_a_tasmin_M9[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,9] 
  m_a_tasmin_M10[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,10] 
  m_a_tasmin_M11[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,11] 
  m_a_tasmin_M12[i,c(1:length(origin_ids))] <- extract(x = tasmin, y = coordsO)[,12] 
  
  m_a_tasrng_M1[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,1] # tasrng
  m_a_tasrng_M2[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,2] 
  m_a_tasrng_M3[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,3] 
  m_a_tasrng_M4[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,4] 
  m_a_tasrng_M5[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,5] 
  m_a_tasrng_M6[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,6] 
  m_a_tasrng_M7[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,7] 
  m_a_tasrng_M8[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,8] 
  m_a_tasrng_M9[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,9] 
  m_a_tasrng_M10[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,10] 
  m_a_tasrng_M11[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,11] 
  m_a_tasrng_M12[i,c(1:length(origin_ids))] <- extract(x = tasrng, y = coordsO)[,12] 

  # Precipitation
  m_a_pcp_P1[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,1] # pcp
  m_a_pcp_P2[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,2] 
  m_a_pcp_P3[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,3] 
  m_a_pcp_P4[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,4] 
  m_a_pcp_P5[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,5] 
  m_a_pcp_P6[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,6] 
  m_a_pcp_P7[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,7] 
  m_a_pcp_P8[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,8] 
  m_a_pcp_P9[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,9] 
  m_a_pcp_P10[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,10] 
  m_a_pcp_P11[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,11] 
  m_a_pcp_P12[i,c(1:length(origin_ids))] <- extract(x = pcp, y = coordsO)[,12] 
  
  m_a_pcpmax_P1[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,1] # pcpmax
  m_a_pcpmax_P2[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,2] 
  m_a_pcpmax_P3[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,3] 
  m_a_pcpmax_P4[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,4] 
  m_a_pcpmax_P5[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,5] 
  m_a_pcpmax_P6[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,6] 
  m_a_pcpmax_P7[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,7] 
  m_a_pcpmax_P8[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,8] 
  m_a_pcpmax_P9[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,9] 
  m_a_pcpmax_P10[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,10] 
  m_a_pcpmax_P11[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,11] 
  m_a_pcpmax_P12[i,c(1:length(origin_ids))] <- extract(x = pcpmax, y = coordsO)[,12] 
  
  m_a_pcpmin_P1[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,1] # pcpmin
  m_a_pcpmin_P2[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,2] 
  m_a_pcpmin_P3[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,3] 
  m_a_pcpmin_P4[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,4] 
  m_a_pcpmin_P5[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,5] 
  m_a_pcpmin_P6[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,6] 
  m_a_pcpmin_P7[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,7] 
  m_a_pcpmin_P8[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,8] 
  m_a_pcpmin_P9[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,9] 
  m_a_pcpmin_P10[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,10] 
  m_a_pcpmin_P11[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,11] 
  m_a_pcpmin_P12[i,c(1:length(origin_ids))] <- extract(x = pcpmin, y = coordsO)[,12]
  
  m_a_pcprng_P1[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,1] # pcprng
  m_a_pcprng_P2[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,2] 
  m_a_pcprng_P3[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,3] 
  m_a_pcprng_P4[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,4] 
  m_a_pcprng_P5[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,5] 
  m_a_pcprng_P6[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,6] 
  m_a_pcprng_P7[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,7] 
  m_a_pcprng_P8[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,8] 
  m_a_pcprng_P9[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,9] 
  m_a_pcprng_P10[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,10] 
  m_a_pcprng_P11[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,11] 
  m_a_pcprng_P12[i,c(1:length(origin_ids))] <- extract(x = pcprng, y = coordsO)[,12] 
  
}
setwd(wdMainDataFrame)
save(m_a_ids , m_a_lons , m_a_lats , m_a_eles , m_a_slop , m_a_aspe ,
     m_a_tas_M1 , m_a_tasmax_M1 , m_a_tasmin_M1 , m_a_tasrng_M1 ,
     m_a_tas_M2 , m_a_tasmax_M2 , m_a_tasmin_M2 , m_a_tasrng_M2 ,
     m_a_tas_M3 , m_a_tasmax_M3 , m_a_tasmin_M3 , m_a_tasrng_M3 ,
     m_a_tas_M4 , m_a_tasmax_M4 , m_a_tasmin_M4 , m_a_tasrng_M4 ,
     m_a_tas_M5 , m_a_tasmax_M5 , m_a_tasmin_M5 , m_a_tasrng_M5 ,
     m_a_tas_M6 , m_a_tasmax_M6 , m_a_tasmin_M6 , m_a_tasrng_M6 ,
     m_a_tas_M7 , m_a_tasmax_M7 , m_a_tasmin_M7 , m_a_tasrng_M7 ,
     m_a_tas_M8 , m_a_tasmax_M8 , m_a_tasmin_M8 , m_a_tasrng_M8 ,
     m_a_tas_M9 , m_a_tasmax_M9 , m_a_tasmin_M9 , m_a_tasrng_M9 ,
     m_a_tas_M10 , m_a_tasmax_M10 , m_a_tasmin_M10 , m_a_tasrng_M10 ,
     m_a_tas_M11 , m_a_tasmax_M11 , m_a_tasmin_M11 , m_a_tasrng_M11 ,
     m_a_tas_M12 , m_a_tasmax_M12 , m_a_tasmin_M12 , m_a_tasrng_M12 ,
     m_a_pcp_P1 , m_a_pcpmax_P1 , m_a_pcpmin_P1 , m_a_pcprng_P1 ,
     m_a_pcp_P2 , m_a_pcpmax_P2 , m_a_pcpmin_P2 , m_a_pcprng_P2 ,
     m_a_pcp_P3 , m_a_pcpmax_P3 , m_a_pcpmin_P3 , m_a_pcprng_P3 ,
     m_a_pcp_P4 , m_a_pcpmax_P4 , m_a_pcpmin_P4 , m_a_pcprng_P4 ,
     m_a_pcp_P5 , m_a_pcpmax_P5 , m_a_pcpmin_P5 , m_a_pcprng_P5 ,
     m_a_pcp_P6 , m_a_pcpmax_P6 , m_a_pcpmin_P6 , m_a_pcprng_P6 ,
     m_a_pcp_P7 , m_a_pcpmax_P7 , m_a_pcpmin_P7 , m_a_pcprng_P7 ,
     m_a_pcp_P8 , m_a_pcpmax_P8 , m_a_pcpmin_P8 , m_a_pcprng_P8 ,
     m_a_pcp_P9 , m_a_pcpmax_P9 , m_a_pcpmin_P9 , m_a_pcprng_P9 ,
     m_a_pcp_P10 , m_a_pcpmax_P10 , m_a_pcpmin_P10 , m_a_pcprng_P10 ,
     m_a_pcp_P11 , m_a_pcpmax_P11 , m_a_pcpmin_P11 , m_a_pcprng_P11 ,
     m_a_pcp_P12 , m_a_pcpmax_P12 , m_a_pcpmin_P12 , m_a_pcprng_P12 ,
     m_a_bblons_ele , m_a_bblats_ele , m_aB_ele_mcosts , m_aB_ele_plengths ,
     m_a_bblons_tmp , m_a_bblats_tmp , m_aB_tmp_mcosts , m_aB_tmp_plengths ,
     m_a_bblons_pcp , m_a_bblats_pcp , m_aB_pcp_mcosts , m_aB_pcp_plengths ,
     file = "a_mats_UNMANIPULATED.Rdata")



# Chunks for batch run (PAM and contorl file) ----------------------------------
# nchunks <- 10
nchunks <- 200
nTotalOrigins <- sum(!is.na(m_a_ids))
originBreaks <- seq(0, nTotalOrigins, length.out = nchunks + 1)

mybreaks <- c(0)
for(k in 1:nchunks){
  i <- 0
  j <- 0
  while (i < originBreaks[k+1]) {
    j <- j + 1
    i <- i + sum(!is.na(m_a_ids[j,]))
  }
  mybreaks <- c(mybreaks, j)
}
for (i in 1:length(mybreaks)) { # go to nearest even to keep pairs together
  mybreaks[i] <- FindNearestEven(floor(mybreaks[i]), ud = "d")
}
mybreaks
# sum(!is.na(m_a_ids[c((mybreaks[1] + 1): mybreaks[2]),])) # it works!
# sum(!is.na(m_a_ids[c((mybreaks[2] + 1): mybreaks[3]),]))
# sum(!is.na(m_a_ids[c((mybreaks[3] + 1): mybreaks[4]),]))
# sum(!is.na(m_a_ids[c((mybreaks[4] + 1): mybreaks[5]),]))
# sum(!is.na(m_a_ids[c((mybreaks[5] + 1): mybreaks[6]),]))
# sum(!is.na(m_a_ids[c((mybreaks[6] + 1): mybreaks[7]),]))
# sum(!is.na(m_a_ids[c((mybreaks[7] + 1): mybreaks[8]),]))
# sum(!is.na(m_a_ids[c((mybreaks[8] + 1): mybreaks[9]),]))
# sum(!is.na(m_a_ids[c((mybreaks[9] + 1): mybreaks[10]),]))
# sum(!is.na(m_a_ids[c((mybreaks[10] + 1): mybreaks[11]),]))
setwd(wdMainDataFrame)
save(mybreaks, file = "mybreaks.rdata")


rm(Aspect_raster, Slope_raster, Elev_raster, coordsA, coordsO, LonLat_BirdPAM_raster, pcp, pcpmax,
   pcpmin, pcprng, tas, tasmax, tasmin, tasrng, cbPAM); gc(); memory.size()


# break up the matrices into chunkes to speed batch runs -----------------------
matlist <- c("m_a_ids" , "m_a_lons" , "m_a_lats" , "m_a_eles" , "m_a_slop" , "m_a_aspe" ,
             "m_a_tas_M1" , "m_a_tasmax_M1" , "m_a_tasmin_M1" , "m_a_tasrng_M1" ,
             "m_a_tas_M2" , "m_a_tasmax_M2" , "m_a_tasmin_M2" , "m_a_tasrng_M2" ,
             "m_a_tas_M3" , "m_a_tasmax_M3" , "m_a_tasmin_M3" , "m_a_tasrng_M3" ,
             "m_a_tas_M4" , "m_a_tasmax_M4" , "m_a_tasmin_M4" , "m_a_tasrng_M4" ,
             "m_a_tas_M5" , "m_a_tasmax_M5" , "m_a_tasmin_M5" , "m_a_tasrng_M5" ,
             "m_a_tas_M6" , "m_a_tasmax_M6" , "m_a_tasmin_M6" , "m_a_tasrng_M6" ,
             "m_a_tas_M7" , "m_a_tasmax_M7" , "m_a_tasmin_M7" , "m_a_tasrng_M7" ,
             "m_a_tas_M8" , "m_a_tasmax_M8" , "m_a_tasmin_M8" , "m_a_tasrng_M8" ,
             "m_a_tas_M9" , "m_a_tasmax_M9" , "m_a_tasmin_M9" , "m_a_tasrng_M9" ,
             "m_a_tas_M10" , "m_a_tasmax_M10" , "m_a_tasmin_M10" , "m_a_tasrng_M10" ,
             "m_a_tas_M11" , "m_a_tasmax_M11" , "m_a_tasmin_M11" , "m_a_tasrng_M11" ,
             "m_a_tas_M12" , "m_a_tasmax_M12" , "m_a_tasmin_M12" , "m_a_tasrng_M12" ,
             "m_a_pcp_P1" , "m_a_pcpmax_P1" , "m_a_pcpmin_P1" , "m_a_pcprng_P1" , 
             "m_a_pcp_P2" , "m_a_pcpmax_P2" , "m_a_pcpmin_P2" , "m_a_pcprng_P2" ,
             "m_a_pcp_P3" , "m_a_pcpmax_P3" , "m_a_pcpmin_P3" , "m_a_pcprng_P3" ,
             "m_a_pcp_P4" , "m_a_pcpmax_P4" , "m_a_pcpmin_P4" , "m_a_pcprng_P4" ,
             "m_a_pcp_P5" , "m_a_pcpmax_P5" , "m_a_pcpmin_P5" , "m_a_pcprng_P5" ,
             "m_a_pcp_P6" , "m_a_pcpmax_P6" , "m_a_pcpmin_P6" , "m_a_pcprng_P6" ,
             "m_a_pcp_P7" , "m_a_pcpmax_P7" , "m_a_pcpmin_P7" , "m_a_pcprng_P7" ,
             "m_a_pcp_P8" , "m_a_pcpmax_P8" , "m_a_pcpmin_P8" , "m_a_pcprng_P8" ,
             "m_a_pcp_P9" , "m_a_pcpmax_P9" , "m_a_pcpmin_P9" , "m_a_pcprng_P9" ,
             "m_a_pcp_P10" , "m_a_pcpmax_P10" , "m_a_pcpmin_P10" , "m_a_pcprng_P10" ,
             "m_a_pcp_P11" , "m_a_pcpmax_P11" , "m_a_pcpmin_P11" , "m_a_pcprng_P11" ,
             "m_a_pcp_P12" , "m_a_pcpmax_P12" , "m_a_pcpmin_P12" , "m_a_pcprng_P12" ,
             "m_a_bblons_ele" , "m_a_bblats_ele" , "m_aB_ele_mcosts" , "m_aB_ele_plengths" ,
             "m_a_bblons_tmp" , "m_a_bblats_tmp" , "m_aB_tmp_mcosts" , "m_aB_tmp_plengths" ,
             "m_a_bblons_pcp" , "m_a_bblats_pcp" , "m_aB_pcp_mcosts" , "m_aB_pcp_plengths")


colStart <- 1
colEnd <- 5000
for (i in 1:(length(mybreaks) - 1 )) {
  print(i)
  rowStart <- mybreaks[i] + 1
  rowEnd <- mybreaks[i+1]
  for (j in 1:length(matlist)){
    print(paste0(i, " - ", j))
    temp <- get(matlist[j])
    temp <- temp[rowStart:rowEnd, colStart:colEnd]
    assign(paste0(matlist[j], "_block"), temp)
    rm(temp); gc()
  }
  setwd(wdMainDataFrame)
  save(m_a_ids_block , m_a_lons_block , m_a_lats_block , m_a_eles_block , m_a_slop_block , m_a_aspe_block ,
       m_a_tas_M1_block , m_a_tasmax_M1_block , m_a_tasmin_M1_block , m_a_tasrng_M1_block ,
       m_a_tas_M2_block , m_a_tasmax_M2_block , m_a_tasmin_M2_block , m_a_tasrng_M2_block ,
       m_a_tas_M3_block , m_a_tasmax_M3_block , m_a_tasmin_M3_block , m_a_tasrng_M3_block ,
       m_a_tas_M4_block , m_a_tasmax_M4_block , m_a_tasmin_M4_block , m_a_tasrng_M4_block ,
       m_a_tas_M5_block , m_a_tasmax_M5_block , m_a_tasmin_M5_block , m_a_tasrng_M5_block ,
       m_a_tas_M6_block , m_a_tasmax_M6_block , m_a_tasmin_M6_block , m_a_tasrng_M6_block ,
       m_a_tas_M7_block , m_a_tasmax_M7_block , m_a_tasmin_M7_block , m_a_tasrng_M7_block ,
       m_a_tas_M8_block , m_a_tasmax_M8_block , m_a_tasmin_M8_block , m_a_tasrng_M8_block ,
       m_a_tas_M9_block , m_a_tasmax_M9_block , m_a_tasmin_M9_block , m_a_tasrng_M9_block ,
       m_a_tas_M10_block , m_a_tasmax_M10_block , m_a_tasmin_M10_block , m_a_tasrng_M10_block ,
       m_a_tas_M11_block , m_a_tasmax_M11_block , m_a_tasmin_M11_block , m_a_tasrng_M11_block ,
       m_a_tas_M12_block , m_a_tasmax_M12_block , m_a_tasmin_M12_block , m_a_tasrng_M12_block ,
       m_a_pcp_P1_block , m_a_pcpmax_P1_block , m_a_pcpmin_P1_block , m_a_pcprng_P1_block ,
       m_a_pcp_P2_block , m_a_pcpmax_P2_block , m_a_pcpmin_P2_block , m_a_pcprng_P2_block ,
       m_a_pcp_P3_block , m_a_pcpmax_P3_block , m_a_pcpmin_P3_block , m_a_pcprng_P3_block ,
       m_a_pcp_P4_block , m_a_pcpmax_P4_block , m_a_pcpmin_P4_block , m_a_pcprng_P4_block ,
       m_a_pcp_P5_block , m_a_pcpmax_P5_block , m_a_pcpmin_P5_block , m_a_pcprng_P5_block ,
       m_a_pcp_P6_block , m_a_pcpmax_P6_block , m_a_pcpmin_P6_block , m_a_pcprng_P6_block ,
       m_a_pcp_P7_block , m_a_pcpmax_P7_block , m_a_pcpmin_P7_block , m_a_pcprng_P7_block ,
       m_a_pcp_P8_block , m_a_pcpmax_P8_block , m_a_pcpmin_P8_block , m_a_pcprng_P8_block ,
       m_a_pcp_P9_block , m_a_pcpmax_P9_block , m_a_pcpmin_P9_block , m_a_pcprng_P9_block ,
       m_a_pcp_P10_block , m_a_pcpmax_P10_block , m_a_pcpmin_P10_block , m_a_pcprng_P10_block ,
       m_a_pcp_P11_block , m_a_pcpmax_P11_block , m_a_pcpmin_P11_block , m_a_pcprng_P11_block ,
       m_a_pcp_P12_block , m_a_pcpmax_P12_block , m_a_pcpmin_P12_block , m_a_pcprng_P12_block ,
       m_a_bblons_ele_block , m_a_bblats_ele_block , m_aB_ele_mcosts_block , m_aB_ele_plengths_block ,
       m_a_bblons_tmp_block , m_a_bblats_tmp_block , m_aB_tmp_mcosts_block , m_aB_tmp_plengths_block ,
       m_a_bblons_pcp_block , m_a_bblats_pcp_block , m_aB_pcp_mcosts_block , m_aB_pcp_plengths_block ,
       file = paste0("a_mats_block", i, '.rdata'))
  
  rm(m_a_ids_block , m_a_lons_block , m_a_lats_block , m_a_eles_block , m_a_slop_block , m_a_aspe_block ,
       m_a_tas_M1_block , m_a_tasmax_M1_block , m_a_tasmin_M1_block , m_a_tasrng_M1_block ,
       m_a_tas_M2_block , m_a_tasmax_M2_block , m_a_tasmin_M2_block , m_a_tasrng_M2_block ,
       m_a_tas_M3_block , m_a_tasmax_M3_block , m_a_tasmin_M3_block , m_a_tasrng_M3_block ,
       m_a_tas_M4_block , m_a_tasmax_M4_block , m_a_tasmin_M4_block , m_a_tasrng_M4_block ,
       m_a_tas_M5_block , m_a_tasmax_M5_block , m_a_tasmin_M5_block , m_a_tasrng_M5_block ,
       m_a_tas_M6_block , m_a_tasmax_M6_block , m_a_tasmin_M6_block , m_a_tasrng_M6_block ,
       m_a_tas_M7_block , m_a_tasmax_M7_block , m_a_tasmin_M7_block , m_a_tasrng_M7_block ,
       m_a_tas_M8_block , m_a_tasmax_M8_block , m_a_tasmin_M8_block , m_a_tasrng_M8_block ,
       m_a_tas_M9_block , m_a_tasmax_M9_block , m_a_tasmin_M9_block , m_a_tasrng_M9_block ,
       m_a_tas_M10_block , m_a_tasmax_M10_block , m_a_tasmin_M10_block , m_a_tasrng_M10_block ,
       m_a_tas_M11_block , m_a_tasmax_M11_block , m_a_tasmin_M11_block , m_a_tasrng_M11_block ,
       m_a_tas_M12_block , m_a_tasmax_M12_block , m_a_tasmin_M12_block , m_a_tasrng_M12_block ,
       m_a_pcp_P1_block , m_a_pcpmax_P1_block , m_a_pcpmin_P1_block , m_a_pcprng_P1_block ,
       m_a_pcp_P2_block , m_a_pcpmax_P2_block , m_a_pcpmin_P2_block , m_a_pcprng_P2_block ,
       m_a_pcp_P3_block , m_a_pcpmax_P3_block , m_a_pcpmin_P3_block , m_a_pcprng_P3_block ,
       m_a_pcp_P4_block , m_a_pcpmax_P4_block , m_a_pcpmin_P4_block , m_a_pcprng_P4_block ,
       m_a_pcp_P5_block , m_a_pcpmax_P5_block , m_a_pcpmin_P5_block , m_a_pcprng_P5_block ,
       m_a_pcp_P6_block , m_a_pcpmax_P6_block , m_a_pcpmin_P6_block , m_a_pcprng_P6_block ,
       m_a_pcp_P7_block , m_a_pcpmax_P7_block , m_a_pcpmin_P7_block , m_a_pcprng_P7_block ,
       m_a_pcp_P8_block , m_a_pcpmax_P8_block , m_a_pcpmin_P8_block , m_a_pcprng_P8_block ,
       m_a_pcp_P9_block , m_a_pcpmax_P9_block , m_a_pcpmin_P9_block , m_a_pcprng_P9_block ,
       m_a_pcp_P10_block , m_a_pcpmax_P10_block , m_a_pcpmin_P10_block , m_a_pcprng_P10_block ,
       m_a_pcp_P11_block , m_a_pcpmax_P11_block , m_a_pcpmin_P11_block , m_a_pcprng_P11_block ,
       m_a_pcp_P12_block , m_a_pcpmax_P12_block , m_a_pcpmin_P12_block , m_a_pcprng_P12_block ,
       m_a_bblons_ele_block , m_a_bblats_ele_block , m_aB_ele_mcosts_block , m_aB_ele_plengths_block ,
       m_a_bblons_tmp_block , m_a_bblats_tmp_block , m_aB_tmp_mcosts_block , m_aB_tmp_plengths_block ,
       m_a_bblons_pcp_block , m_a_bblats_pcp_block , m_aB_pcp_mcosts_block , m_aB_pcp_plengths_block); gc() 
}
rm(m_a_ids , m_a_lons , m_a_lats , m_a_eles , m_a_slop , m_a_aspe ,
   m_a_tas_M1 , m_a_tasmax_M1 , m_a_tasmin_M1 , m_a_tasrng_M1 ,
   m_a_tas_M2 , m_a_tasmax_M2 , m_a_tasmin_M2 , m_a_tasrng_M2 ,
   m_a_tas_M3 , m_a_tasmax_M3 , m_a_tasmin_M3 , m_a_tasrng_M3 ,
   m_a_tas_M4 , m_a_tasmax_M4 , m_a_tasmin_M4 , m_a_tasrng_M4 ,
   m_a_tas_M5 , m_a_tasmax_M5 , m_a_tasmin_M5 , m_a_tasrng_M5 ,
   m_a_tas_M6 , m_a_tasmax_M6 , m_a_tasmin_M6 , m_a_tasrng_M6 ,
   m_a_tas_M7 , m_a_tasmax_M7 , m_a_tasmin_M7 , m_a_tasrng_M7 ,
   m_a_tas_M8 , m_a_tasmax_M8 , m_a_tasmin_M8 , m_a_tasrng_M8 ,
   m_a_tas_M9 , m_a_tasmax_M9 , m_a_tasmin_M9 , m_a_tasrng_M9 ,
   m_a_tas_M10 , m_a_tasmax_M10 , m_a_tasmin_M10 , m_a_tasrng_M10 ,
   m_a_tas_M11 , m_a_tasmax_M11 , m_a_tasmin_M11 , m_a_tasrng_M11 ,
   m_a_tas_M12 , m_a_tasmax_M12 , m_a_tasmin_M12 , m_a_tasrng_M12 ,
   m_a_pcp_P1 , m_a_pcpmax_P1 , m_a_pcpmin_P1 , m_a_pcprng_P1 ,
   m_a_pcp_P2 , m_a_pcpmax_P2 , m_a_pcpmin_P2 , m_a_pcprng_P2 ,
   m_a_pcp_P3 , m_a_pcpmax_P3 , m_a_pcpmin_P3 , m_a_pcprng_P3 ,
   m_a_pcp_P4 , m_a_pcpmax_P4 , m_a_pcpmin_P4 , m_a_pcprng_P4 ,
   m_a_pcp_P5 , m_a_pcpmax_P5 , m_a_pcpmin_P5 , m_a_pcprng_P5 ,
   m_a_pcp_P6 , m_a_pcpmax_P6 , m_a_pcpmin_P6 , m_a_pcprng_P6 ,
   m_a_pcp_P7 , m_a_pcpmax_P7 , m_a_pcpmin_P7 , m_a_pcprng_P7 ,
   m_a_pcp_P8 , m_a_pcpmax_P8 , m_a_pcpmin_P8 , m_a_pcprng_P8 ,
   m_a_pcp_P9 , m_a_pcpmax_P9 , m_a_pcpmin_P9 , m_a_pcprng_P9 ,
   m_a_pcp_P10 , m_a_pcpmax_P10 , m_a_pcpmin_P10 , m_a_pcprng_P10 ,
   m_a_pcp_P11 , m_a_pcpmax_P11 , m_a_pcpmin_P11 , m_a_pcprng_P11 ,
   m_a_pcp_P12 , m_a_pcpmax_P12 , m_a_pcpmin_P12 , m_a_pcprng_P12 ,
   m_a_bblons_ele , m_a_bblats_ele , m_aB_ele_mcosts , m_aB_ele_plengths ,
   m_a_bblons_tmp , m_a_bblats_tmp , m_aB_tmp_mcosts , m_aB_tmp_plengths ,
   m_a_bblons_pcp , m_a_bblats_pcp , m_aB_pcp_mcosts , m_aB_pcp_plengths); gc()




# break up the cbPAM into chunks to speed up the run ---------------------------
setwd(wdPAM); load("cbPAM.rdata") # bring the pam back in. had to remove for a bit to have enough memory for the prev piece of code.
cbPAM <- cbPAM[,c("Longitude(x)", "Latitude(y)", cooneyp$Species.1bl)]
for (i in 1:nchunks) {
  assign(paste0("cbPAM", i), cbPAM[,c("Longitude(x)", "Latitude(y)", cooneyp$Species.1bl[(mybreaks[i]+1):mybreaks[i+1]])]);
  save(list=paste0("cbPAM", i), file = paste0("cbPAM", i, '.rdata'))
  rm(list=paste0("cbPAM", i))
}



# break up cooney as well. -----------------------------------------------------
setwd(wdMainDataFrame); load("cooneyp.rdata") # bring the pam back in. had to remove for a bit to have enough memory for the prev piece of code.
for (i in 1:nchunks) {
  assign(paste0("cooneyp", i), cooneyp[(mybreaks[i]+1):mybreaks[i+1],])
  
  save(list=paste0("cooneyp", i), file = paste0("cooneyp", i, '.rdata'))
  rm(list=paste0("cooneyp", i))
}






















