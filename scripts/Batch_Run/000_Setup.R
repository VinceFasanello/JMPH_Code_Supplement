# clear workspace and load all required packages -------------------------------
rm(list=ls())
require(rgdal)
require(raster)
require(dplyr)
require(movecost)
require(sp)
require(raster)
require(gdistance)



# get sources ------------------------------------------------------------------
setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); source("1_getdata_movecost_fxn.R")
rm(movecostAngela, movecostAngela_vOCTDontAdapt)


# Load the PAM -----------------------------------------------------------------
setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); load(file="BirdPAM.rdata")
pamMAT <- myPAM$Presence_and_Absence_Matrix
rasterpam <- myPAM$Richness_Raster
save(rasterpam, file = "rasterpam.rdata")
rm(myPAM) #memory
crsrasterpam <- crs(rasterpam)


# Load and handle the Elevation data -------------------------------------------
setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); load("elevRaster_use.rdata")
elev_raster <- projectRaster(from = Elev_raster, to = rasterpam) # project elevation as raster pam - so coordinates match
# values(elev_raster)[1] # first cell is an NA before changing NAs to 0
values(elev_raster)[is.na(values(elev_raster))] <- 0
elev_raster_scaled <- elev_raster; values(elev_raster_scaled)<-scale(values(elev_raster))
rm(Elev_raster)
save(elev_raster, elev_raster_scaled, file = "000_Setup_elev_raster_outputs.Rdata")



# define the cost function -----------------------------------------------------
calc_EDraster <- function(x, output, ref_value, value_map) {
  coords_target <- data.frame(lon = x[1], lat = x[2])
  coordinates(coords_target)<-c("lon", "lat")
  temp_value <- extract(x = value_map, y = coords_target)
  if(!is.na(ref_value) & !is.na(temp_value)) {
    to_ret <- temp_value - ref_value
  } else {
    to_ret <- NA
  } 
  return(to_ret) # to ret = value of difference in elevation between origin & destination cell (abs happens later)
}



# Create base cost map for transformation --------------------------------------
value_map <- elev_raster_scaled
value_map_coords <- as.data.frame(coordinates(value_map))
ref_value <- values(elev_raster_scaled)[1]
to_add <- apply(value_map_coords, 1, calc_EDraster, ref_value = ref_value, value_map = value_map) # get a vector for the raster cost values
ref_cost_map <- value_map
values(ref_cost_map)[cellFromXY(ref_cost_map, value_map_coords)] <- to_add
setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); save(ref_value, ref_cost_map, file = "ref_cost_map_&_value.Rdata")
rm(value_map, value_map_coords, to_add)



# Load and handle the pairs data -----------------------------------------------
# cooney dataset ---------------------------------
setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); cooneyd <- read.csv("Cooney_Pair_Data.csv", stringsAsFactors = F)
cooneyd <- cooneyd[cooneyd$Tree == "mcc",]; cooneyd <- cooneyd[,which(colnames(cooneyd) != "Tree")]
cooneyd$pairID <- seq(1, nrow(cooneyd))
cooneydi <- cooneyd
cooneydi$Species.1 <- cooneyd$Species.2
cooneydi$Species.2 <- cooneyd$Species.1
cooneyd <- rbind(cooneyd, cooneydi); rm(cooneydi)

# name matching ----------------------------------
setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); load(file="Matched_Names.Rdata")
matchednames <- matchednames[matchednames$dup == F,] # remove entries where two cooney species map to the same latin name / names
matchednames_2bl <- matchednames[!is.na(matchednames$a_BL_latin_2),] # pull out entries where 1 cooney name has two latin names
matchednames <- matchednames[is.na(matchednames$a_BL_latin_2),] # this is a clean dataset with entriew where one cooney name matches a single bl name.
cooneyd <- cooneyd[cooneyd$Species.1 %in% matchednames$cooney_name & cooneyd$Species.2 %in% matchednames$cooney_name,] # subset cooneyd to only those entries where both names in pair are in the clean dataset
cooneyd$Species.1bl <- NA # add a column to house the new bl names for the cooneyd species 1
cooneyd$Species.2bl <- NA # add a column to house the new bl names for the cooneyd species 2
for (i in 1:nrow(cooneyd)) { # populate the new names columns with the bl names that match the pam
  cooneyd$Species.1bl[i] <- matchednames$a_BL_latin_1[matchednames$cooney_name == cooneyd$Species.1[i]]
  cooneyd$Species.2bl[i] <- matchednames$a_BL_latin_1[matchednames$cooney_name == cooneyd$Species.2[i]]
}
cooneyd[,"Species.1bl"] <- gsub("_", " ", cooneyd[,"Species.1bl"]) # replace underscores with spaces for the bl names that match the pam
cooneyd[,"Species.2bl"] <- gsub("_", " ", cooneyd[,"Species.2bl"])
cooneyd <- cooneyd[cooneyd$Species.1bl %in% colnames(pamMAT) & cooneyd$Species.2bl %in% colnames(pamMAT),] # subset cooneyd to only those entries with name matches in pamMAT



# Calculate lat, ele, and overlap for all pairs --------------------------------
for (i in 1:nrow(cooneyd)) {
  mytemp <- pamMAT [,c("Longitude(x)", "Latitude(y)" ,paste0(cooneyd[i,"Species.1bl"]), paste0(cooneyd[i,"Species.2bl"]))] # subset
  
  range1<-subset(mytemp, mytemp[,3]==1) # get species ranges
  range2<-subset(mytemp, mytemp[,4]==1)
  
  coords1<-data.frame(lon=range1[,1], lat=range1[,2]); coordinates(coords1)<-c("lon","lat"); crs(coords1)<-crs(rasterpam) # get coordinates
  coords2<-data.frame(lon=range2[,1], lat=range2[,2]); coordinates(coords2)<-c("lon","lat"); crs(coords2)<-crs(rasterpam)
  
  elev_1<-as.data.frame(extract(x=elev_raster, y=coords1)); elev_1$sp <- "sp1"; colnames(elev_1) <- c("elev", "sp") # get elevation dfs
  elev_2<-as.data.frame(extract(x=elev_raster, y=coords2)); elev_2$sp <- "sp2"; colnames(elev_2) <- c("elev", "sp")
  elevs <- rbind(elev_1, elev_2)
  
  lat_1<-as.data.frame(coords1$lat); lat_1$sp <- "sp1"; colnames(lat_1) <- c("latitude", "sp") # get latitude dfs
  lat_2<-as.data.frame(coords2$lat); lat_2$sp <- "sp2"; colnames(lat_2) <- c("latitude", "sp")
  lats <- rbind(lat_1, lat_2)
  
  cooneyd$min_lat_sp1[i] <- min(lat_1$latitude, na.rm = T); cooneyd$max_lat_sp1[i] <- max(lat_1$latitude, na.rm = T) # lat calcs
  cooneyd$min_lat_sp2[i] <- min(lat_2$latitude, na.rm = T);cooneyd$max_lat_sp2[i] <- max(lat_2$latitude, na.rm = T)
  
  # elev calcs
  cooneyd$min_ele_sp1[i] <- min(elev_1$elev, na.rm = T); cooneyd$max_ele_sp1[i] <- max(elev_1$elev, na.rm = T) # max and min
  cooneyd$min_ele_sp2[i] <- min(elev_2$elev, na.rm = T); cooneyd$max_ele_sp2[i] <- max(elev_2$elev, na.rm = T)
  cooneyd$ele_range_sp1[i] <- abs(cooneyd$min_ele_sp1[i] - cooneyd$max_ele_sp1[i]) # range
  cooneyd$ele_range_sp2[i] <- abs(cooneyd$min_ele_sp2[i] - cooneyd$max_ele_sp2[i])
  cooneyd$ele_maxofmins[i] <- max(cooneyd$min_ele_sp1[i], cooneyd$min_ele_sp2[i], na.rm = T) # overlap % = Union range  / smaller range
  cooneyd$ele_minofmaxs[i] <- min(cooneyd$max_ele_sp1[i], cooneyd$max_ele_sp2[i], na.rm = T)
  cooneyd$ele_ov_range[i] <- cooneyd$ele_minofmaxs[i] - cooneyd$ele_maxofmins[i]
  cooneyd$ele_ov_perc_smrnge[i] <- cooneyd$ele_ov_range[i] / min(cooneyd$ele_range_sp1[i], cooneyd$ele_range_sp2[i], na.rm = T)
  
  # lat calcs
  cooneyd$n_pam_cells_sp1[i] <- length(!is.na(lat_1$latitude)); cooneyd$n_pam_cells_sp2[i] <- length(!is.na(lat_2$latitude)) # max and min
  cooneyd$n_ele_cells_sp1[i] <- length(!is.na(elev_1$elev)); cooneyd$n_ele_cells_sp2[i] <- length(!is.na(elev_2$elev))
  cooneyd$lat_range_sp1[i] <- abs(cooneyd$min_lat_sp1[i] - cooneyd$max_lat_sp1[i]) # range
  cooneyd$lat_range_sp2[i] <- abs(cooneyd$min_lat_sp2[i] - cooneyd$max_lat_sp2[i])
  cooneyd$lat_maxofmins[i] <- max(cooneyd$min_lat_sp1[i], cooneyd$min_lat_sp2[i], na.rm = T) # overlap % = Union range  / smaller range
  cooneyd$lat_minofmaxs[i] <- min(cooneyd$max_lat_sp1[i], cooneyd$max_lat_sp2[i], na.rm = T)
  cooneyd$lat_ov_range[i] <- cooneyd$lat_minofmaxs[i] - cooneyd$lat_maxofmins[i]
  cooneyd$lat_ov_perc_smrnge[i] <- cooneyd$lat_ov_range[i] / min(cooneyd$lat_range_sp1[i], cooneyd$lat_range_sp2[i], na.rm = T)
}
cooneyd$sortcol <- rowMeans(cbind(cooneyd$n_pam_cells_sp1, cooneyd$n_pam_cells_sp2), na.rm = T)
cooneyd <- cooneyd[order(cooneyd$sortcol, cooneyd$pairID),]
cooneyd$sortorder <- seq(1, nrow(cooneyd))
cooneyd_hold <- cooneyd # store unmanipulated frame before subsetting and further operations.
setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); save(cooneyd_hold, file = "cooneyd_hold.Rdata")

# subset to specific datasets --------------------------------------------------
# 1 criteria --------------------------
cooneyd <- cooneyd_hold
nrow(cooneyd[which(cooneyd$ele_ov_perc_smrnge > 0.5 & cooneyd$lat_ov_perc_smrnge > 0.5 & cooneyd$Range.overlap < 0.30),])
cooneyd <- cooneyd[which(cooneyd$ele_ov_perc_smrnge > 0.5 & cooneyd$lat_ov_perc_smrnge > 0.5 & cooneyd$Range.overlap < 0.30),]
mydata <- cooneyd
setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run"); save(cooneyd, file = "cooneyd.Rdata")


# # 2 Criteria --------------------------
# nrow(cooneyd[which(cooneyd$ele_ov_perc_smrnge > 0.99 & cooneyd$lat_ov_perc_smrnge > 0.99 & cooneyd$Range.overlap < 0.05),])
# cooneyd <- cooneyd[which(cooneyd$ele_ov_perc_smrnge > 0.99 & cooneyd$lat_ov_perc_smrnge > 0.99 & cooneyd$Range.overlap < 0.05),]
# mydata <- cooneyd

# subset the pamMAT because it is sooo big ----------------------------------------
neven <- function(x, ud){ # this function finds the nearest even integer, rounding up or down as specified
  if(ud == "d"){
    if(x %% 2 != 0){x <- x - 1}
  } else if(ud == "u"){
    if(x %% 2 != 0){x <- x + 1}
  }
  return(x)
}
mybreaks <- c(0, neven(floor(nrow(cooneyd)/10), "d"), neven(floor(2*nrow(cooneyd)/10), "d"), neven(floor(3*nrow(cooneyd)/10), "d"),
              neven(floor(4*nrow(cooneyd)/10), "d"), neven(floor(5*nrow(cooneyd)/10), "d"),  neven(floor(6*nrow(cooneyd)/10), "d"),
              neven(floor(7*nrow(cooneyd)/10), "d"), neven(floor(8*nrow(cooneyd)/10), "d"),  neven(floor(9*nrow(cooneyd)/10), "d"),
              neven(ceiling(10*nrow(cooneyd)/10), "u"))


setwd("/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run");
pamMAT <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl)]
pamMAT001 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[1]+1):mybreaks[2]])]; save(pamMAT001, file = "pamMAT001.Rdata")
pamMAT002 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[2]+1):mybreaks[3]])]; save(pamMAT002, file = "pamMAT002.Rdata")
pamMAT003 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[3]+1):mybreaks[4]])]; save(pamMAT003, file = "pamMAT003.Rdata")
pamMAT004 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[4]+1):mybreaks[5]])]; save(pamMAT004, file = "pamMAT004.Rdata")
pamMAT005 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[5]+1):mybreaks[6]])]; save(pamMAT005, file = "pamMAT005.Rdata")
pamMAT006 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[6]+1):mybreaks[7]])]; save(pamMAT006, file = "pamMAT006.Rdata")
pamMAT007 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[7]+1):mybreaks[8]])]; save(pamMAT007, file = "pamMAT007.Rdata")
pamMAT008 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[8]+1):mybreaks[9]])]; save(pamMAT008, file = "pamMAT008.Rdata")
pamMAT009 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[9]+1):mybreaks[10]])]; save(pamMAT009, file = "pamMAT009.Rdata")
pamMAT010 <- pamMAT[,c("Longitude(x)", "Latitude(y)", mydata$Species.1bl[(mybreaks[10]+1):mybreaks[11]])]; save(pamMAT010, file = "pamMAT010.Rdata")
rm(pamMAT, pamMAT001, pamMAT002, pamMAT003, pamMAT004, pamMAT005, pamMAT006, pamMAT007, pamMAT008, pamMAT009, pamMAT010)
