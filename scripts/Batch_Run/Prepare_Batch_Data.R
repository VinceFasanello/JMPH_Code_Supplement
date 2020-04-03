# clear workspace and load all required packages -------------------------------
rm(list=ls())
options(scipen=999)
require(rgdal)
# require(dplyr)
# require(movecost)
require(sp)
require(raster)
# require(gdistance)
require(reshape2)
# library(broom)
# library(ggplot2)
# require(viridis)
# require(gridExtra)
# require(mapproj)

# open and concatonate all results file sets -------------------------------------------
inwd <- "~/Box Sync/JMPH_2020/data/Batch_Run"
setwd(inwd)
outputs <- c("mydata", "m_a_ids", "m_a_lons", "m_a_lats", "m_a_eles", "m_a_bblons", 
             "m_a_bblats", "m_a_bbeles", "m_aB_mcosts", "m_aB_plengths")
slices <- 10
for (i in 1:slices){
  if(i < 10){load(file = paste0("00", i, "/00", i, "_results_100ptsea.rdata"))}
  if(i == 10){load(file = paste0("0", i, "/0", i, "_results_100ptsea.rdata"))}
  keeprows <- is.na(m_a_ids[,1]); keeprows <- as.integer(which(keeprows == F)) # get the ids of the rows to retain
  for (j in outputs) {
    if(i == 1){
      assign(paste0(j, "_ag"), get(j)[keeprows,])
    } else {
      assign(paste0(j, "_ag"), rbind(get(paste0(j, "_ag")), get(j)[keeprows,]))
    }
  }
  rm(mydata, m_a_ids, m_a_lons, m_a_lats, m_a_eles, m_a_bblons, 
     m_a_bblats, m_a_bbeles, m_aB_mcosts, m_aB_plengths)
}
mymelt <- cbind(mydata_ag, m_a_ids_ag)

# remove problematic paths.
# m_aB_mcosts_ag[m_aB_mcosts_ag == "sPath_extentpoint"] <- NA; mode(m_aB_mcosts_ag) <- "numeric" # more conservative, assumes these are problem data. 
# m_aB_plengths_ag[m_aB_plengths_ag == "sPath_extentpoint"] <- NA; mode(m_aB_plengths_ag) <- "numeric"
m_aB_mcosts_ag[m_aB_mcosts_ag == "sPath_extentpoint"] <- 0; mode(m_aB_mcosts_ag) <- "numeric" # less conservative, assumes thesea re all sympatric start ends
m_aB_plengths_ag[m_aB_plengths_ag == "sPath_extentpoint"] <- 0; mode(m_aB_plengths_ag) <- "numeric"


# melt to create a long format data frame --------------------------------------
# start the dataframe...
mymelt <- melt(mymelt, id.vars = colnames(mymelt)[1:ncol(mydata_ag)])
mymelt <- mymelt[,c(1:(ncol(mymelt) - 2), ncol(mymelt))]
colnames(mymelt)[ncol(mymelt)] <- "m_a_ids_ag" # this column is the id 1:100 for polygon A starting location ai


# repeat for the other variables....
outputs <- paste0(outputs, "_ag")
for (i in outputs[3:length(outputs)]) {
  mymeltt <- as.data.frame(get(i)); mymeltt$sp <- rownames(mymeltt); mymeltt <- melt(mymeltt, id.vars = "sp")
  mymelt <- cbind(mymelt, mymeltt[,3]); colnames(mymelt)[ncol(mymelt)] <- i
}
mymelt <- mymelt[!is.na(mymelt$m_aB_mcosts_ag),] # remove NA entries



# add richness information to each row ----------------------------------------
setwd(inwd)
load(file = "rasterpam.rdata")

# add to our dataframe
mymelt$div <- NA
for (i in 1:nrow(mymelt)) {
  tlon <- mymelt$m_a_lons_ag[i]
  tlat <- mymelt$m_a_lats_ag[i]
  if(!is.na(tlon) & !is.na(tlat)){
    mymelt$div[i] <- extract(rasterpam, coordinates(cbind(tlon, tlat )))
  }
  
}

# read the bioregion shapefile -------------------------------------------------
setwd("~/Downloads")
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"/official/") , 
  layer="wwf_terr_ecos",
  verbose=FALSE
)

# reproject the bioregion file and rasterize --- 
my_spdf <- spTransform(my_spdf, crs(rasterpam))
spdfdata <- my_spdf@data # save the dataframe too
str(spdfdata)
spdfdata$ECO_NAME <- as.character(spdfdata$ECO_NAME)
spdfdata$REALM <- as.character(spdfdata$REALM)
spdfdata$G200_REGIO <- as.character(spdfdata$G200_REGIO)
spdfdata$eco_code <- as.character(spdfdata$eco_code)

new_spdf <- raster::rasterize(x = my_spdf, y = rasterpam, field = "OBJECTID") # rasterize
# plot(new_spdf)



# add Bioregion information to each row ----------------------------------------
lonlats <- as.data.frame(cbind(mymelt$m_a_lons_ag, mymelt$m_a_lats_ag))
colnames(lonlats) <- c("lon", "lat")
mymelt$bioregion <- NA
for (i in 1:nrow(mymelt)) {
  mymelt$bioregion[i] <- extract(x = new_spdf, y = lonlats[i,])
  
}

adds <- colnames(spdfdata)
for (i in 1:length(adds)){
  mymelt[, adds[i]] <- NA
}
for (i in 1:nrow(mymelt)) {
  for (j in 1:length(colnames(spdfdata))) {
    if (!is.na(mymelt[i, "bioregion"])){
      mymelt[i, colnames(spdfdata)[j]] <- spdfdata[which(spdfdata$OBJECTID == mymelt$bioregion[i]),colnames(spdfdata)[j]]
      
    } else {
      mymelt[i, colnames(spdfdata)[j]] <- NA
    }
    
  }
}


# SAVE a dataframe for analysis by species with rows for each originxsp  ----------------------------------
# 
# 
mydata <- mymelt
setwd(inwd)
save(mydata, file = "lcp_elevation_results_1rowper_origin.rdata") # use "Species.1bl" for grouping variable
#
#
# ----------------------------------------------------------------------------------------------------------


# create a dataframe for analysis by pair rather than by species -------------------------------------------
# 
# 
# not necessary -- use "pairID" rather than "Species.1bl" for grouping variable for these analyses!
#
#
# ----------------------------------------------------------------------------------------------------------





# create a BY CELL dataframe that averages data from all species in that cell ----------------------------------
# 
# 
mymelt$cell <- paste0(mymelt$m_a_lons_ag, ",", mymelt$m_a_lats_ag)
myworld <- mymelt
myworld <- mymelt[,c("m_a_lons_ag", "m_a_lats_ag", "m_a_eles_ag", "m_aB_mcosts_ag", "m_aB_plengths_ag", "n_pam_cells_sp1", "div", "cell", 
                     "bioregion", "OBJECTID", "AREA" , "PERIMETER", "ECO_NAME", "REALM", "BIOME", "ECO_NUM", "ECO_ID", "ECO_SYM", "GBL_STAT",
                     "G200_REGIO", "G200_NUM", "G200_BIOME", "G200_STAT", "Shape_Leng", "Shape_Area" ,"area_km2", "eco_code",                 
                     "PER_area", "PER_area_1","PER_area_2" )]
myworldg <- NA
for (i in unique(myworld$cell)) {
  # pull out only those entries for this unique lon x lat combination -----
  lonlat <- i
  mytemp <- myworld[myworld$cell == lonlat,]
  
  # calculate some (potentially) useful fields -----
  mytemp$n_sp_sampled <- nrow(mytemp) # number of species with data for this cell
  mytemp$bird_div <- mean(mytemp$div, na.rm = T) # diversity for this cell (using mean, but all values are identical here)
  
  # species RANGE SIZE info for path starting in this cell
  mytemp$mean_rs <- mean(mytemp$n_pam_cells_sp1, na.rm = T)
  mytemp$min_rs <- min(mytemp$n_pam_cells_sp1, na.rm = T)
  mytemp$max_rs <- max(mytemp$n_pam_cells_sp1, na.rm = T)
  mytemp$var_rs <- var(mytemp$n_pam_cells_sp1, na.rm = T)
  
  # path COST summary info for paths starting in this cell
  mytemp$m_aB_mcosts_ag <- mean(mytemp$m_aB_mcosts_ag, na.rm = T) # legacy field, delete when correct name is implemented.
  mytemp$mean_cost <- mean(mytemp$m_aB_mcosts_ag, na.rm = T)
  mytemp$min_cost <- min(mytemp$m_aB_mcosts_ag, na.rm = T)
  mytemp$max_cost <- max(mytemp$m_aB_mcosts_ag, na.rm = T)
  mytemp$var_cost <- var(mytemp$m_aB_mcosts_ag, na.rm = T)
  
  # path LENGTH summary info for paths starting in this cell
  mytemp$m_aB_plengths_ag <- mean(mytemp$m_aB_plengths_ag, na.rm = T) # legacy field, delete when correct name is implemented.
  mytemp$mean_len <- mean(mytemp$m_aB_plengths_ag, na.rm = T)
  mytemp$min_len <- min(mytemp$m_aB_plengths_ag, na.rm = T)
  mytemp$max_len <- max(mytemp$m_aB_plengths_ag, na.rm = T)
  mytemp$var_len <- var(mytemp$m_aB_plengths_ag, na.rm = T)
  
  mytemp <- mytemp[1,]
  myworldg <- rbind(myworldg, mytemp)
}
myworld <- myworldg[2:nrow(myworldg),]

# add more informative names to...
myworld$REALM[which(myworld$REALM == "AA")] <- "Australasia" # realms
myworld$REALM[which(myworld$REALM == "AN")] <- "Antarctic"
myworld$REALM[which(myworld$REALM == "AT")] <- "Afrotropics"
myworld$REALM[which(myworld$REALM == "IM")] <- "IndoMalay"
myworld$REALM[which(myworld$REALM == "NA")] <- "Nearctic"
myworld$REALM[which(myworld$REALM == "NT")] <- "Neotropics"
myworld$REALM[which(myworld$REALM == "OC")] <- "Oceania"
myworld$REALM[which(myworld$REALM == "PA")] <- "Palearctic"

myworld$BIOME[which(myworld$BIOME == 1)] <- "Tropical-Subtropical-Moist-Broadleaf-Forests" # biomes
myworld$BIOME[which(myworld$BIOME == 2)] <- "Tropical-Subtropical-Dry-Broadleaf-Forests"
myworld$BIOME[which(myworld$BIOME == 3)] <- "Tropical-Subtropical-Coniferous-Forests"
myworld$BIOME[which(myworld$BIOME == 4)] <- "Temperate-Broadleaf-Mixed-Forests"
myworld$BIOME[which(myworld$BIOME == 5)] <- "Temperate-Conifer-Forests"
myworld$BIOME[which(myworld$BIOME == 6)] <- "Boreal-Forests-Taiga"
myworld$BIOME[which(myworld$BIOME == 7)] <- "Tropical-Subtropical-Grasslands-Savannas-Shrublands"
myworld$BIOME[which(myworld$BIOME == 8)] <- "Temperate-Grasslands-Savannas-Shrublands"
myworld$BIOME[which(myworld$BIOME == 9)] <- "Flooded-Grasslands-Savannas"
myworld$BIOME[which(myworld$BIOME == 10)] <- "Montane-Grasslands-Shrublands"
myworld$BIOME[which(myworld$BIOME == 11)] <- "Tundra"
myworld$BIOME[which(myworld$BIOME == 12)] <- "Mediterranean-Forests-Woodlands-Scrub"
myworld$BIOME[which(myworld$BIOME == 13)] <- "Deserts-Xeric-Shrublands"
myworld$BIOME[which(myworld$BIOME == 14)] <- "Mangroves"

# nrow(myworld[which(myworld$BIOME == 98),]) # strange unassinged values (don't correspond to a lot of data though, so probably safe to omit or lump...)
# nrow(myworld[which(myworld$BIOME == 99),])
myworld$BIOME[which(myworld$BIOME == 98)] <- NA
myworld$BIOME[which(myworld$BIOME == 99)] <- NA

# save
setwd(inwd)
save(myworld, file = "lcp_elevation_results_1rowper_sampledgridcell.rdata")
#
#
# --------------------------------------------------------------------------------------------------------------
