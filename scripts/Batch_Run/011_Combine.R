# clear workspace and load all required packages -------------------------------
rm(list=ls())
options(scipen=999)
require(rgdal)
require(raster)
require(dplyr)
require(movecost)
require(sp)
require(raster)
require(gdistance)
require(reshape2)
library(broom)
library(ggplot2)
library(rgdal)
require(ggplot2)
require(viridis)
require(gridExtra)
require(mapproj)

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



# load the richness data -------------------------------------------------------
inwd <- "~/Box Sync/JMPH_2020/data/Batch_Run"
setwd(inwd)
load(file = "rasterpam.rdata")

# add diversity information to each row ----------------------------------------
require(ggplot2); require(viridis); require(gridExtra)
setwd(inwd)
load(file = "rasterpam.rdata")
raterpam_hold <- rasterpam

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
save(myworld, file = "lcp_elevation_results_1rowper_sampledgridcell.rdata")
#
#
# --------------------------------------------------------------------------------------------------------------





data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

sort(tapply(myworld$n_sp_sampled, myworld$BIOME, mean)) # so some variation in # species sampled per biome. 
sort(tapply(myworld$n_sp_sampled, myworld$BIOME, sum)) # so some variation in # total cells sampled per biome as well. 
# with these caveats, lets look at some more interesting things...
sort(tapply(myworld$m_aB_mcosts_ag, myworld$BIOME, mean))# cost by biome
myorder <- names(sort(tapply(myworld$m_aB_mcosts_ag[myworld$m_aB_mcosts_ag < quantile(myworld$m_aB_mcosts_ag, 0.9999)], myworld$BIOME[myworld$m_aB_mcosts_ag < quantile(myworld$m_aB_mcosts_ag, 0.9999)], mean)))

myworldp <- myworld[!is.na(myworld$BIOME),]
myworldp$BIOME <- factor(myworldp$BIOME, levels = myorder) # reorder the treat factor
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=BIOME, y=m_aB_mcosts_ag)) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = "blue")+
  geom_violin()+
  stat_summary(fun.data=data_summary, color = "red")+
  coord_flip()
p



p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = BIOME)) + 
  geom_point(size = 1.25, shape = "square")
p
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = BIOME)) + 
  geom_point(size = 1.25, shape = "square")+
  coord_equal()
p
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = BIOME)) + 
  geom_point(size = 1.25, shape = "square")+
  coord_cartesian()
p
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = BIOME)) + 
  geom_point(size = 1.25, shape = "square")+
  coord_polar()
p




sort(tapply(myworld$n_sp_sampled, myworld$REALM, mean)) # so some variation in # species sampled per biome. 
sort(tapply(myworld$n_sp_sampled, myworld$REALM, sum)) # so some variation in # total cells sampled per biome as well. 
# with these caveats, lets look at some more interesting things...
sort(tapply(myworld$m_aB_mcosts_ag, myworld$REALM, mean))# cost by biome
myorder <- names(sort(tapply(myworld$m_aB_mcosts_ag, myworld$REALM, median)))

myworldp <- myworld[!is.na(myworld$REALM),]
myworldp$REALM <- factor(myworldp$REALM, levels = myorder) # reorder the treat factor
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.999),], aes(x=REALM, y=m_aB_mcosts_ag)) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = "blue")+
  geom_violin()+
  stat_summary(fun.data=data_summary, color = "red")+
  coord_flip()
p



p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = REALM)) + 
  geom_point(size = 1.25, shape = "square")
p

p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = -GBL_STAT)) + 
  geom_point(size = 1.25, shape = "square")
p

mycol <- viridis(n = length(unique(myworld$ECO_NAME)), option = "C")
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = ECO_NAME)) + 
  geom_point(size = 1.25, shape = "square")+ 
  theme(legend.position = "none") +
  scale_color_manual(values = mycol)

p


myworldp$REALM <- relevel(myworldp$REALM, ref = "Oceania") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Australasia") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "IndoMalay") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Nearctic") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Afrotropics") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Palearctic") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Neotropics") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))




# view the diversity data 
rasterpam <- as.data.frame(rasterpam, xy=TRUE)
ggplot(data = rasterpam, aes(x = x , y =y, color = (layer)))+
  geom_point(cex = 0.5)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# some models
plot(mymelt$div[mymelt$m_aB_mcosts_ag < 8000000]~ mymelt$m_aB_mcosts_ag[mymelt$m_aB_mcosts_ag < 8000000]) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
summary(glm(mymelt$div[mymelt$m_aB_mcosts_ag < 8000000] ~mymelt$m_aB_mcosts_ag[mymelt$m_aB_mcosts_ag < 8000000] )) # negative relationship between diversity and cost
summary(glm(mymelt$div[mymelt$m_aB_mcosts_ag < 8000000] ~mymelt$m_aB_mcosts_ag[mymelt$m_aB_mcosts_ag < 8000000] + I(mymelt$m_aB_mcosts_ag[mymelt$m_aB_mcosts_ag < 8000000]^2) )) # true here as well



# some models with the world data
plot(myworld$div~ myworld$m_aB_mcosts_ag, col =rgb(0,0,0,0.1), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
plot(myworld$div[myworld$m_aB_mcosts_ag < 8000000]~ myworld$m_aB_mcosts_ag[myworld$m_aB_mcosts_ag < 8000000], col =rgb(0,0,0,0.25), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
summary(glm(myworld$div ~myworld$m_aB_mcosts_ag)) # negative relationship between diversity and cost
summary(glm(myworld$div ~myworld$m_aB_mcosts_ag + I(myworld$m_aB_mcosts_ag^2) )) # true here as well

plot(myworld$m_aB_mcosts_ag~ myworld$m_a_lats_ag, col =rgb(0,0,0,0.1), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
plot(myworld$m_aB_mcosts_ag[myworld$m_aB_mcosts_ag < 8000000]~ myworld$m_a_lats_ag[myworld$m_aB_mcosts_ag < 8000000], col =rgb(0,0,0,0.4), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
summary(glm(myworld$m_aB_mcosts_ag ~myworld$m_a_lats_ag )) # negative relationship between diversity and cost




# Load elevation data and view -------------------------------------------------
setwd(inwd)
load(file = "000_Setup_elev_raster_outputs.Rdata")
elev_raster_hold <- elev_raster
elev_raster[elev_raster$layer <= 0,] <- NA
elev_raster <- as.data.frame(elev_raster, xy=TRUE)

# view elevations
ggplot(data = elev_raster, aes(x = x , y =y, color = (layer)))+
  geom_point(cex = 0.5)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()


Mp_raster1950 <- projectRaster(Mp_raster1950, elev_raster_hold)
Mp_raster1950_2 <- as.data.frame(Mp_raster1950, xy=TRUE)
ggplot()+
  geom_point(data = Mp_raster1950_2, aes(x = x , y =y, color = (layer)), cex = 0.5)+
  geom_point(data = elev_raster, aes(x = x , y =y), color = rgb(0,0,0,0.05), cex = 0.5)+
  
  coord_equal()+
  theme_bw()+
  scale_color_viridis()


# view points where it is POSSIBLE to have an ai (will be background on our plots below.)
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  coord_equal()+
  theme_bw()



# Exploratory World Plots ------------------------------------------------------
# LEAST COST PATH SUM COST -------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata$m_aB_mcosts_ag <- scale(pdata$m_aB_mcosts_ag, center = F)
plot(pdata$m_aB_mcosts_ag~pdata$m_a_lats_ag) # notice that there are some natural breaks that will influence our interpretations...


# exclude top 0.01% of data points -------------------------
pd1 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.9999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p1 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd1, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 0.10% of data points -------------------------
pd2 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p2 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd2, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 1.00% of data points -------------------------
pd3 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.99),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p3 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd3, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 10.0% of data points -------------------------
pd4 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.9),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p4 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd4, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

lay <- rbind(c(1),
             c(2),
             c(3),
             c(4))
x <- grid.arrange(p1, p2, p3, p4, layout_matrix = lay)
grid::grid.draw(x)
pdf(file = paste("000_test.pdf"),height = 36, width = 18); grid::grid.draw(x); dev.off()




# Exploratory World Plots ------------------------------------------------------
# LEAST COST PATH SUM COST -------------------------------------------
pdata <- myworld[order(mymelt$m_aB_mcosts_ag),]
pdata$m_aB_mcosts_ag <- scale(pdata$m_aB_mcosts_ag, center = F)
plot(pdata$m_aB_mcosts_ag~pdata$m_a_lats_ag) # notice that there are some natural breaks that will influence our interpretations...


# exclude top 0.01% of data points -------------------------
pd1 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.9999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p1 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd1, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 0.10% of data points -------------------------
pd2 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p2 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd2, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 1.00% of data points -------------------------
pd3 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.99),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p3 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd3, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 10.0% of data points -------------------------
pd4 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.9),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p4 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd4, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

lay <- rbind(c(1),
             c(2),
             c(3),
             c(4))
x <- grid.arrange(p1, p2, p3, p4, layout_matrix = lay)
grid::grid.draw(x)
pdf(file = paste("000_test_world.pdf"),height = 36, width = 18); grid::grid.draw(x); dev.off()








# Look at geographic region...-------------------------
# These data are flawed...geographic region was for sp1, or the pair centroid, does not work here. 
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
plot(pdata$m_aB_mcosts_ag~pdata$m_a_lats_ag)
pd1 <- pdata[pdata$Geographic.region == "Africa",]
pd2 <- pdata[pdata$Geographic.region == "Eurasia",]
pd3 <- pdata[pdata$Geographic.region == "South America",]
pd4 <- pdata[pdata$Geographic.region == "North America",]
pd5 <- pdata[pdata$Geographic.region == "Oceania",]
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = Geographic.region), inherit.aes = F)+
  coord_equal()+
  theme_bw()




# Assign lat groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$m_a_lats_ag, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$latgroup <- NA
pdata$latgroup[pdata$m_a_lats_ag < qs[1]] <- 1
pdata$latgroup[pdata$m_a_lats_ag >= qs[1] & pdata$m_a_lats_ag < qs[2]] <- 2
pdata$latgroup[pdata$m_a_lats_ag >= qs[2] & pdata$m_a_lats_ag < qs[3]] <- 3
pdata$latgroup[pdata$m_a_lats_ag >= qs[3] & pdata$m_a_lats_ag < qs[4]] <- 4
pdata$latgroup[pdata$m_a_lats_ag >= qs[4] & pdata$m_a_lats_ag < qs[5]] <- 5
pdata$latgroup[pdata$m_a_lats_ag >= qs[5] & pdata$m_a_lats_ag < qs[6]] <- 6
pdata$latgroup[pdata$m_a_lats_ag >= qs[6] & pdata$m_a_lats_ag < qs[7]] <- 7
pdata$latgroup[pdata$m_a_lats_ag >= qs[7] & pdata$m_a_lats_ag < qs[8]] <- 8
pdata$latgroup[pdata$m_a_lats_ag >= qs[8] & pdata$m_a_lats_ag < qs[9]] <- 9
pdata$latgroup[pdata$m_a_lats_ag >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = latgroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# Assign ele groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$m_a_eles_ag, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$elegroup <- NA
pdata$elegroup[pdata$m_a_eles_ag < qs[1]] <- 1
pdata$elegroup[pdata$m_a_eles_ag >= qs[1] & pdata$m_a_eles_ag < qs[2]] <- 2
pdata$elegroup[pdata$m_a_eles_ag >= qs[2] & pdata$m_a_eles_ag < qs[3]] <- 3
pdata$elegroup[pdata$m_a_eles_ag >= qs[3] & pdata$m_a_eles_ag < qs[4]] <- 4
pdata$elegroup[pdata$m_a_eles_ag >= qs[4] & pdata$m_a_eles_ag < qs[5]] <- 5
pdata$elegroup[pdata$m_a_eles_ag >= qs[5] & pdata$m_a_eles_ag < qs[6]] <- 6
pdata$elegroup[pdata$m_a_eles_ag >= qs[6] & pdata$m_a_eles_ag < qs[7]] <- 7
pdata$elegroup[pdata$m_a_eles_ag >= qs[7] & pdata$m_a_eles_ag < qs[8]] <- 8
pdata$elegroup[pdata$m_a_eles_ag >= qs[8] & pdata$m_a_eles_ag < qs[9]] <- 9
pdata$elegroup[pdata$m_a_eles_ag >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = elegroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# Assign div groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$div, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$divgroup <- NA
pdata$divgroup[pdata$div < qs[1]] <- 1
pdata$divgroup[pdata$div >= qs[1] & pdata$div < qs[2]] <- 2
pdata$divgroup[pdata$div >= qs[2] & pdata$div < qs[3]] <- 3
pdata$divgroup[pdata$div >= qs[3] & pdata$div < qs[4]] <- 4
pdata$divgroup[pdata$div >= qs[4] & pdata$div < qs[5]] <- 5
pdata$divgroup[pdata$div >= qs[5] & pdata$div < qs[6]] <- 6
pdata$divgroup[pdata$div >= qs[6] & pdata$div < qs[7]] <- 7
pdata$divgroup[pdata$div >= qs[7] & pdata$div < qs[8]] <- 8
pdata$divgroup[pdata$div >= qs[8] & pdata$div < qs[9]] <- 9
pdata$divgroup[pdata$div >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = divgroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# Assign cost groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$costgroup <- NA
pdata$costgroup[pdata$m_aB_mcosts_ag < qs[1]] <- 1
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[1] & pdata$m_aB_mcosts_ag < qs[2]] <- 2
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[2] & pdata$m_aB_mcosts_ag < qs[3]] <- 3
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[3] & pdata$m_aB_mcosts_ag < qs[4]] <- 4
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[4] & pdata$m_aB_mcosts_ag < qs[5]] <- 5
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[5] & pdata$m_aB_mcosts_ag < qs[6]] <- 6
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[6] & pdata$m_aB_mcosts_ag < qs[7]] <- 7
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[7] & pdata$m_aB_mcosts_ag < qs[8]] <- 8
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[8] & pdata$m_aB_mcosts_ag < qs[9]] <- 9
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = costgroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()


# Assign length groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_plengths_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$m_aB_plengths_ag, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$lengroup <- NA
pdata$lengroup[pdata$m_aB_plengths_ag < qs[1]] <- 1
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[1] & pdata$m_aB_plengths_ag < qs[2]] <- 2
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[2] & pdata$m_aB_plengths_ag < qs[3]] <- 3
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[3] & pdata$m_aB_plengths_ag < qs[4]] <- 4
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[4] & pdata$m_aB_plengths_ag < qs[5]] <- 5
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[5] & pdata$m_aB_plengths_ag < qs[6]] <- 6
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[6] & pdata$m_aB_plengths_ag < qs[7]] <- 7
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[7] & pdata$m_aB_plengths_ag < qs[8]] <- 8
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[8] & pdata$m_aB_plengths_ag < qs[9]] <- 9
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = lengroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()
























pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata$m_a_eles_ag <- scale(pdata$m_a_eles_ag, center = F)
plot(pdata$m_a_eles_ag~pdata$m_a_lats_ag) # notice that there are some natural breaks that will influence our interpretations...


# exclude top 0.01% of data points -------------------------
pd1 <- pdata[pdata$m_a_eles_ag <= quantile(pdata$m_a_eles_ag, na.rm = T, probs = 0.9999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p1 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd1, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_a_eles_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

pd2 <- pdata[pdata$m_a_eles_ag <= quantile(pdata$m_a_eles_ag, na.rm = T, probs = 0.999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p2 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd2, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_a_eles_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

pd3 <- pdata[pdata$m_a_eles_ag <= quantile(pdata$m_a_eles_ag, na.rm = T, probs = 0.99),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p3 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd3, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_a_eles_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

pd4 <- pdata[pdata$m_a_eles_ag <= quantile(pdata$m_a_eles_ag, na.rm = T, probs = 0.9),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p4 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd4, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_a_eles_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()


