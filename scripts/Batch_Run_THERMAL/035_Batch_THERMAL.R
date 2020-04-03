rm(list=ls()) # clear workspace 

# control block ----------------------------------------------------------------
prefix <- 35
brstart <- prefix
brend <- brstart + 1

# load all required packages ---------------------------------------------------
require(rgdal)
require(raster)
require(dplyr)
require(movecost)
require(sp)
require(raster)
require(gdistance)

# directories -----------------------------------------------------------------
wdscripts <- "/Users/boterolab1/Box Sync/JMPH_2020/scripts/Batch_Run_THERMAL"
wddata <-  "/Users/boterolab1/Box Sync/JMPH_2020/data/Batch_Run_THERMAL"


# load required files & sources  -----------------------------------------------
setwd(wdscripts)
source("THERMAL_SOURCE.R")
setwd(wddata)
load(file = "cooneyd.Rdata"); mydata <- cooneyd; rm(cooneyd)
load(file = "tas_rasters_VJF.rdata")
load(file = "tasmin_rasters_VJF.rdata")
load(file = "tasmax_rasters_VJF.rdata")
load(file = "tasrng_rasters_VJF.rdata")
load("elevRaster_use.rdata"); elev_raster <- projectRaster(from = Elev_raster, to = tasmin[[1]]); rm(Elev_raster)
load(file = paste0("pamMAT", prefix, ".Rdata")); pamMAT <- get(paste0("pamMAT", prefix))
load(file = "mybreaks.rdata")

# breaks and setup -------------------------------------------------------------
sp_start <- mybreaks[brstart] + 1 
sp_end <- mybreaks[brend]
savelist_path_prefix <- prefix
savefile_metadata_prefix <- prefix

# subset mydata ----------------------------------------------------------------
mydata <- mydata[c(seq(sp_start, sp_end)),]

# Prepare dataframe for path run -----------------------------------------------
# pregenerate necessary columns that will be populated by the main loop
n_origins <- 100 # number of cells in the origin range, A, to use as origin points
m_a_ids <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))

m_a_lons <- m_a_ids # store origin data matrices
m_a_lats <- m_a_ids
m_a_eles <- m_a_ids
m_a_tas_M1 <- m_a_ids; m_a_tasmax_M1 <- m_a_ids; m_a_tasmin_M1 <- m_a_ids; m_a_tasrng_M1 <- m_a_ids
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


m_a_bblons <- m_a_ids # store destination data matrices
m_a_bblats <- m_a_ids
m_a_bbeles <- m_a_ids
m_a_bbtas_M1 <- m_a_ids; m_a_bbtasmax_M1 <- m_a_ids; m_a_bbtasmin_M1 <- m_a_ids; m_a_bbtasrng_M1 <- m_a_ids
m_a_bbtas_M2 <- m_a_ids; m_a_bbtasmax_M2 <- m_a_ids; m_a_bbtasmin_M2 <- m_a_ids; m_a_bbtasrng_M2 <- m_a_ids
m_a_bbtas_M3 <- m_a_ids; m_a_bbtasmax_M3 <- m_a_ids; m_a_bbtasmin_M3 <- m_a_ids; m_a_bbtasrng_M3 <- m_a_ids
m_a_bbtas_M4 <- m_a_ids; m_a_bbtasmax_M4 <- m_a_ids; m_a_bbtasmin_M4 <- m_a_ids; m_a_bbtasrng_M4 <- m_a_ids
m_a_bbtas_M5 <- m_a_ids; m_a_bbtasmax_M5 <- m_a_ids; m_a_bbtasmin_M5 <- m_a_ids; m_a_bbtasrng_M5 <- m_a_ids
m_a_bbtas_M6 <- m_a_ids; m_a_bbtasmax_M6 <- m_a_ids; m_a_bbtasmin_M6 <- m_a_ids; m_a_bbtasrng_M6 <- m_a_ids
m_a_bbtas_M7 <- m_a_ids; m_a_bbtasmax_M7 <- m_a_ids; m_a_bbtasmin_M7 <- m_a_ids; m_a_bbtasrng_M7 <- m_a_ids
m_a_bbtas_M8 <- m_a_ids; m_a_bbtasmax_M8 <- m_a_ids; m_a_bbtasmin_M8 <- m_a_ids; m_a_bbtasrng_M8 <- m_a_ids
m_a_bbtas_M9 <- m_a_ids; m_a_bbtasmax_M9 <- m_a_ids; m_a_bbtasmin_M9 <- m_a_ids; m_a_bbtasrng_M9 <- m_a_ids
m_a_bbtas_M10 <- m_a_ids; m_a_bbtasmax_M10 <- m_a_ids; m_a_bbtasmin_M10 <- m_a_ids; m_a_bbtasrng_M10 <- m_a_ids
m_a_bbtas_M11 <- m_a_ids; m_a_bbtasmax_M11 <- m_a_ids; m_a_bbtasmin_M11 <- m_a_ids; m_a_bbtasrng_M11 <- m_a_ids
m_a_bbtas_M12 <- m_a_ids; m_a_bbtasmax_M12 <- m_a_ids; m_a_bbtasmin_M12 <- m_a_ids; m_a_bbtasrng_M12 <- m_a_ids


m_aB_mcosts <- m_a_ids # store lcp data matrices
m_aB_plengths <- m_a_ids
m_aB_walltime <- m_a_ids

# Main Loop --------------------------------------------------------------------
i <- 1; j <- 1; k <- 1 # reset just in case.

# load previously existing outputs IF THEY EXIST
load(file = paste0(savefile_metadata_prefix, "_results_THERMAL_100ptsea.rdata"))
if (exists("completed_rows") == FALSE){
  completed_rows <- 0 # if no data exist, start on the 1st species
}
sp_start <- completed_rows + 1 # if data exist, start on the next species that is not complete. 
sp_end <- nrow(mydata)
for (i in sp_start:sp_end){
  print(paste0("starting species ", i)) # :)  qw
  
  # get coords for A and B -----------------------------------------------------
  mytemp <- pamMAT[,c("Longitude(x)", "Latitude(y)", paste0(mydata[i, "Species.1bl"]), paste0(mydata[i, "Species.2bl"]))]
  coordsA <- subset(mytemp, mytemp[, 3] == 1); coordsA <- data.frame(lon = coordsA[ , 1], lat = coordsA[ , 2]); coordinates(coordsA) <- c("lon", "lat"); crs(coordsA) <- "+proj=wag4 +lon_0=0 +ellps=WGS84"
  coordsB <- subset(mytemp, mytemp[, 4] == 1); coordsB <- data.frame(lon = coordsB[ , 1], lat = coordsB[ , 2]); coordinates(coordsB) <- c("lon", "lat"); crs(coordsB) <- "+proj=wag4 +lon_0=0 +ellps=WGS84"
  
  # generate origins information ---------------------------
  origins <- sample(x = mydata$n_pam_cells_sp1[i], size = min(n_origins, mydata$n_pam_cells_sp1[i]), replace = F) # sample origins indices in range A
  origins.id <- origins
  origins <- data.frame(lon = coordinates(coordsA)[origins, 1], lat = coordinates(coordsA)[origins, 2]) # get origins coords
  
  savelist_full_paths <- c() # initialize a list to save the lcps (as lines) for this species. 
  for (j in 1:nrow(origins)) {
    ijstart <- Sys.time()
    print(paste0("species ", i, " origin ", j))
    
    # Store information for the current origin cell, a, in the respective matrices ------
    m_a_ids[i,j] <- origins.id[j] # id
    m_a_lons[i,j] <- origins[j, "lon"] # lon & lat
    m_a_lats[i,j] <- origins[j, "lat"]
    m_a_eles[i,j] <- extract(x = elev_raster, y = origins[j,]) # elevation
    
    
    # extract and calculate origin point data -------------------------
    ai <- origins[j,]
    Omean <- extract(tas, ai) # tas (mean monthly temp)
    Omax <- extract(tasmax, ai) # tasmax (max monthly temp)
    Omin <- extract(tasmin, ai) # tasmin (min monthly temp)
    Orng <- extract(tasrng, ai) # range, tasmax - tasmin (monthly max - monthly min temp)
    m_a_tas_M1[i,j] <- Omean[1]; m_a_tasmax_M1[i,j] <- Omax[1]; m_a_tasmin_M1[i,j] <- Omin[1]; m_a_tasrng_M1[i,j] <- Orng[1] 
    m_a_tas_M2[i,j] <- Omean[2]; m_a_tasmax_M2[i,j] <- Omax[2]; m_a_tasmin_M2[i,j] <- Omin[2]; m_a_tasrng_M2[i,j] <- Orng[2] 
    m_a_tas_M3[i,j] <- Omean[3]; m_a_tasmax_M3[i,j] <- Omax[3]; m_a_tasmin_M3[i,j] <- Omin[3]; m_a_tasrng_M3[i,j] <- Orng[3] 
    m_a_tas_M4[i,j] <- Omean[4]; m_a_tasmax_M4[i,j] <- Omax[4]; m_a_tasmin_M4[i,j] <- Omin[4]; m_a_tasrng_M4[i,j] <- Orng[4] 
    m_a_tas_M5[i,j] <- Omean[5]; m_a_tasmax_M5[i,j] <- Omax[5]; m_a_tasmin_M5[i,j] <- Omin[5]; m_a_tasrng_M5[i,j] <- Orng[5] 
    m_a_tas_M6[i,j] <- Omean[6]; m_a_tasmax_M6[i,j] <- Omax[6]; m_a_tasmin_M6[i,j] <- Omin[6]; m_a_tasrng_M6[i,j] <- Orng[6] 
    m_a_tas_M7[i,j] <- Omean[7]; m_a_tasmax_M7[i,j] <- Omax[7]; m_a_tasmin_M7[i,j] <- Omin[7]; m_a_tasrng_M7[i,j] <- Orng[7] 
    m_a_tas_M8[i,j] <- Omean[8]; m_a_tasmax_M8[i,j] <- Omax[8]; m_a_tasmin_M8[i,j] <- Omin[8]; m_a_tasrng_M8[i,j] <- Orng[8] 
    m_a_tas_M9[i,j] <- Omean[9]; m_a_tasmax_M9[i,j] <- Omax[9]; m_a_tasmin_M9[i,j] <- Omin[9]; m_a_tasrng_M9[i,j] <- Orng[9] 
    m_a_tas_M10[i,j] <- Omean[10]; m_a_tasmax_M10[i,j] <- Omax[10]; m_a_tasmin_M10[i,j] <- Omin[10]; m_a_tasrng_M10[i,j] <- Orng[10] 
    m_a_tas_M11[i,j] <- Omean[11]; m_a_tasmax_M11[i,j] <- Omax[11]; m_a_tasmin_M11[i,j] <- Omin[11]; m_a_tasrng_M11[i,j] <- Orng[11] 
    m_a_tas_M12[i,j] <- Omean[12]; m_a_tasmax_M12[i,j] <- Omax[12]; m_a_tasmin_M12[i,j] <- Omin[12]; m_a_tasrng_M12[i,j] <- Orng[12] 
    

    
    # create the cost map -----------------------------------------------------
    minofmaxs <- tasmax # value is Dmax....# ...unless Omax is SMALLER...then replace with Omax
    maxofmins <- tasmin # value is Dmin...# ...unless Omin is GREATER...then replace with Omin
    minofmaxs[[1]][minofmaxs[[1]] > Omax[1]] <- Omax[1]; maxofmins[[1]][maxofmins[[1]] < Omin[1]] <- Omin[1] # jan
    minofmaxs[[2]][minofmaxs[[2]] > Omax[2]] <- Omax[2]; maxofmins[[2]][maxofmins[[2]] < Omin[2]] <- Omin[2] # feb
    minofmaxs[[3]][minofmaxs[[3]] > Omax[3]] <- Omax[3]; maxofmins[[3]][maxofmins[[3]] < Omin[3]] <- Omin[3] # march....
    minofmaxs[[4]][minofmaxs[[4]] > Omax[4]] <- Omax[4]; maxofmins[[4]][maxofmins[[4]] < Omin[4]] <- Omin[4]
    minofmaxs[[5]][minofmaxs[[5]] > Omax[5]] <- Omax[5]; maxofmins[[5]][maxofmins[[5]] < Omin[5]] <- Omin[5]
    minofmaxs[[6]][minofmaxs[[6]] > Omax[6]] <- Omax[6]; maxofmins[[6]][maxofmins[[6]] < Omin[6]] <- Omin[6]
    minofmaxs[[7]][minofmaxs[[7]] > Omax[7]] <- Omax[7]; maxofmins[[7]][maxofmins[[7]] < Omin[7]] <- Omin[7]
    minofmaxs[[8]][minofmaxs[[8]] > Omax[8]] <- Omax[8]; maxofmins[[8]][maxofmins[[8]] < Omin[8]] <- Omin[8]
    minofmaxs[[9]][minofmaxs[[9]] > Omax[9]] <- Omax[9]; maxofmins[[9]][maxofmins[[9]] < Omin[9]] <- Omin[9]
    minofmaxs[[10]][minofmaxs[[10]] > Omax[10]] <- Omax[10]; maxofmins[[10]][maxofmins[[10]] < Omin[10]] <- Omin[10]
    minofmaxs[[11]][minofmaxs[[11]] > Omax[11]] <- Omax[11]; maxofmins[[11]][maxofmins[[11]] < Omin[11]] <- Omin[11]
    minofmaxs[[12]][minofmaxs[[12]] > Omax[12]] <- Omax[12]; maxofmins[[12]][maxofmins[[12]] < Omin[12]] <- Omin[12] 
    overlap <- minofmaxs - maxofmins # calculate overlap
    
    # Calculate V (thermal overlap) using eq from Janzen 1967 -----
    Vmatgeo <- overlap / (sqrt((as.vector(Orng) * tasrng))) # for each month...
    Vmatgeo_stack <- Vmatgeo; Vmatgeo <- Vmatgeo[[1]]; for (m in 2:12) { Vmatgeo <- Vmatgeo + Vmatgeo_stack[[m]] }  # now sum up over the whole year....
    cost_map <- abs(Vmatgeo - abs(max(values(Vmatgeo), na.rm = T))) # fix the overlap values to work with our cost function

    # generate destinations information ---------------------------
    destinations <- seq(from = 1, to = mydata$n_pam_cells_sp2[i]) # doing all destinations, so do them sequentially to make data collection easy. 
    destinations <- data.frame(lon = coordinates(coordsB)[destinations, 1], lat = coordinates(coordsB)[destinations, 2]) # select coords to use as destination points
    
    # generate all paths a_j -> B ----------------------------------
    mypaths <- CalculateLCP(dtm = cost_map,
                            origin = coordinates(origins[j,]),
                            destin = coordinates(destinations))
    plot(cost_map); points(ai); lines(mypaths$sPath)

    
    mypath_mc_mcosts <- c()
    mypath_mc_plens <- c()
    for (k in 1:length(mypaths$sPath)) {
      print(paste0("species ", i, " origin ", j, " dest ", k))
      mypath_mc <- mypaths$sPath[k]
      mypath_mc_mcost <- mypaths$mcost[k]
      mypath_mc_plen <- mypaths$plen[k]
      
      if(mypath_mc_mcost == "sPath_extentpoint"){
        mypath_mc_mcosts <- c(mypath_mc_mcosts, 0)
        mypath_mc_plens <- c(mypath_mc_plens, 0)
      }
      
      if(mypath_mc_mcost != "sPath_extentpoint"){
        mypath_mc_mcosts <- c(mypath_mc_mcosts, mypath_mc_mcost)
        mypath_mc_plens <- c(mypath_mc_plens, mypath_mc_plen)
      }
      
    }
    # find the cheapest, shortest path ai -> B -------------
    min_cost_for_cheapest_ids <- which(mypath_mc_mcosts == min(mypath_mc_mcosts))
    min_length_for_cheapest_ids <- which(mypath_mc_plens == min(mypath_mc_plens[min_cost_for_cheapest_ids]))
    mypath_id <- intersect(min_cost_for_cheapest_ids, min_length_for_cheapest_ids)
    mypath_id <- mypath_id[1]
    
    
    # store untrimmed path info for ai->b ------------------
    savelist_full_paths <- c(savelist_full_paths, mypaths$sPath[mypath_id])
    m_aB_mcosts[i,j] <- mypaths$mcost[mypath_id]
    m_aB_plengths[i,j] <- mypaths$plen[mypath_id]
    
    # store the destination information for this a_i best destination ---
    destination <- destinations[mypath_id,]
    m_a_bblons[i,j] <- destination$lon
    m_a_bblats[i,j] <- destination$lat
    m_a_bbeles[i,j] <- extract(x = elev_raster, y = destination) 
    
    a_bb <- destination
    Omean <- extract(tas, a_bb) # tas (mean monthly temp)
    Omax <- extract(tasmax, a_bb) # tasmax (max monthly temp)
    Omin <- extract(tasmin, a_bb) # tasmin (min monthly temp)
    Orng <- extract(tasrng, a_bb) # range, tasmax - tasmin (monthly max - monthly min temp)
    m_a_bbtas_M1[i,j] <- Omean[1]; m_a_bbtasmax_M1[i,j] <- Omax[1]; m_a_bbtasmin_M1[i,j] <- Omin[1]; m_a_bbtasrng_M1[i,j] <- Orng[1] 
    m_a_bbtas_M2[i,j] <- Omean[2]; m_a_bbtasmax_M2[i,j] <- Omax[2]; m_a_bbtasmin_M2[i,j] <- Omin[2]; m_a_bbtasrng_M2[i,j] <- Orng[2] 
    m_a_bbtas_M3[i,j] <- Omean[3]; m_a_bbtasmax_M3[i,j] <- Omax[3]; m_a_bbtasmin_M3[i,j] <- Omin[3]; m_a_bbtasrng_M3[i,j] <- Orng[3] 
    m_a_bbtas_M4[i,j] <- Omean[4]; m_a_bbtasmax_M4[i,j] <- Omax[4]; m_a_bbtasmin_M4[i,j] <- Omin[4]; m_a_bbtasrng_M4[i,j] <- Orng[4] 
    m_a_bbtas_M5[i,j] <- Omean[5]; m_a_bbtasmax_M5[i,j] <- Omax[5]; m_a_bbtasmin_M5[i,j] <- Omin[5]; m_a_bbtasrng_M5[i,j] <- Orng[5] 
    m_a_bbtas_M6[i,j] <- Omean[6]; m_a_bbtasmax_M6[i,j] <- Omax[6]; m_a_bbtasmin_M6[i,j] <- Omin[6]; m_a_bbtasrng_M6[i,j] <- Orng[6] 
    m_a_bbtas_M7[i,j] <- Omean[7]; m_a_bbtasmax_M7[i,j] <- Omax[7]; m_a_bbtasmin_M7[i,j] <- Omin[7]; m_a_bbtasrng_M7[i,j] <- Orng[7] 
    m_a_bbtas_M8[i,j] <- Omean[8]; m_a_bbtasmax_M8[i,j] <- Omax[8]; m_a_bbtasmin_M8[i,j] <- Omin[8]; m_a_bbtasrng_M8[i,j] <- Orng[8] 
    m_a_bbtas_M9[i,j] <- Omean[9]; m_a_bbtasmax_M9[i,j] <- Omax[9]; m_a_bbtasmin_M9[i,j] <- Omin[9]; m_a_bbtasrng_M9[i,j] <- Orng[9] 
    m_a_bbtas_M10[i,j] <- Omean[10]; m_a_bbtasmax_M10[i,j] <- Omax[10]; m_a_bbtasmin_M10[i,j] <- Omin[10]; m_a_bbtasrng_M10[i,j] <- Orng[10] 
    m_a_bbtas_M11[i,j] <- Omean[11]; m_a_bbtasmax_M11[i,j] <- Omax[11]; m_a_bbtasmin_M11[i,j] <- Omin[11]; m_a_bbtasrng_M11[i,j] <- Orng[11] 
    m_a_bbtas_M12[i,j] <- Omean[12]; m_a_bbtasmax_M12[i,j] <- Omax[12]; m_a_bbtasmin_M12[i,j] <- Omin[12]; m_a_bbtasrng_M12[i,j] <- Orng[12] 
  }
  
  setwd(wddata)
  save(savelist_full_paths, file = paste0(savelist_path_prefix, "_FULLPATH_results_for_", mydata[i,"Species.1bl"], "_", mydata[i,"Species.2bl"], ".rdata"))
  savelist_full_paths <- NULL
  ijend <- Sys.time()
  m_aB_walltime[i,j] <- ijend - ijstart
  
  if(i %% 5 == 0){
    completed_rows <- i - 1
    setwd(wddata)
    save(sp_start, sp_end, completed_rows, mydata, 
         m_a_ids, m_a_lons,m_a_lats,m_a_eles,
         m_a_tas_M1, m_a_tasmax_M1, m_a_tasmin_M1, m_a_tasrng_M1,
         m_a_tas_M2, m_a_tasmax_M2, m_a_tasmin_M2, m_a_tasrng_M2,
         m_a_tas_M3, m_a_tasmax_M3, m_a_tasmin_M3, m_a_tasrng_M3,
         m_a_tas_M4, m_a_tasmax_M4, m_a_tasmin_M4, m_a_tasrng_M4,
         m_a_tas_M5, m_a_tasmax_M5, m_a_tasmin_M5, m_a_tasrng_M5,
         m_a_tas_M6, m_a_tasmax_M6, m_a_tasmin_M6, m_a_tasrng_M6,
         m_a_tas_M7, m_a_tasmax_M7, m_a_tasmin_M7, m_a_tasrng_M7,
         m_a_tas_M8, m_a_tasmax_M8, m_a_tasmin_M8, m_a_tasrng_M8,
         m_a_tas_M9, m_a_tasmax_M9, m_a_tasmin_M9, m_a_tasrng_M9,
         m_a_tas_M10, m_a_tasmax_M10, m_a_tasmin_M10, m_a_tasrng_M10,
         m_a_tas_M11, m_a_tasmax_M11, m_a_tasmin_M11, m_a_tasrng_M11,
         m_a_tas_M12, m_a_tasmax_M12, m_a_tasmin_M12, m_a_tasrng_M12,
         m_a_bblons,m_a_bblats,m_a_bbeles,
         m_a_bbtas_M1, m_a_bbtasmax_M1, m_a_bbtasmin_M1, m_a_bbtasrng_M1,
         m_a_bbtas_M2, m_a_bbtasmax_M2, m_a_bbtasmin_M2, m_a_bbtasrng_M2,
         m_a_bbtas_M3, m_a_bbtasmax_M3, m_a_bbtasmin_M3, m_a_bbtasrng_M3,
         m_a_bbtas_M4, m_a_bbtasmax_M4, m_a_bbtasmin_M4, m_a_bbtasrng_M4,
         m_a_bbtas_M5, m_a_bbtasmax_M5, m_a_bbtasmin_M5, m_a_bbtasrng_M5,
         m_a_bbtas_M6, m_a_bbtasmax_M6, m_a_bbtasmin_M6, m_a_bbtasrng_M6,
         m_a_bbtas_M7, m_a_bbtasmax_M7, m_a_bbtasmin_M7, m_a_bbtasrng_M7,
         m_a_bbtas_M8, m_a_bbtasmax_M8, m_a_bbtasmin_M8, m_a_bbtasrng_M8,
         m_a_bbtas_M9, m_a_bbtasmax_M9, m_a_bbtasmin_M9, m_a_bbtasrng_M9,
         m_a_bbtas_M10, m_a_bbtasmax_M10, m_a_bbtasmin_M10, m_a_bbtasrng_M10,
         m_a_bbtas_M11, m_a_bbtasmax_M11, m_a_bbtasmin_M11, m_a_bbtasrng_M11,
         m_a_bbtas_M12, m_a_bbtasmax_M12, m_a_bbtasmin_M12, m_a_bbtasrng_M12,
         m_aB_mcosts, m_aB_plengths, m_aB_walltime,
         file = paste0(savefile_metadata_prefix, "_results_THERMAL_100ptsea.rdata"))
  }
  
}
completed_rows <- i - 1
setwd(wddata)
save(sp_start, sp_end, completed_rows, mydata, 
m_a_ids, m_a_lons,m_a_lats,m_a_eles,
m_a_tas_M1, m_a_tasmax_M1, m_a_tasmin_M1, m_a_tasrng_M1,
m_a_tas_M2, m_a_tasmax_M2, m_a_tasmin_M2, m_a_tasrng_M2,
m_a_tas_M3, m_a_tasmax_M3, m_a_tasmin_M3, m_a_tasrng_M3,
m_a_tas_M4, m_a_tasmax_M4, m_a_tasmin_M4, m_a_tasrng_M4,
m_a_tas_M5, m_a_tasmax_M5, m_a_tasmin_M5, m_a_tasrng_M5,
m_a_tas_M6, m_a_tasmax_M6, m_a_tasmin_M6, m_a_tasrng_M6,
m_a_tas_M7, m_a_tasmax_M7, m_a_tasmin_M7, m_a_tasrng_M7,
m_a_tas_M8, m_a_tasmax_M8, m_a_tasmin_M8, m_a_tasrng_M8,
m_a_tas_M9, m_a_tasmax_M9, m_a_tasmin_M9, m_a_tasrng_M9,
m_a_tas_M10, m_a_tasmax_M10, m_a_tasmin_M10, m_a_tasrng_M10,
m_a_tas_M11, m_a_tasmax_M11, m_a_tasmin_M11, m_a_tasrng_M11,
m_a_tas_M12, m_a_tasmax_M12, m_a_tasmin_M12, m_a_tasrng_M12,
m_a_bblons,m_a_bblats,m_a_bbeles,
m_a_bbtas_M1, m_a_bbtasmax_M1, m_a_bbtasmin_M1, m_a_bbtasrng_M1,
m_a_bbtas_M2, m_a_bbtasmax_M2, m_a_bbtasmin_M2, m_a_bbtasrng_M2,
m_a_bbtas_M3, m_a_bbtasmax_M3, m_a_bbtasmin_M3, m_a_bbtasrng_M3,
m_a_bbtas_M4, m_a_bbtasmax_M4, m_a_bbtasmin_M4, m_a_bbtasrng_M4,
m_a_bbtas_M5, m_a_bbtasmax_M5, m_a_bbtasmin_M5, m_a_bbtasrng_M5,
m_a_bbtas_M6, m_a_bbtasmax_M6, m_a_bbtasmin_M6, m_a_bbtasrng_M6,
m_a_bbtas_M7, m_a_bbtasmax_M7, m_a_bbtasmin_M7, m_a_bbtasrng_M7,
m_a_bbtas_M8, m_a_bbtasmax_M8, m_a_bbtasmin_M8, m_a_bbtasrng_M8,
m_a_bbtas_M9, m_a_bbtasmax_M9, m_a_bbtasmin_M9, m_a_bbtasrng_M9,
m_a_bbtas_M10, m_a_bbtasmax_M10, m_a_bbtasmin_M10, m_a_bbtasrng_M10,
m_a_bbtas_M11, m_a_bbtasmax_M11, m_a_bbtasmin_M11, m_a_bbtasrng_M11,
m_a_bbtas_M12, m_a_bbtasmax_M12, m_a_bbtasmin_M12, m_a_bbtasrng_M12,
m_aB_mcosts, m_aB_plengths, m_aB_walltime,
file = paste0(savefile_metadata_prefix, "_results_THERMAL_100ptsea.rdata"))
