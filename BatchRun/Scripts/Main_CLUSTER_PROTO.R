rm(list=ls()); gc() # clear workspace 

# control block ----------------------------------------------------------------
batchNumber <- 1
originStart <- 1
originEnd <- 5000
sp_start <- 1
quantifyElevationalBarriers <- T
quantifyThermalBarriers <- T
quantifyPrecipitationBarriers <- T

# load all required packages ---------------------------------------------------
require(rgdal)
require(raster)
require(dplyr)
require(movecost)
require(sp)
require(gdistance)

# directory paths --------------------------------------------------------------
wdtopo <- "~/JMPH/PREP/Topo/Data"
wdclimate <- "~/JMPH/PREP/Climate/Data"
wdPAM <- "~/JMPH/PREP/PAM/Data"
wdMainDataFrame <- "~/JMPH/PREP/MainDataFrame/Data"
wdSource <- "~/JMPH/BatchRun/Scripts"
wdELEVATIONPathOutputs <- "~/JMPH/BatchRun/Data/ELEVATION_Paths"
wdTHERMALPathOutputs <- "~/JMPH/BatchRun//Data/THERMAL_Paths"
wdPRECIPITATIONPathOutputs  <- "~/JMPH/BatchRun/Data/PRECIPITATION_Paths"
wdBatchResults <- "~/JMPH/BatchRun/Data/Main_Batch_Results"
# source -----------------------------------------------------------------------
setwd(wdSource); source("CalculateLCP_SOURCE.R")

# breaks control file ----------------------------------------------------------
setwd(wdMainDataFrame); load(file = "mybreaks.rdata")
breakStart_index <- batchNumber
breakEnd_index <- breakStart_index + 1
breakStart_index <- mybreaks[breakStart_index] + 1
breakEnd_index <- mybreaks[breakEnd_index]

# cbPAM file for this chunk ----------------------------------------------------
setwd(wdPAM); load(file = paste0("cbPAM", batchNumber, ".Rdata")); cbPAM <- get(paste0("cbPAM", batchNumber)); rm(list = paste0("cbPAM", batchNumber)); gc()


# load rasters -----------------------------------------------------------------
# Elevation
setwd(wdtopo); load(file = "Elev_raster.rdata")
# Temperature
setwd(wdclimate); load(file = "tasmax_rasters_VJF.rdata"); load(file = "tasmin_rasters_VJF.rdata"); load(file = "tasrng_rasters_VJF.rdata")
# Precipitation
setwd(wdclimate); load(file = "pcpmax_rasters_VJF.rdata"); load(file = "pcpmin_rasters_VJF.rdata"); load(file = "pcprng_rasters_VJF.rdata")

# cooneyp frame ----------------------------------------------------------------
setwd(wdMainDataFrame); load(file = paste0("cooneyp", batchNumber, ".Rdata")); cooneyp <- get(paste0("cooneyp", batchNumber)); rm(list = paste0("cooneyp", batchNumber)); gc()

# matrices ---------------------------------------------------------------------
setwd(wdMainDataFrame); load(file = paste0("a_mats_block", batchNumber, ".Rdata"));



# Main Loop --------------------------------------------------------------------
for (i in sp_start:nrow(cooneyp)){
  print(paste0("starting species ", i))
  
  # destination cells (range B) --------
  destinations <- cbPAM[,c("Longitude(x)", "Latitude(y)", paste0(cooneyp[i, "Species.2bl"]))]
  destinations <- subset(destinations, destinations[, 3] == 1);
  destinations <- data.frame(lon = destinations[ , 1], lat = destinations[ , 2]);
  coordinates(destinations) <- c("lon", "lat");
  crs(destinations) <- crs(Elev_raster)
  
  # full path savelists ----------------
  savelist_full_paths_ele <- c()
  savelist_full_paths_tmp <- c()
  savelist_full_paths_pcp <- c()
  
  for (j in originStart:originEnd){
    # [0a] ... if there exists an ORIGIN j for species i ... (i.e., if we haven't run out of origins) ... attempt to find LCP for origin ai_j ....
    if (!is.na(m_a_ids_block[i,j])){
      print(paste0("starting species ", i, " origin ", j))
      
      # Elev Barrier -------------------------------------------------------------------------------------------------------------------------
      if (quantifyElevationalBarriers == TRUE){
        
        # create cost map ----------
        cost_map_original <- Elev_raster; cost_map_original <- abs(cost_map_original) # original costmap is difference from elevation 0
        cost_map <- cost_map_original
        values(cost_map) <- abs(values(cost_map) - m_a_eles_block[i,j])
        # extract(x = cost_map, y = cbind(m_a_lons_block[i,j], m_a_lats_block[i,j])) # check , should always be zero. 
        
        # generate all paths a_j -> B ----------
        mypaths <- CalculateLCP(dtm = cost_map,
                                origin = coordinates(data.frame(lon = m_a_lons_block[i,j],lat = m_a_lats_block[i,j])),
                                destin = destinations)
        
        
        # Populate plen matrix, populate mcost matrix, store paths ----------
        # [1a] ... if the ORIGIN has elevation data ... begin attempt to record results ...
        if(!is.na(mypaths$plen[1])){
          mypath_mc_mcosts <- c()
          mypath_mc_plens <- c()
          
          # for every destination for origin cell aj.....
          for (k in 1:length(mypaths$sPath)) {
            print(paste0("starting species ", i, " origin ", j, " destination ", k, " ELEVATION"))
            mypath_mc_mcost <- mypaths$mcost[k]
            mypath_mc_plen <- mypaths$plen[k]
            
            # [2a] ... if the DESTINATION is distinct from the ORIGIN ... record results for that destination 
            if(mypath_mc_mcost != "sPath_extentpoint"){
              mypath_mc_mcosts <- c(mypath_mc_mcosts, mypath_mc_mcost)
              mypath_mc_plens <- c(mypath_mc_plens, mypath_mc_plen)
            }
            
            # [2b] ... if the DESTINATION is not distinct from the ORIGIN ... record results as sympatric (no cost) for that destination 
            #       ***NOTE: these points will STILL show up as "sPath_extentpoint" in the mcosts and plen matrices in order to IDENTIFY these cases consistently. ***
            if(mypath_mc_mcost == "sPath_extentpoint"){
              mypath_mc_mcosts <- c(mypath_mc_mcosts, 0)
              mypath_mc_plens <- c(mypath_mc_plens, 0)
            }
          }
          
          # from this set of paths, select the shortest of the cheapest paths ... this is the LCP for origin ai
          min_cost_for_cheapest_ids <- which(mypath_mc_mcosts == min(mypath_mc_mcosts, na.rm = T))
          min_length_for_cheapest_ids <- which(mypath_mc_plens == min(mypath_mc_plens[min_cost_for_cheapest_ids], na.rm = T))
          mypath_id <- NA; mypath_id <- intersect(min_cost_for_cheapest_ids, min_length_for_cheapest_ids)[1]
          
          # [3a] ... if there are any good paths for aj --> B ... store the mcost, plength, and dest info; add the spatiallines object for the path aj->B to the growing list for species i
          if (!is.na(mypath_id)){
            savelist_full_paths_ele <- c(savelist_full_paths_ele, mypaths$sPath[mypath_id])
            m_aB_ele_mcosts_block[i,j] <- mypaths$mcost[mypath_id]
            m_aB_ele_plengths_block[i,j] <- mypaths$plen[mypath_id]
            destination <- destinations[mypath_id,]
            m_a_bblons_ele_block[i,j] <- destination$lon
            m_a_bblats_ele_block[i,j] <- destination$lat
            
          # [3b] ... if there are not any good paths for aj --> B ...leave the mcost, plength, and dest info as NA; add an NA to the pathlist for species i
          } else {
            savelist_full_paths_ele <- c(savelist_full_paths_ele, NA)
          }
          
        # [1b] ... if the ORIGIN does not have thermal data ... leave the mcost, plength, and dest info as NA; add an NA to the pathlist for species i    
        } else {
          savelist_full_paths_ele <- c(savelist_full_paths_ele, NA)
        }
        
        # clean for speed and to ensure that old obj aren't used in next loop iteration if obj fail to update.
        rm(cost_map_original, cost_map, mypaths, mypath_mc_mcosts, mypath_mc_plens,
           mypath_mc_mcost, mypath_mc_plen, min_cost_for_cheapest_ids, min_length_for_cheapest_ids, mypath_id, destination)
      } # ------------------------------------------------------------------------------------------------------------------------------------
      
      # Thermal Barrier ----------------------------------------------------------------------------------------------------------------------
      if (quantifyThermalBarriers == TRUE){
        
        # get origin information ----------
        Omax <- matrix(c(m_a_tasmax_M1_block[i,j], m_a_tasmax_M2_block[i,j], m_a_tasmax_M3_block[i,j], m_a_tasmax_M4_block[i,j], m_a_tasmax_M5_block[i,j], m_a_tasmax_M6_block[i,j],
                         m_a_tasmax_M7_block[i,j], m_a_tasmax_M8_block[i,j], m_a_tasmax_M9_block[i,j], m_a_tasmax_M10_block[i,j], m_a_tasmax_M11_block[i,j], m_a_tasmax_M12_block[i,j]), ncol = 12)
        Omin <- matrix(c(m_a_tasmin_M1_block[i,j], m_a_tasmin_M2_block[i,j], m_a_tasmin_M3_block[i,j], m_a_tasmin_M4_block[i,j], m_a_tasmin_M5_block[i,j], m_a_tasmin_M6_block[i,j],
                         m_a_tasmin_M7_block[i,j], m_a_tasmin_M8_block[i,j], m_a_tasmin_M9_block[i,j], m_a_tasmin_M10_block[i,j], m_a_tasmin_M11_block[i,j], m_a_tasmin_M12_block[i,j]), ncol = 12)
        Orng <- matrix(c(m_a_tasrng_M1_block[i,j], m_a_tasrng_M2_block[i,j], m_a_tasrng_M3_block[i,j], m_a_tasrng_M4_block[i,j], m_a_tasrng_M5_block[i,j], m_a_tasrng_M6_block[i,j],
                         m_a_tasrng_M7_block[i,j], m_a_tasrng_M8_block[i,j], m_a_tasrng_M9_block[i,j], m_a_tasrng_M10_block[i,j], m_a_tasrng_M11_block[i,j], m_a_tasrng_M12_block[i,j]), ncol = 12)
        
        # create cost map ----------
        minofmaxs <- tasmax
        maxofmins <- tasmin
        minofmaxs[[1]][minofmaxs[[1]] > Omax[1]] <- Omax[1]; maxofmins[[1]][maxofmins[[1]] < Omin[1]] <- Omin[1]
        minofmaxs[[2]][minofmaxs[[2]] > Omax[2]] <- Omax[2]; maxofmins[[2]][maxofmins[[2]] < Omin[2]] <- Omin[2]
        minofmaxs[[3]][minofmaxs[[3]] > Omax[3]] <- Omax[3]; maxofmins[[3]][maxofmins[[3]] < Omin[3]] <- Omin[3]
        minofmaxs[[4]][minofmaxs[[4]] > Omax[4]] <- Omax[4]; maxofmins[[4]][maxofmins[[4]] < Omin[4]] <- Omin[4]
        minofmaxs[[5]][minofmaxs[[5]] > Omax[5]] <- Omax[5]; maxofmins[[5]][maxofmins[[5]] < Omin[5]] <- Omin[5]
        minofmaxs[[6]][minofmaxs[[6]] > Omax[6]] <- Omax[6]; maxofmins[[6]][maxofmins[[6]] < Omin[6]] <- Omin[6]
        minofmaxs[[7]][minofmaxs[[7]] > Omax[7]] <- Omax[7]; maxofmins[[7]][maxofmins[[7]] < Omin[7]] <- Omin[7]
        minofmaxs[[8]][minofmaxs[[8]] > Omax[8]] <- Omax[8]; maxofmins[[8]][maxofmins[[8]] < Omin[8]] <- Omin[8]
        minofmaxs[[9]][minofmaxs[[9]] > Omax[9]] <- Omax[9]; maxofmins[[9]][maxofmins[[9]] < Omin[9]] <- Omin[9]
        minofmaxs[[10]][minofmaxs[[10]] > Omax[10]] <- Omax[10]; maxofmins[[10]][maxofmins[[10]] < Omin[10]] <- Omin[10]
        minofmaxs[[11]][minofmaxs[[11]] > Omax[11]] <- Omax[11]; maxofmins[[11]][maxofmins[[11]] < Omin[11]] <- Omin[11]
        minofmaxs[[12]][minofmaxs[[12]] > Omax[12]] <- Omax[12]; maxofmins[[12]][maxofmins[[12]] < Omin[12]] <- Omin[12] 
        degreesoverlap <- minofmaxs - maxofmins
        similaritymaps <- degreesoverlap / (sqrt((as.vector(Orng) * tasrng))) # Janzen 1967
        similaritymap_stack <- similaritymaps; similaritymaps <- similaritymaps[[1]]; for(m in 2:12){similaritymaps <- similaritymaps + similaritymap_stack[[m]]} # Janzen 1967 cont.
        cost_map <- abs(similaritymaps - abs(max(values(similaritymaps), na.rm = T)))
        
        # generate all paths a_j -> B ----------
        mypaths <- CalculateLCP(dtm = cost_map,
                                origin = coordinates(data.frame(lon = m_a_lons_block[i,j],lat = m_a_lats_block[i,j])),
                                destin = destinations)
        
        # Populate plen matrix, populate mcost matrix, store paths ----------
        # [1a] ... if the ORIGIN has thermal data ... begin attempt to record results ...
        if(!is.na(mypaths$plen[1])){
          mypath_mc_mcosts <- c()
          mypath_mc_plens <- c()
          
          # for every destination for origin cell aj.....
          for (k in 1:length(mypaths$sPath)) {
            print(paste0("starting species ", i, " origin ", j, " destination ", k, " THERMAL"))
            mypath_mc_mcost <- mypaths$mcost[k]
            mypath_mc_plen <- mypaths$plen[k]
            
            # [2a] ... if the DESTINATION is distinct from the ORIGIN ... record results for that destination 
            if(mypath_mc_mcost != "sPath_extentpoint"){
              mypath_mc_mcosts <- c(mypath_mc_mcosts, mypath_mc_mcost)
              mypath_mc_plens <- c(mypath_mc_plens, mypath_mc_plen)
            }
            
            # [2b] ... if the DESTINATION is not distinct from the ORIGIN ... record results as sympatric (no cost) for that destination 
            #       ***NOTE: these points will STILL show up as "sPath_extentpoint" in the mcosts and plen matrices in order to IDENTIFY these cases consistently. ***
            if(mypath_mc_mcost == "sPath_extentpoint"){
              mypath_mc_mcosts <- c(mypath_mc_mcosts, 0)
              mypath_mc_plens <- c(mypath_mc_plens, 0)
            }
          }
          
          # from this set of paths, select the shortest of the cheapest paths ... this is the LCP for origin ai
          min_cost_for_cheapest_ids <- which(mypath_mc_mcosts == min(mypath_mc_mcosts, na.rm = T))
          min_length_for_cheapest_ids <- which(mypath_mc_plens == min(mypath_mc_plens[min_cost_for_cheapest_ids], na.rm = T))
          mypath_id <- NA; mypath_id <- intersect(min_cost_for_cheapest_ids, min_length_for_cheapest_ids)[1]
          
          # [3a] ... if there are any good paths for aj --> B ... store the mcost, plength, and dest info; add the spatiallines object for the path aj->B to the growing list for species i
          if (!is.na(mypath_id)){
            savelist_full_paths_tmp <- c(savelist_full_paths_tmp, mypaths$sPath[mypath_id])
            m_aB_tmp_mcosts_block[i,j] <- mypaths$mcost[mypath_id]
            m_aB_tmp_plengths_block[i,j] <- mypaths$plen[mypath_id]
            destination <- destinations[mypath_id,]
            m_a_bblons_tmp_block[i,j] <- destination$lon
            m_a_bblats_tmp_block[i,j] <- destination$lat
          
          # [3b] ... if there are not any good paths for aj --> B ...leave the mcost, plength, and dest info as NA; add an NA to the pathlist for species i
          } else {
            savelist_full_paths_tmp <- c(savelist_full_paths_tmp, NA)
          }
        
        # [1b] ... if the ORIGIN does not have thermal data ... leave the mcost, plength, and dest info as NA; add an NA to the pathlist for species i    
        } else {
          savelist_full_paths_tmp <- c(savelist_full_paths_tmp, NA)
        }
        
        # clean for speed and to ensure that old obj aren't used in next loop iteration if obj fail to update.
        rm(Omax, Omin, Orng, minofmaxs, maxofmins, similaritymaps, similaritymap_stack, cost_map, mypaths, mypath_mc_mcosts, mypath_mc_plens,
           mypath_mc_mcost, mypath_mc_plen, min_cost_for_cheapest_ids, min_length_for_cheapest_ids, mypath_id, destination)
      } # ------------------------------------------------------------------------------------------------------------------------------------

      # Precip Barrier -----------------------------------------------------------------------------------------------------------------------
      if (quantifyPrecipitationBarriers == TRUE){
        
        # get origin information ----------
        Omax <- matrix(c(m_a_pcpmax_P1_block[i,j], m_a_pcpmax_P2_block[i,j], m_a_pcpmax_P3_block[i,j], m_a_pcpmax_P4_block[i,j], m_a_pcpmax_P5_block[i,j], m_a_pcpmax_P6_block[i,j],
                         m_a_pcpmax_P7_block[i,j], m_a_pcpmax_P8_block[i,j], m_a_pcpmax_P9_block[i,j], m_a_pcpmax_P10_block[i,j], m_a_pcpmax_P11_block[i,j], m_a_pcpmax_P12_block[i,j]), ncol = 12)
        Omin <- matrix(c(m_a_pcpmin_P1_block[i,j], m_a_pcpmin_P2_block[i,j], m_a_pcpmin_P3_block[i,j], m_a_pcpmin_P4_block[i,j], m_a_pcpmin_P5_block[i,j], m_a_pcpmin_P6_block[i,j],
                         m_a_pcpmin_P7_block[i,j], m_a_pcpmin_P8_block[i,j], m_a_pcpmin_P9_block[i,j], m_a_pcpmin_P10_block[i,j], m_a_pcpmin_P11_block[i,j], m_a_pcpmin_P12_block[i,j]), ncol = 12)
        Orng <- matrix(c(m_a_pcprng_P1_block[i,j], m_a_pcprng_P2_block[i,j], m_a_pcprng_P3_block[i,j], m_a_pcprng_P4_block[i,j], m_a_pcprng_P5_block[i,j], m_a_pcprng_P6_block[i,j],
                         m_a_pcprng_P7_block[i,j], m_a_pcprng_P8_block[i,j], m_a_pcprng_P9_block[i,j], m_a_pcprng_P10_block[i,j], m_a_pcprng_P11_block[i,j], m_a_pcprng_P12_block[i,j]), ncol = 12)
        
        # create cost map ----------
        minofmaxs <- pcpmax
        maxofmins <- pcpmin
        minofmaxs[[1]][minofmaxs[[1]] > Omax[1]] <- Omax[1]; maxofmins[[1]][maxofmins[[1]] < Omin[1]] <- Omin[1]
        minofmaxs[[2]][minofmaxs[[2]] > Omax[2]] <- Omax[2]; maxofmins[[2]][maxofmins[[2]] < Omin[2]] <- Omin[2]
        minofmaxs[[3]][minofmaxs[[3]] > Omax[3]] <- Omax[3]; maxofmins[[3]][maxofmins[[3]] < Omin[3]] <- Omin[3]
        minofmaxs[[4]][minofmaxs[[4]] > Omax[4]] <- Omax[4]; maxofmins[[4]][maxofmins[[4]] < Omin[4]] <- Omin[4]
        minofmaxs[[5]][minofmaxs[[5]] > Omax[5]] <- Omax[5]; maxofmins[[5]][maxofmins[[5]] < Omin[5]] <- Omin[5]
        minofmaxs[[6]][minofmaxs[[6]] > Omax[6]] <- Omax[6]; maxofmins[[6]][maxofmins[[6]] < Omin[6]] <- Omin[6]
        minofmaxs[[7]][minofmaxs[[7]] > Omax[7]] <- Omax[7]; maxofmins[[7]][maxofmins[[7]] < Omin[7]] <- Omin[7]
        minofmaxs[[8]][minofmaxs[[8]] > Omax[8]] <- Omax[8]; maxofmins[[8]][maxofmins[[8]] < Omin[8]] <- Omin[8]
        minofmaxs[[9]][minofmaxs[[9]] > Omax[9]] <- Omax[9]; maxofmins[[9]][maxofmins[[9]] < Omin[9]] <- Omin[9]
        minofmaxs[[10]][minofmaxs[[10]] > Omax[10]] <- Omax[10]; maxofmins[[10]][maxofmins[[10]] < Omin[10]] <- Omin[10]
        minofmaxs[[11]][minofmaxs[[11]] > Omax[11]] <- Omax[11]; maxofmins[[11]][maxofmins[[11]] < Omin[11]] <- Omin[11]
        minofmaxs[[12]][minofmaxs[[12]] > Omax[12]] <- Omax[12]; maxofmins[[12]][maxofmins[[12]] < Omin[12]] <- Omin[12] 
        degreesoverlap <- minofmaxs - maxofmins
        similaritymaps <- degreesoverlap / (sqrt((as.vector(Orng) * pcprng))) # Janzen 1967
        similaritymap_stack <- similaritymaps; similaritymaps <- similaritymaps[[1]]; for(m in 2:12){similaritymaps <- similaritymaps + similaritymap_stack[[m]]} # Janzen 1967 cont.
        cost_map <- abs(similaritymaps - abs(max(values(similaritymaps), na.rm = T)))
        
        # generate all paths a_j -> B ----------
        mypaths <- CalculateLCP(dtm = cost_map,
                                origin = coordinates(data.frame(lon = m_a_lons_block[i,j],lat = m_a_lats_block[i,j])),
                                destin = destinations)
        
        # Populate plen matrix, populate mcost matrix, store paths ----------
        # [1a] ... if the ORIGIN has precipitation data ... begin attempt to record results ...
        if(!is.na(mypaths$plen[1])){
          mypath_mc_mcosts <- c()
          mypath_mc_plens <- c()
          
          # for every destination for origin cell aj.....
          for (k in 1:length(mypaths$sPath)) {
            print(paste0("starting species ", i, " origin ", j, " destination ", k, " PRECIP"))
            mypath_mc_mcost <- mypaths$mcost[k]
            mypath_mc_plen <- mypaths$plen[k]
            
            # [2a] ... if the DESTINATION is distinct from the ORIGIN ... record results for that destination 
            if(mypath_mc_mcost != "sPath_extentpoint"){
              mypath_mc_mcosts <- c(mypath_mc_mcosts, mypath_mc_mcost)
              mypath_mc_plens <- c(mypath_mc_plens, mypath_mc_plen)
            }
            
            # [2b] ... if the DESTINATION is not distinct from the ORIGIN ... record results as sympatric (no cost) for that destination 
            #       ***NOTE: these points will STILL show up as "sPath_extentpoint" in the mcosts and plen matrices in order to IDENTIFY these cases consistently. ***
            if(mypath_mc_mcost == "sPath_extentpoint"){
              mypath_mc_mcosts <- c(mypath_mc_mcosts, 0)
              mypath_mc_plens <- c(mypath_mc_plens, 0)
            }
          }
          
          # from this set of paths, select the shortest of the cheapest paths ... this is the LCP for origin ai
          min_cost_for_cheapest_ids <- which(mypath_mc_mcosts == min(mypath_mc_mcosts, na.rm = T))
          min_length_for_cheapest_ids <- which(mypath_mc_plens == min(mypath_mc_plens[min_cost_for_cheapest_ids], na.rm = T))
          mypath_id <- NA; mypath_id <- intersect(min_cost_for_cheapest_ids, min_length_for_cheapest_ids)[1]
          
          # [3a] ... if there are any good paths for aj --> B ... store the mcost, plength, and dest info; add the spatiallines object for the path aj->B to the growing list for species i
          if (!is.na(mypath_id)){
            savelist_full_paths_pcp <- c(savelist_full_paths_pcp, mypaths$sPath[mypath_id])
            m_aB_pcp_mcosts_block[i,j] <- mypaths$mcost[mypath_id]
            m_aB_pcp_plengths_block[i,j] <- mypaths$plen[mypath_id]
            destination <- destinations[mypath_id,]
            m_a_bblons_pcp_block[i,j] <- destination$lon
            m_a_bblats_pcp_block[i,j] <- destination$lat
            
          # [3b] ... if there are not any good paths for aj --> B ...leave the mcost, plength, and dest info as NA; add an NA to the pathlist for species i
          } else {
            savelist_full_paths_pcp <- c(savelist_full_paths_pcp, NA)
          }
          
        # [1b] ... if the ORIGIN does not have precipitation data ... leave the mcost, plength, and dest info as NA; add an NA to the pathlist for species i    
        } else {
          savelist_full_paths_pcp <- c(savelist_full_paths_pcp, NA)
        }
        
        # clean for speed and to ensure that old obj aren't used in next loop iteration if obj fail to update.
        rm(Omax, Omin, Orng, minofmaxs, maxofmins, similaritymaps, similaritymap_stack, cost_map, mypaths, mypath_mc_mcosts, mypath_mc_plens,
           mypath_mc_mcost, mypath_mc_plen, min_cost_for_cheapest_ids, min_length_for_cheapest_ids, mypath_id, destination)
        
      } # ------------------------------------------------------------------------------------------------------------------------------------
      
    }

  }
  
  # save path list as the end of each species. ----
  if (quantifyElevationalBarriers == TRUE){  # Thermal
    setwd(wdELEVATIONPathOutputs) 
    save(savelist_full_paths_ele, file = paste0(batchNumber, "_ELEVATION_LCP_Paths_for_", cooneyp[i,"Species.1bl"], "_", cooneyp[i,"Species.2bl"],"_origin_",originStart,"_to_",originEnd, ".rdata"))
    savelist_full_paths_ele <- NULL
  }
  
  if (quantifyThermalBarriers == TRUE){  # Thermal
  setwd(wdTHERMALPathOutputs) 
  save(savelist_full_paths_tmp, file = paste0(batchNumber, "_THERMAL_LCP_Paths_for_", cooneyp[i,"Species.1bl"], "_", cooneyp[i,"Species.2bl"],"_origin_",originStart,"_to_",originEnd, ".rdata"))
  savelist_full_paths_tmp <- NULL
  }
  
  if (quantifyPrecipitationBarriers == TRUE){  # Precipitation
    setwd(wdPRECIPITATIONPathOutputs) 
    save(savelist_full_paths_pcp, file = paste0(batchNumber, "_PRECIPITATION_LCP_Paths_for_", cooneyp[i,"Species.1bl"], "_", cooneyp[i,"Species.2bl"],"_origin_",originStart,"_to_",originEnd, ".rdata"))
    savelist_full_paths_pcp <- NULL
  }

}
completed_rows <- i
setwd(wdBatchResults)
save(completed_rows, 
     m_a_bblons_ele_block, m_a_bblats_ele_block, m_aB_ele_mcosts_block, m_aB_ele_plengths_block,
     m_a_bblons_tmp_block, m_a_bblats_tmp_block, m_aB_tmp_mcosts_block, m_aB_tmp_plengths_block,
     m_a_bblons_pcp_block, m_a_bblats_pcp_block, m_aB_pcp_mcosts_block, m_aB_pcp_plengths_block,
     file = paste0(batchNumber, "_Main_Batch_Results_origin_",originStart,"_to_",originEnd,".rdata"))


