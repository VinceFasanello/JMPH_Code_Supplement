rm(list=ls()); gc() # clear workspace 

# control block ----------------------------------------------------------------
batchNumber <- 90
originStart <- 1
originEnd <- 5000

# load all required packages ---------------------------------------------------
require(gdistance) # will load dependencies: raster, sp, igraph, Matrix
require(rgeos)

# directory paths --------------------------------------------------------------
wdin <- "/scratch/vincefasanello/inputs"
wdout <- "/scratch/vincefasanello/outputs_e"

# inputs -----------------------------------------------------------------------
setwd(wdin)
source("CalculateLCP_SOURCE.R")
load(file = "mybreaks.rdata")
load(file = paste0("cbPAM", batchNumber, ".rdata")); cbPAM <- get(paste0("cbPAM", batchNumber)); rm(list = paste0("cbPAM", batchNumber)); gc()
load(file = "Elev_raster.rdata")
# load(file = "tasmax_rasters_VJF.rdata"); load(file = "tasmin_rasters_VJF.rdata"); load(file = "tasrng_rasters_VJF.rdata")
# load(file = "pcpmax_rasters_VJF.rdata"); load(file = "pcpmin_rasters_VJF.rdata"); load(file = "pcprng_rasters_VJF.rdata")
load(file = paste0("cooneyp", batchNumber, ".rdata")); cooneyp <- get(paste0("cooneyp", batchNumber)); rm(list = paste0("cooneyp", batchNumber)); gc()
load(file = paste0("a_mats_block", batchNumber, ".rdata"))

setwd(wdout)
# load(file = paste0(batchNumber, "_Main_Batch_Results_origin_",originStart,"_to_",originEnd,"_E.rdata")) # willy only exist if there is a hotstart
if (exists("completed_rows") == FALSE){completed_rows <- 0} # if no data exist, start on the 1st species
sp_start <- completed_rows + 1 # if data exist, start on the next species that is not complete. 

# breaks control file ----------------------------------------------------------
breakStart_index <- batchNumber
breakEnd_index <- breakStart_index + 1
breakStart_index <- mybreaks[breakStart_index] + 1
breakEnd_index <- mybreaks[breakEnd_index]

# Main Loop --------------------------------------------------------------------
for (i in sp_start:nrow(cooneyp)){
  # print(paste0("starting species ", i))
  # destination cells (range B) --------
  destinations <- cbPAM[,c("Longitude(x)", "Latitude(y)", paste0(cooneyp[i, "Species.2bl"]))]
  destinations <- subset(destinations, destinations[, 3] == 1);
  destinations <- data.frame(lon = destinations[ , 1], lat = destinations[ , 2]);
  coordinates(destinations) <- c("lon", "lat");
  crs(destinations) <- crs(Elev_raster)
  # full path savelists ----------------
  savelist_full_paths_ele <- c()
  for (j in originStart:originEnd){
    # [0a] ... if there exists an ORIGIN j for species i ... (i.e., if we haven't run out of origins) ... attempt to find LCP for origin ai_j ....
    if (!is.na(m_a_ids_block[i,j])){
      # print(paste0("starting species ", i, " origin ", j))
      # Elev Barrier -------------------------------------------------------------------------------------------------------------------------
      # create cost map ----------
      cost_map <- Elev_raster
      values(cost_map) <- abs(values(cost_map) - m_a_eles_block[i,j])
      # generate all paths a_j -> B ----------
      mypaths <- CalculateLCP(dtm = cost_map,
                              origin = coordinates(data.frame(lon = m_a_lons_block[i,j],lat = m_a_lats_block[i,j])),
                              destin = destinations)
      # plot(log(cost_map+1), col = terrain.colors(255)); points(coordinates(data.frame(lon = m_a_lons_block[i,j],lat = m_a_lats_block[i,j]))); lines(mypaths$sPath)
      # Populate plen matrix, populate mcost matrix, store paths ----------
      # [1a] ... if the ORIGIN has elevation data ... begin attempt to record results ...
      if(!is.na(mypaths$plen[1])){
        mypath_mc_mcosts <- c()
        mypath_mc_plens <- c()
        # for every destination for origin cell aj.....
        for (k in 1:length(mypaths$sPath)) {
          # print(paste0("starting species ", i, " origin ", j, " destination ", k, " ELEVATION"))
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
        } else {savelist_full_paths_ele <- c(savelist_full_paths_ele, NA)}
      # [1b] ... if the ORIGIN does not have thermal data ... leave the mcost, plength, and dest info as NA; add an NA to the pathlist for species i    
      } else {savelist_full_paths_ele <- c(savelist_full_paths_ele, NA)}
      rm(cost_map, mypaths, mypath_mc_mcosts, mypath_mc_plens,
         mypath_mc_mcost, mypath_mc_plen, min_cost_for_cheapest_ids, min_length_for_cheapest_ids, mypath_id, destination)
      # ------------------------------------------------------------------------------------------------------------------------------------
    }
  }
  # save at the end of each species. ----
  save(savelist_full_paths_ele, file = paste0(batchNumber, "_ELEVATION_LCP_Paths_for_", cooneyp[i,"Species.1bl"], "_", cooneyp[i,"Species.2bl"],"_origin_",originStart,"_to_",originEnd, ".rdata"))
  completed_rows <- i
  save(completed_rows, 
       m_a_bblons_ele_block, m_a_bblats_ele_block, m_aB_ele_mcosts_block, m_aB_ele_plengths_block,
       file = paste0(batchNumber, "_Main_Batch_Results_origin_",originStart,"_to_",originEnd,"_E.rdata"))
  print(paste0("finished species ", i))
  
}



