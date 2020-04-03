# control block ----------------------------------------------------------------
rm(list=ls())
inwd <- "/Users/VJF/Desktop/000_JMPH_2020/Data"
outwd <- inwd
prefix <- "004"
brstart <- 4
brend <- 5

# load all required packages ---------------------------------------------------
require(rgdal)
require(raster)
require(dplyr)
require(movecost)
require(sp)
require(raster)
require(gdistance)


# load required files & sources  -----------------------------------------------
setwd(inwd)
source("1_getdata_movecost_fxn.R"); rm(movecostAngela, movecostAngela_vOCTDontAdapt)
load(file = "000_Setup_elev_raster_outputs.Rdata")
load(file = "ref_cost_map_&_value.Rdata")
load(file = "cooneyd.Rdata"); mydata <- cooneyd
load(file = paste0("pamMAT", prefix, ".Rdata")); pamMAT <- get(paste0("pamMAT", prefix))


# breaks and setup -------------------------------------------------------------
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
sp_start <- mybreaks[brstart] + 1
sp_end <- mybreaks[brend]
savelist_path_prefix <- prefix
savefile_metadata_prefix <- prefix



# Prepare dataframe for path run -----------------------------------------------
# pregenerate necessary columns that will be populated by the main loop
n_origins <- 100 # number of cells in the origin range, A, to use as origin points

m_a_ids <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
m_a_lons <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
m_a_lats <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
m_a_eles <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
m_a_bblons <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
m_a_bblats <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
m_a_bbeles <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
m_aB_mcosts <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
m_aB_plengths <- matrix(nrow = nrow(mydata), ncol = n_origins, dimnames = list(c(paste0("sp", seq(1, nrow(mydata), 1))), c(paste0("a", seq(1, n_origins, 1)))))
n_species <- nrow(mydata) # number of origin ranges A -- rows in mydata.

# Main Loop --------------------------------------------------------------------
i <- 1; j <- 1; k <- 1
load(file = paste0(savefile_metadata_prefix, "_results_100ptsea.rdata"))
sp_start <- completed_rows + 1
for (i in sp_start:sp_end){
  print(paste0("starting species ", i))
  
  # get coords for A and B -----------------------------------------------------
  # subset pamMAT to create objects for species A range and species B range
  mytemp <- pamMAT[,c("Longitude(x)", "Latitude(y)", paste0(mydata[i, "Species.1bl"]), paste0(mydata[i, "Species.2bl"]))]
  rangeA <- subset(mytemp, mytemp[, 3] == 1)
  rangeB <- subset(mytemp, mytemp[, 4] == 1)
  
  # convert range to coords for species A & B
  coordsA <- data.frame(lon = rangeA[ , 1], lat = rangeA[ , 2]); coordinates(coordsA) <- c("lon", "lat"); crs(coordsA) <- "+proj=wag4 +lon_0=0 +ellps=WGS84"
  coordsB <- data.frame(lon = rangeB[ , 1], lat = rangeB[ , 2]); coordinates(coordsB) <- c("lon", "lat"); crs(coordsB) <- "+proj=wag4 +lon_0=0 +ellps=WGS84"
  
  # generate origins information ---------------------------
  origins <- min(n_origins, mydata$n_pam_cells_sp1[i]) # get number of origins
  if(origins <= n_origins){
    origins <- seq(from = 1, to = origins) # if less than n_origins origins in range A, do them in order...else..
  } else {
    origins <- sample(x = mydata$n_pam_cells_sp1[i], size = origins, replace = F) # sample origins indices in range A
  }
  origins.id <- origins
  origins <- data.frame(lon = coordinates(coordsA)[origins, 1], lat = coordinates(coordsA)[origins, 2]) # get origins coords
  
  savelist_full_paths <- c()
  for (j in 1:nrow(origins)) {
    print(paste0("species ", i, " origin ", j))
    
    # Store information for the current origin cell in the mydata dataframe ----
    m_a_ids[i,j] <- origins.id[j]
    m_a_lons[i,j] <- origins[j, "lon"]
    m_a_lats[i,j] <- origins[j, "lat"]
    
    # a_value <- extract(x = elev_raster_scaled, y = origins[j,]); mydata[i, paste0("a_ele_", j)] <- a_value # elevation
    a_value <- extract(x = elev_raster_scaled, y = origins[j,]); m_a_eles[i,j] <- a_value # elevation
    
    # transform the cost map to be for the focal origin cell -------------------
    new_less_old <- a_value - ref_value # transform value
    new_cost_map <- ref_cost_map # create new map from reference template
    values(new_cost_map) <- values(new_cost_map) - new_less_old # update new map values
    values(new_cost_map) <- abs(values(new_cost_map))
    # extract(x = new_cost_map, y = origins[j,]) # this check should always be 0
    
    # generate destinations information ---------------------------
    destinations <- seq(from = 1, to = mydata$n_pam_cells_sp2[i]) # doing all destinations, so do them sequentially to make data collection easy. 
    destinations <- data.frame(lon = coordinates(coordsB)[destinations, 1], lat = coordinates(coordsB)[destinations, 2]) # select coords to use as destination points
    
    # generate all paths a_j -> B ----------------------------------
    mypaths <- movecostAngela_vOCTDontAdapt_multdestin(dtm = new_cost_map,
                                                       origin = coordinates(origins[j,]),
                                                       destin = coordinates(destinations))
    
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
    m_a_bbeles[i,j] <- extract(x = elev_raster_scaled, y = destination) 
  }
  
  setwd(outwd)
  save(savelist_full_paths, file = paste0(savelist_path_prefix, "_FULLPATH_results_for_", mydata[i,"Species.1bl"], "_", mydata[i,"Species.2bl"], ".rdata"))
  rm(savelist_full_paths)
  
  if(i %% 5 == 0){
    completed_rows <- i - 1
    setwd(outwd)
    save(sp_start, sp_end, completed_rows, mydata, 
         m_a_ids, m_a_lons, m_a_lats, m_a_eles, 
         m_a_bblons, m_a_bblats, m_a_bbeles, 
         m_aB_mcosts, m_aB_plengths,
         file = paste0(savefile_metadata_prefix, "_results_100ptsea.rdata"))
  }
  
}
completed_rows <- i - 1
setwd(outwd)
save(sp_start, sp_end, completed_rows, mydata, 
     m_a_ids, m_a_lons, m_a_lats, m_a_eles, 
     m_a_bblons, m_a_bblats, m_a_bbeles, 
     m_aB_mcosts, m_aB_plengths,
     file = paste0(savefile_metadata_prefix, "_results_100ptsea.rdata"))
myend <- Sys.time()

