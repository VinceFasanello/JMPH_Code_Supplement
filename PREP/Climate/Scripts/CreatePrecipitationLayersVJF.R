rm(list=ls())
require(ncdf4)
require(raster)
wdPAM <- "~/JMPH/PREP/PAM/Data"
wdclimate <- "~/JMPH/PREP/Climate_Data/Data"

# MEAN TEMPERATURE -------------------------------------------------------------
setwd(wdclimate) # read ncdf file with raw data
precip.nc = nc_open("pr_Amon_CCSM4_historical_r1i1p1_185001-200512.nc"); # print(precip.nc)

# extract coordinate system and temp data for each 0.5x0.5 cell -----
lat_t = ncvar_get( nc = precip.nc, varid = "lat")          
lon_t = ncvar_get( nc = precip.nc, varid = "lon") 
myprecip = ncvar_get( nc = precip.nc, varid = "pr")  # 3d array with 288 (lon) x 192 (lat) x 1872 (months in 114 years since 1901)
myprecip <- myprecip[,,601:1872] # restrict to years of interest

# create empty dataframes to hold the calculated climate variables for each grid cell. -----
P01_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P01_prmax) <- lat_t; colnames(P01_prmax) <- lon_t
P02_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P02_prmax) <- lat_t; colnames(P02_prmax) <- lon_t
P03_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P03_prmax) <- lat_t; colnames(P03_prmax) <- lon_t
P04_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P04_prmax) <- lat_t; colnames(P04_prmax) <- lon_t
P05_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P05_prmax) <- lat_t; colnames(P05_prmax) <- lon_t
P06_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P06_prmax) <- lat_t; colnames(P06_prmax) <- lon_t
P07_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P07_prmax) <- lat_t; colnames(P07_prmax) <- lon_t
P08_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P08_prmax) <- lat_t; colnames(P08_prmax) <- lon_t
P09_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P09_prmax) <- lat_t; colnames(P09_prmax) <- lon_t
P10_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P10_prmax) <- lat_t; colnames(P10_prmax) <- lon_t
P11_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P11_prmax) <- lat_t; colnames(P11_prmax) <- lon_t
P12_prmax <- as.data.frame(matrix(NA, 192,288)); row.names(P12_prmax) <- lat_t; colnames(P12_prmax) <- lon_t

P01_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P01_prmin) <- lat_t; colnames(P01_prmin) <- lon_t
P02_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P02_prmin) <- lat_t; colnames(P02_prmin) <- lon_t
P03_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P03_prmin) <- lat_t; colnames(P03_prmin) <- lon_t
P04_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P04_prmin) <- lat_t; colnames(P04_prmin) <- lon_t
P05_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P05_prmin) <- lat_t; colnames(P05_prmin) <- lon_t
P06_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P06_prmin) <- lat_t; colnames(P06_prmin) <- lon_t
P07_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P07_prmin) <- lat_t; colnames(P07_prmin) <- lon_t
P08_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P08_prmin) <- lat_t; colnames(P08_prmin) <- lon_t
P09_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P09_prmin) <- lat_t; colnames(P09_prmin) <- lon_t
P10_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P10_prmin) <- lat_t; colnames(P10_prmin) <- lon_t
P11_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P11_prmin) <- lat_t; colnames(P11_prmin) <- lon_t
P12_prmin <- as.data.frame(matrix(NA, 192,288)); row.names(P12_prmin) <- lat_t; colnames(P12_prmin) <- lon_t

P01_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P01_prmean) <- lat_t; colnames(P01_prmean) <- lon_t
P02_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P02_prmean) <- lat_t; colnames(P02_prmean) <- lon_t
P03_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P03_prmean) <- lat_t; colnames(P03_prmean) <- lon_t
P04_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P04_prmean) <- lat_t; colnames(P04_prmean) <- lon_t
P05_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P05_prmean) <- lat_t; colnames(P05_prmean) <- lon_t
P06_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P06_prmean) <- lat_t; colnames(P06_prmean) <- lon_t
P07_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P07_prmean) <- lat_t; colnames(P07_prmean) <- lon_t
P08_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P08_prmean) <- lat_t; colnames(P08_prmean) <- lon_t
P09_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P09_prmean) <- lat_t; colnames(P09_prmean) <- lon_t
P10_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P10_prmean) <- lat_t; colnames(P10_prmean) <- lon_t
P11_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P11_prmean) <- lat_t; colnames(P11_prmean) <- lon_t
P12_prmean <- as.data.frame(matrix(NA, 192,288)); row.names(P12_prmean) <- lat_t; colnames(P12_prmean) <- lon_t

# compute descriptive variables (POPULATE) -----
for ( i in 1:length(lon_t) ) {
  for (j in 1:length(lat_t) ) {
    
    # for each point in space generate a matrix in which cols are months and rows are years
    Data <- as.data.frame(matrix(myprecip[i,j,], length(myprecip[i,j,])/12, 12, byrow=T))
    colnames(Data) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    row.names(Data) <- (1:(length(myprecip[i,j,])/12)) + 1900
    
    # populate dataframes from above. 
    P01_prmax[j,i] <- max(Data[,c(1,2,3)], na.rm = T)
    P02_prmax[j,i] <- max(Data[,c(2,3,4)], na.rm = T)
    P03_prmax[j,i] <- max(Data[,c(3,4,5)], na.rm = T)
    P04_prmax[j,i] <- max(Data[,c(4,5,6)], na.rm = T)
    P05_prmax[j,i] <- max(Data[,c(5,6,7)], na.rm = T)
    P06_prmax[j,i] <- max(Data[,c(6,7,8)], na.rm = T)
    P07_prmax[j,i] <- max(Data[,c(7,8,9)], na.rm = T)
    P08_prmax[j,i] <- max(Data[,c(8,9,10)], na.rm = T)
    P09_prmax[j,i] <- max(Data[,c(9,10,11)], na.rm = T)
    P10_prmax[j,i] <- max(Data[,c(10,11,12)], na.rm = T)
    P11_prmax[j,i] <- max(Data[,c(11,12,1)], na.rm = T)
    P12_prmax[j,i] <- max(Data[,c(12,1,2)], na.rm = T)
    
    P01_prmin[j,i] <- min(Data[,c(1,2,3)], na.rm = T)
    P02_prmin[j,i] <- min(Data[,c(2,3,4)], na.rm = T)
    P03_prmin[j,i] <- min(Data[,c(3,4,5)], na.rm = T)
    P04_prmin[j,i] <- min(Data[,c(4,5,6)], na.rm = T)
    P05_prmin[j,i] <- min(Data[,c(5,6,7)], na.rm = T)
    P06_prmin[j,i] <- min(Data[,c(6,7,8)], na.rm = T)
    P07_prmin[j,i] <- min(Data[,c(7,8,9)], na.rm = T)
    P08_prmin[j,i] <- min(Data[,c(8,9,10)], na.rm = T)
    P09_prmin[j,i] <- min(Data[,c(9,10,11)], na.rm = T)
    P10_prmin[j,i] <- min(Data[,c(10,11,12)], na.rm = T)
    P11_prmin[j,i] <- min(Data[,c(11,12,1)], na.rm = T)
    P12_prmin[j,i] <- min(Data[,c(12,1,2)], na.rm = T)
    
    P01_prmean[j,i] <- mean(unlist(Data[,c(1,2,3)]), na.rm = T)
    P02_prmean[j,i] <- mean(unlist(Data[,c(2,3,4)]), na.rm = T)
    P03_prmean[j,i] <- mean(unlist(Data[,c(3,4,5)]), na.rm = T)
    P04_prmean[j,i] <- mean(unlist(Data[,c(4,5,6)]), na.rm = T)
    P05_prmean[j,i] <- mean(unlist(Data[,c(5,6,7)]), na.rm = T)
    P06_prmean[j,i] <- mean(unlist(Data[,c(6,7,8)]), na.rm = T)
    P07_prmean[j,i] <- mean(unlist(Data[,c(7,8,9)]), na.rm = T)
    P08_prmean[j,i] <- mean(unlist(Data[,c(8,9,10)]), na.rm = T)
    P09_prmean[j,i] <- mean(unlist(Data[,c(9,10,11)]), na.rm = T)
    P10_prmean[j,i] <- mean(unlist(Data[,c(10,11,12)]), na.rm = T)
    P11_prmean[j,i] <- mean(unlist(Data[,c(11,12,1)]), na.rm = T)
    P12_prmean[j,i] <- mean(unlist(Data[,c(12,1,2)]), na.rm = T)
  } 
  print(paste(i*1.25, " degrees of lon traversed!"))
}


# Create rasters -------------------------------------------
setwd(wdclimate)
pdf(file = "precip_maps_MAX.pdf", width = 17, height = 11)
setwd(wdPAM); load(file = "LonLat_BirdPAM_raster.rdata"); setwd(wdclimate) 
for (i in c("P01", "P02", "P03", "P04", "P05", "P06", 
            "P07", "P08", "P09", "P10", "P11", "P12")) {
  temp <- raster(ncol=288, nrow=192, xmn=-180, xmx=180, ymn=-90, ymx=90)
  values(temp) <- as.vector(t(get(paste0(i, "_prmax"))[seq(dim(get(paste0(i, "_prmax")))[1],1,by = -1),]))
  extent(temp) <- extent(c(0, 360, -90, 90))
  temp <- rotate(temp)
  extent(temp) <- extent(c(-180, 180, -90, 90))
  temp <- projectRaster(temp, LonLat_BirdPAM_raster)
  plot(temp, main=paste0(i, "_prmax"))
  assign(paste0(i, "_prmax_raster"), temp)
}
dev.off()


setwd(wdclimate)
pdf(file = "precip_maps_MIN.pdf", width = 17, height = 11)
setwd(wdPAM); load(file = "LonLat_BirdPAM_raster.rdata"); setwd(wdclimate) 
for (i in c("P01", "P02", "P03", "P04", "P05", "P06", 
            "P07", "P08", "P09", "P10", "P11", "P12")) {
  temp <- raster(ncol=288, nrow=192, xmn=-180, xmx=180, ymn=-90, ymx=90)
  values(temp) <- as.vector(t(get(paste0(i, "_prmin"))[seq(dim(get(paste0(i, "_prmin")))[1],1,by = -1),]))
  extent(temp) <- extent(c(0, 360, -90, 90))
  temp <- rotate(temp)
  extent(temp) <- extent(c(-180, 180, -90, 90))
  temp <- projectRaster(temp, LonLat_BirdPAM_raster)
  plot(temp, main=paste0(i, "_prmin"))
  assign(paste0(i, "_prmin_raster"), temp)
}
dev.off()


setwd(wdclimate)
pdf(file = "precip_maps_MEAN.pdf", width = 17, height = 11)
setwd(wdPAM); load(file = "LonLat_BirdPAM_raster.rdata"); setwd(wdclimate) 
for (i in c("P01", "P02", "P03", "P04", "P05", "P06", 
            "P07", "P08", "P09", "P10", "P11", "P12")) {
  temp <- raster(ncol=288, nrow=192, xmn=-180, xmx=180, ymn=-90, ymx=90)
  values(temp) <- as.vector(t(get(paste0(i, "_prmean"))[seq(dim(get(paste0(i, "_prmean")))[1],1,by = -1),]))
  extent(temp) <- extent(c(0, 360, -90, 90))
  temp <- rotate(temp)
  extent(temp) <- extent(c(-180, 180, -90, 90))
  temp <- projectRaster(temp, LonLat_BirdPAM_raster)
  plot(temp, main=paste0(i, "_prmean"))
  assign(paste0(i, "_prmean_raster"), temp)
}
dev.off()


rm(P01_prmax, P02_prmax, P03_prmax, P04_prmax, P05_prmax, P06_prmax,
   P07_prmax, P08_prmax, P09_prmax, P10_prmax, P11_prmax, P12_prmax,
   P01_prmin, P02_prmin, P03_prmin, P04_prmin, P05_prmin, P06_prmin,
   P07_prmin, P08_prmin, P09_prmin, P10_prmin, P11_prmin, P12_prmin,
   P01_prmean, P02_prmean, P03_prmean, P04_prmean, P05_prmean, P06_prmean,
   P07_prmean, P08_prmean, P09_prmean, P10_prmean, P11_prmean, P12_prmean,
   Data, temp, myprecip, precip.nc, i, j, lat_t, lon_t) # tidy.




P01_prrng_raster <- P01_prmax_raster - P01_prmin_raster
P02_prrng_raster <- P02_prmax_raster - P02_prmin_raster
P03_prrng_raster <- P03_prmax_raster - P03_prmin_raster
P04_prrng_raster <- P04_prmax_raster - P04_prmin_raster
P05_prrng_raster <- P05_prmax_raster - P05_prmin_raster
P06_prrng_raster <- P06_prmax_raster - P06_prmin_raster
P07_prrng_raster <- P07_prmax_raster - P07_prmin_raster
P08_prrng_raster <- P08_prmax_raster - P08_prmin_raster
P09_prrng_raster <- P09_prmax_raster - P09_prmin_raster
P10_prrng_raster <- P10_prmax_raster - P10_prmin_raster
P11_prrng_raster <- P11_prmax_raster - P11_prmin_raster
P12_prrng_raster <- P12_prmax_raster - P12_prmin_raster
setwd(wdclimate)
pdf(file = "precip_maps_RNG.pdf", width = 17, height = 11)
for (i in c("P01", "P02", "P03", "P04", "P05", "P06", 
            "P07", "P08", "P09", "P10", "P11", "P12")) {
  plot(get(paste0(i,"_prrng_raster")), main=paste0(i, "_prrng"))
}
dev.off()



# save(P01_prmax_raster, P02_prmax_raster, P03_prmax_raster, P04_prmax_raster, P05_prmax_raster, P06_prmax_raster,
#      P07_prmax_raster, P08_prmax_raster, P09_prmax_raster, P10_prmax_raster, P11_prmax_raster, P12_prmax_raster,
#      file = "prmax_rasters_indiv_VJF.rdata")
# save(P01_prmin_raster, P02_prmin_raster, P03_prmin_raster, P04_prmin_raster, P05_prmin_raster, P06_prmin_raster,
#      P07_prmin_raster, P08_prmin_raster, P09_prmin_raster, P10_prmin_raster, P11_prmin_raster, P12_prmin_raster,
#      file = "prmin_rasters_indiv_VJF.rdata")
# save(P01_prmean_raster, P02_prmean_raster, P03_prmean_raster, P04_prmean_raster, P05_prmean_raster, P06_prmean_raster,
#      P07_prmean_raster, P08_prmean_raster, P09_prmean_raster, P10_prmean_raster, P11_prmean_raster, P12_prmean_raster,
#      file = "prmean_rasters_indiv_VJF.rdata")
# save(P01_prrng_raster, P02_prrng_raster, P03_prrng_raster, P04_prrng_raster, P05_prrng_raster, P06_prrng_raster,
#      P07_prrng_raster, P08_prrng_raster, P09_prrng_raster, P10_prrng_raster, P11_prrng_raster, P12_prrng_raster,
#      file = "prrng_rasters_indiv_VJF.rdata")





pcp <- stack(x = c(P01_prmean_raster, P02_prmean_raster, P03_prmean_raster, P04_prmean_raster, P05_prmean_raster, P06_prmean_raster,
                   P07_prmean_raster, P08_prmean_raster, P09_prmean_raster, P10_prmean_raster, P11_prmean_raster, P12_prmean_raster))
rm(P01_prmean_raster, P02_prmean_raster, P03_prmean_raster, P04_prmean_raster, P05_prmean_raster, P06_prmean_raster,
   P07_prmean_raster, P08_prmean_raster, P09_prmean_raster, P10_prmean_raster, P11_prmean_raster, P12_prmean_raster)


pcpmin <- stack(x = c(P01_prmin_raster, P02_prmin_raster, P03_prmin_raster, P04_prmin_raster, P05_prmin_raster, P06_prmin_raster,
                      P07_prmin_raster, P08_prmin_raster, P09_prmin_raster, P10_prmin_raster, P11_prmin_raster, P12_prmin_raster))
rm(P01_prmin_raster, P02_prmin_raster, P03_prmin_raster, P04_prmin_raster, P05_prmin_raster, P06_prmin_raster,
   P07_prmin_raster, P08_prmin_raster, P09_prmin_raster, P10_prmin_raster, P11_prmin_raster, P12_prmin_raster)


pcpmax <- stack(x = c(P01_prmax_raster, P02_prmax_raster, P03_prmax_raster, P04_prmax_raster, P05_prmax_raster, P06_prmax_raster,
                      P07_prmax_raster, P08_prmax_raster, P09_prmax_raster, P10_prmax_raster, P11_prmax_raster, P12_prmax_raster))
rm(P01_prmax_raster, P02_prmax_raster, P03_prmax_raster, P04_prmax_raster, P05_prmax_raster, P06_prmax_raster,
   P07_prmax_raster, P08_prmax_raster, P09_prmax_raster, P10_prmax_raster, P11_prmax_raster, P12_prmax_raster)


pcprng <- stack(x = c(P01_prrng_raster, P02_prrng_raster, P03_prrng_raster, P04_prrng_raster, P05_prrng_raster, P06_prrng_raster,
                      P07_prrng_raster, P08_prrng_raster, P09_prrng_raster, P10_prrng_raster, P11_prrng_raster, P12_prrng_raster))
rm(P01_prrng_raster, P02_prrng_raster, P03_prrng_raster, P04_prrng_raster, P05_prrng_raster, P06_prrng_raster,
   P07_prrng_raster, P08_prrng_raster, P09_prrng_raster, P10_prrng_raster, P11_prrng_raster, P12_prrng_raster)


names(pcpmax) <- c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12")
names(pcpmin) <- paste0("pcpmin_", names(pcpmax))
names(pcprng) <- paste0("pcprng_", names(pcpmax))
names(pcpmax) <- paste0("pcpmax_", names(pcpmax))


setwd(wdclimate)
save(pcp,
     file = "pcp_rasters_VJF.rdata")
save(pcpmin,
     file = "pcpmin_rasters_VJF.rdata")
save(pcpmax,
     file = "pcpmax_rasters_VJF.rdata")
save(pcprng,
     file = "pcprng_rasters_VJF.rdata")


# create PCP raster ----------------------------------------
setwd("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/Climate/Data") 
load(file = "pcp_rasters_VJF.rdata")
PCP <- sum(pcp, na.rm = T)
plot(PCP)
dim(pcp[[1]]) == dim(PCP) # looks good. 
save(PCP, file = "PCP_raster_VJF.rdata")
# ---------------------------------------------------------
