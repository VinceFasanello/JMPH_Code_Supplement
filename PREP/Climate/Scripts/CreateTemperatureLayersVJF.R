rm(list=ls())
require(ncdf4)
require(raster)
wdPAM <- "~/JMPH/PREP/PAM/Data"
wdclimate <- "~/JMPH/PREP/Climate_Data/Data"

# MEAN TEMPERATURE -------------------------------------------------------------
setwd(wdclimate) # read ncdf file with raw data
temp.nc = nc_open("tas_Amon_CCSM4_historical_r1i1p1_185001-200512.nc"); # print(temp.nc)

# extract coordinate system and temp data for each 0.5x0.5 cell -----
lat_t = ncvar_get( nc = temp.nc, varid = "lat")          
lon_t = ncvar_get( nc = temp.nc, varid = "lon") 
mytemp = ncvar_get( nc = temp.nc, varid = "tas")  # 3d array with 288 (lon) x 192 (lat) x 1872 (months in 114 years since 1901)
mytemp <- mytemp[,,601:1872] # restrict to years of interest

# create empty dataframes to hold the calculated climate variables for each grid cell. -----
# MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(MeanT) <- lat_t; colnames(MeanT) <- lon_t # NOT RUN CURRENTLY
Month01_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month01_MeanT) <- lat_t; colnames(Month01_MeanT) <- lon_t
Month02_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month02_MeanT) <- lat_t; colnames(Month02_MeanT) <- lon_t
Month03_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month03_MeanT) <- lat_t; colnames(Month03_MeanT) <- lon_t
Month04_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month04_MeanT) <- lat_t; colnames(Month04_MeanT) <- lon_t
Month05_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month05_MeanT) <- lat_t; colnames(Month05_MeanT) <- lon_t
Month06_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month06_MeanT) <- lat_t; colnames(Month06_MeanT) <- lon_t
Month07_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month07_MeanT) <- lat_t; colnames(Month07_MeanT) <- lon_t
Month08_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month08_MeanT) <- lat_t; colnames(Month08_MeanT) <- lon_t
Month09_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month09_MeanT) <- lat_t; colnames(Month09_MeanT) <- lon_t
Month10_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month10_MeanT) <- lat_t; colnames(Month10_MeanT) <- lon_t
Month11_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month11_MeanT) <- lat_t; colnames(Month11_MeanT) <- lon_t
Month12_MeanT <- as.data.frame(matrix(NA, 192,288)); row.names(Month12_MeanT) <- lat_t; colnames(Month12_MeanT) <- lon_t

# compute descriptive variables (POPULATE) -----
for ( i in 1:length(lon_t) ) {
  for (j in 1:length(lat_t) ) {
    
    # for each point in space generate a matrix in which cols are months and rows are years
    Data <- as.data.frame(matrix(mytemp[i,j,], length(mytemp[i,j,])/12, 12, byrow=T))
    colnames(Data) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    row.names(Data) <- (1:(length(mytemp[i,j,])/12)) + 1900

    # populate dataframes from above. 
    # MeanT[j,i] <- mean(apply(Data, 1, mean, na.rm = T), na.rm = T) # NOT RUN CURRENTLY
    Month01_MeanT[j,i] <- mean(Data[,1], na.rm = T)
    Month02_MeanT[j,i] <- mean(Data[,2], na.rm = T)
    Month03_MeanT[j,i] <- mean(Data[,3], na.rm = T)
    Month04_MeanT[j,i] <- mean(Data[,4], na.rm = T)
    Month05_MeanT[j,i] <- mean(Data[,5], na.rm = T)
    Month06_MeanT[j,i] <- mean(Data[,6], na.rm = T)
    Month07_MeanT[j,i] <- mean(Data[,7], na.rm = T)
    Month08_MeanT[j,i] <- mean(Data[,8], na.rm = T)
    Month09_MeanT[j,i] <- mean(Data[,9], na.rm = T)
    Month10_MeanT[j,i] <- mean(Data[,10], na.rm = T)
    Month11_MeanT[j,i] <- mean(Data[,11], na.rm = T)
    Month12_MeanT[j,i] <- mean(Data[,12], na.rm = T)
  } 
  print(paste(i*1.25, " degrees of lon traversed!"))
}

# Create rasters -------------------------------------------
setwd(wdclimate) 
pdf(file = "tas_maps_MeanT.pdf", width = 17, height = 11)
setwd(wdPAM); load(file = "LonLat_BirdPAM_raster.rdata"); setwd(wdclimate) 
for (i in c("Month01", "Month02", "Month03", "Month04", "Month05", "Month06", 
            "Month07", "Month08", "Month09", "Month10", "Month11", "Month12")) {
  temp <- raster(ncol=288, nrow=192, xmn=-180, xmx=180, ymn=-90, ymx=90)
  values(temp) <- as.vector(t(get(paste0(i, "_MeanT"))[seq(dim(get(paste0(i, "_MeanT")))[1],1,by = -1),]))
  extent(temp) <- extent(c(0, 360, -90, 90))
  temp <- rotate(temp)
  extent(temp) <- extent(c(-180, 180, -90, 90))
  temp <- projectRaster(temp, LonLat_BirdPAM_raster)
  plot(temp, main=paste0(i, "_MeanT"))
  assign(paste0(i, "_MeanT_raster"), temp)
}
dev.off()
rm(Month01_MeanT, Month02_MeanT, Month03_MeanT, Month04_MeanT, Month05_MeanT, Month06_MeanT,
   Month07_MeanT, Month08_MeanT, Month09_MeanT, Month10_MeanT, Month11_MeanT, Month12_MeanT,
   Data, temp, mytemp, temp.nc, i, j, lat_t, lon_t) # tidy.












# read ncdf file with raw data
setwd(wdclimate) # read ncdf file with raw data
tempmax.nc = nc_open("tasmax_Amon_CCSM4_historical_r1i1p1_185001-200512.nc")

# extract coordinate system and temp data for each 0.5x0.5 cell -----
lat_t = ncvar_get( nc = tempmax.nc, varid = "lat")          
lon_t = ncvar_get( nc = tempmax.nc, varid = "lon") 
mytemp = ncvar_get( nc = tempmax.nc, varid = "tasmax")  # 3d array with 288 (lon) x 192 (lat) x 1872 (months in 114 years since 1901)
mytemp <- mytemp[,,601:1872] # restrict to years of interest

# create empty dataframes to hold the calculated climate variables for each grid cell. -----
# MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(MaxT) <- lat_t; colnames(MaxT) <- lon_t # NOT RUN CURRENTLY
Month01_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month01_MaxT) <- lat_t; colnames(Month01_MaxT) <- lon_t
Month02_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month02_MaxT) <- lat_t; colnames(Month02_MaxT) <- lon_t
Month03_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month03_MaxT) <- lat_t; colnames(Month03_MaxT) <- lon_t
Month04_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month04_MaxT) <- lat_t; colnames(Month04_MaxT) <- lon_t
Month05_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month05_MaxT) <- lat_t; colnames(Month05_MaxT) <- lon_t
Month06_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month06_MaxT) <- lat_t; colnames(Month06_MaxT) <- lon_t
Month07_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month07_MaxT) <- lat_t; colnames(Month07_MaxT) <- lon_t
Month08_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month08_MaxT) <- lat_t; colnames(Month08_MaxT) <- lon_t
Month09_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month09_MaxT) <- lat_t; colnames(Month09_MaxT) <- lon_t
Month10_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month10_MaxT) <- lat_t; colnames(Month10_MaxT) <- lon_t
Month11_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month11_MaxT) <- lat_t; colnames(Month11_MaxT) <- lon_t
Month12_MaxT <- as.data.frame(matrix(NA, 192,288)); row.names(Month12_MaxT) <- lat_t; colnames(Month12_MaxT) <- lon_t

# compute descriptive variables (POPULATE) -----
for ( i in 1:length(lon_t) ) {
  for (j in 1:length(lat_t) ) {
    
    # for each point in space generate a matrix in which cols are months and rows are years
    Data <- as.data.frame(matrix(mytemp[i,j,], length(mytemp[i,j,])/12, 12, byrow=T))
    colnames(Data) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    row.names(Data) <- (1:(length(mytemp[i,j,])/12)) + 1900
    
    # populate dataframes from above. 
    # MaxT[j,i] <- max(apply(Data, 1, max, na.rm = T), na.rm = T) # NOT RUN CURRENTLY
    Month01_MaxT[j,i] <- max(Data[,1], na.rm = T)
    Month02_MaxT[j,i] <- max(Data[,2], na.rm = T)
    Month03_MaxT[j,i] <- max(Data[,3], na.rm = T)
    Month04_MaxT[j,i] <- max(Data[,4], na.rm = T)
    Month05_MaxT[j,i] <- max(Data[,5], na.rm = T)
    Month06_MaxT[j,i] <- max(Data[,6], na.rm = T)
    Month07_MaxT[j,i] <- max(Data[,7], na.rm = T)
    Month08_MaxT[j,i] <- max(Data[,8], na.rm = T)
    Month09_MaxT[j,i] <- max(Data[,9], na.rm = T)
    Month10_MaxT[j,i] <- max(Data[,10], na.rm = T)
    Month11_MaxT[j,i] <- max(Data[,11], na.rm = T)
    Month12_MaxT[j,i] <- max(Data[,12], na.rm = T)
  } 
  print(paste(i*1.25, " degrees of lon traversed!"))
}

# Create rasters -------------------------------------------
setwd(wdclimate) 
pdf(file = "tas_maps_MaxT.pdf", width = 17, height = 11)
setwd(wdPAM); load(file = "LonLat_BirdPAM_raster.rdata"); setwd(wdclimate) 
for (i in c("Month01", "Month02", "Month03", "Month04", "Month05", "Month06", 
            "Month07", "Month08", "Month09", "Month10", "Month11", "Month12")) {
  temp <- raster(ncol=288, nrow=192, xmn=-180, xmx=180, ymn=-90, ymx=90)
  values(temp) <- as.vector(t(get(paste0(i, "_MaxT"))[seq(dim(get(paste0(i, "_MaxT")))[1],1,by = -1),]))
  extent(temp) <- extent(c(0, 360, -90, 90))
  temp <- rotate(temp)
  extent(temp) <- extent(c(-180, 180, -90, 90))
  temp <- projectRaster(temp, LonLat_BirdPAM_raster)
  plot(temp, main=paste0(i, "_MaxT"))
  assign(paste0(i, "_MaxT_raster"), temp)
}
dev.off()
rm(Month01_MaxT, Month02_MaxT, Month03_MaxT, Month04_MaxT, Month05_MaxT, Month06_MaxT, 
   Month07_MaxT, Month08_MaxT, Month09_MaxT, Month10_MaxT, Month11_MaxT, Month12_MaxT,
   Data, temp, mytemp, tempmax.nc, i, j, lat_t, lon_t) # tidy. 











# read ncdf file with raw data
setwd(wdclimate) # read ncdf file with raw data
tempmin.nc = nc_open("tasmin_Amon_CCSM4_historical_r1i1p1_185001-200512.nc")


# extract coordinate system and temp data for each 0.5x0.5 cell -----
lat_t = ncvar_get( nc = tempmin.nc, varid = "lat")          
lon_t = ncvar_get( nc = tempmin.nc, varid = "lon") 
mytemp = ncvar_get( nc = tempmin.nc, varid = "tasmin")  # 3d array with 288 (lon) x 192 (lat) x 1872 (months in 114 years since 1901)
mytemp <- mytemp[,,601:1872] # restrict to years of interest

# create empty dataframes to hold the calculated climate variables for each grid cell. -----
# MinT <- as.data.frame(matrix(NA, 192,288)); row.names(MinT) <- lat_t; colnames(MinT) <- lon_t # NOT RUN CURRENTLY
Month01_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month01_MinT) <- lat_t; colnames(Month01_MinT) <- lon_t
Month02_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month02_MinT) <- lat_t; colnames(Month02_MinT) <- lon_t
Month03_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month03_MinT) <- lat_t; colnames(Month03_MinT) <- lon_t
Month04_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month04_MinT) <- lat_t; colnames(Month04_MinT) <- lon_t
Month05_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month05_MinT) <- lat_t; colnames(Month05_MinT) <- lon_t
Month06_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month06_MinT) <- lat_t; colnames(Month06_MinT) <- lon_t
Month07_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month07_MinT) <- lat_t; colnames(Month07_MinT) <- lon_t
Month08_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month08_MinT) <- lat_t; colnames(Month08_MinT) <- lon_t
Month09_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month09_MinT) <- lat_t; colnames(Month09_MinT) <- lon_t
Month10_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month10_MinT) <- lat_t; colnames(Month10_MinT) <- lon_t
Month11_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month11_MinT) <- lat_t; colnames(Month11_MinT) <- lon_t
Month12_MinT <- as.data.frame(matrix(NA, 192,288)); row.names(Month12_MinT) <- lat_t; colnames(Month12_MinT) <- lon_t

# compute descriptive variables (POPULATE) -----
for ( i in 1:length(lon_t) ) {
  for (j in 1:length(lat_t) ) {
    
    # for each point in space generate a matrix in which cols are months and rows are years
    Data <- as.data.frame(matrix(mytemp[i,j,], length(mytemp[i,j,])/12, 12, byrow=T))
    colnames(Data) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    row.names(Data) <- (1:(length(mytemp[i,j,])/12)) + 1900
    
    # populate dataframes from above. 
    # MinT[j,i] <- min(apply(Data, 1, min, na.rm = T), na.rm = T) # NOT RUN CURRENTLY
    Month01_MinT[j,i] <- min(Data[,1], na.rm = T)
    Month02_MinT[j,i] <- min(Data[,2], na.rm = T)
    Month03_MinT[j,i] <- min(Data[,3], na.rm = T)
    Month04_MinT[j,i] <- min(Data[,4], na.rm = T)
    Month05_MinT[j,i] <- min(Data[,5], na.rm = T)
    Month06_MinT[j,i] <- min(Data[,6], na.rm = T)
    Month07_MinT[j,i] <- min(Data[,7], na.rm = T)
    Month08_MinT[j,i] <- min(Data[,8], na.rm = T)
    Month09_MinT[j,i] <- min(Data[,9], na.rm = T)
    Month10_MinT[j,i] <- min(Data[,10], na.rm = T)
    Month11_MinT[j,i] <- min(Data[,11], na.rm = T)
    Month12_MinT[j,i] <- min(Data[,12], na.rm = T)
  } 
  print(paste(i*1.25, " degrees of lon traversed!"))
}

# Create rasters -------------------------------------------
setwd(wdclimate) 
pdf(file = "tas_maps_MinT.pdf", width = 17, height = 11)
setwd(wdPAM); load(file = "LonLat_BirdPAM_raster.rdata"); setwd(wdclimate) 
for (i in c("Month01", "Month02", "Month03", "Month04", "Month05", "Month06", 
            "Month07", "Month08", "Month09", "Month10", "Month11", "Month12")) {
  temp <- raster(ncol=288, nrow=192, xmn=-180, xmx=180, ymn=-90, ymx=90)
  values(temp) <- as.vector(t(get(paste0(i, "_MinT"))[seq(dim(get(paste0(i, "_MinT")))[1],1,by = -1),]))
  extent(temp) <- extent(c(0, 360, -90, 90))
  temp <- rotate(temp)
  extent(temp) <- extent(c(-180, 180, -90, 90))
  temp <- projectRaster(temp, LonLat_BirdPAM_raster)
  plot(temp, main=paste0(i, "_MinT"))
  assign(paste0(i, "_MinT_raster"), temp)
}
dev.off()
rm(Month01_MinT, Month02_MinT, Month03_MinT, Month04_MinT, Month05_MinT, Month06_MinT, 
   Month07_MinT, Month08_MinT, Month09_MinT, Month10_MinT, Month11_MinT, Month12_MinT,
   Data, temp, mytemp, tempmin.nc, i, j, lat_t, lon_t) # tidy. 





# Range
pdf(file = "tas_maps_RngT.pdf", width = 17, height = 11)
Month01_RngT_raster <- Month01_MaxT_raster - Month01_MinT_raster; plot(Month01_RngT_raster, main = "Month01_RngT")
Month02_RngT_raster <- Month02_MaxT_raster - Month02_MinT_raster; plot(Month02_RngT_raster, main = "Month02_RngT")
Month03_RngT_raster <- Month03_MaxT_raster - Month03_MinT_raster; plot(Month03_RngT_raster, main = "Month03_RngT")
Month04_RngT_raster <- Month04_MaxT_raster - Month04_MinT_raster; plot(Month04_RngT_raster, main = "Month04_RngT")
Month05_RngT_raster <- Month05_MaxT_raster - Month05_MinT_raster; plot(Month05_RngT_raster, main = "Month05_RngT")
Month06_RngT_raster <- Month06_MaxT_raster - Month06_MinT_raster; plot(Month06_RngT_raster, main = "Month06_RngT")
Month07_RngT_raster <- Month07_MaxT_raster - Month07_MinT_raster; plot(Month07_RngT_raster, main = "Month07_RngT")
Month08_RngT_raster <- Month08_MaxT_raster - Month08_MinT_raster; plot(Month08_RngT_raster, main = "Month08_RngT")
Month09_RngT_raster <- Month09_MaxT_raster - Month09_MinT_raster; plot(Month09_RngT_raster, main = "Month09_RngT")
Month10_RngT_raster <- Month10_MaxT_raster - Month10_MinT_raster; plot(Month10_RngT_raster, main = "Month10_RngT")
Month11_RngT_raster <- Month11_MaxT_raster - Month11_MinT_raster; plot(Month11_RngT_raster, main = "Month11_RngT")
Month12_RngT_raster <- Month12_MaxT_raster - Month12_MinT_raster; plot(Month12_RngT_raster, main = "Month12_RngT")
dev.off()

# setwd("~/Box Sync/JMPH_2020/data/CCSM4")
# save(Month01_MeanT_raster, Month02_MeanT_raster, Month03_MeanT_raster, Month04_MeanT_raster, Month05_MeanT_raster, Month06_MeanT_raster,
#      Month07_MeanT_raster, Month08_MeanT_raster, Month09_MeanT_raster, Month10_MeanT_raster, Month11_MeanT_raster, Month12_MeanT_raster,
#      file = "tas_rasters_indiv_VJF.rdata")
# save(Month01_MinT_raster, Month02_MinT_raster, Month03_MinT_raster, Month04_MinT_raster, Month05_MinT_raster, Month06_MinT_raster,
#      Month07_MinT_raster, Month08_MinT_raster, Month09_MinT_raster, Month10_MinT_raster, Month11_MinT_raster, Month12_MinT_raster,
#      file = "tasmin_rasters_indiv_VJF.rdata")
# save(Month01_MaxT_raster, Month02_MaxT_raster, Month03_MaxT_raster, Month04_MaxT_raster, Month05_MaxT_raster, Month06_MaxT_raster,
#      Month07_MaxT_raster, Month08_MaxT_raster, Month09_MaxT_raster, Month10_MaxT_raster, Month11_MaxT_raster, Month12_MaxT_raster,
#      file = "tasmax_rasters_indiv_VJF.rdata")
# save(Month01_RngT_raster, Month02_RngT_raster, Month03_RngT_raster, Month04_RngT_raster, Month05_RngT_raster, Month06_RngT_raster,
#      Month07_RngT_raster, Month08_RngT_raster, Month09_RngT_raster, Month10_RngT_raster, Month11_RngT_raster, Month12_RngT_raster,
#      file = "tasrng_rasters_indiv_VJF.rdata")



tas <- stack(x = c(Month01_MeanT_raster, Month02_MeanT_raster, Month03_MeanT_raster, Month04_MeanT_raster, Month05_MeanT_raster, Month06_MeanT_raster,
                      Month07_MeanT_raster, Month08_MeanT_raster, Month09_MeanT_raster, Month10_MeanT_raster, Month11_MeanT_raster, Month12_MeanT_raster))
rm(Month01_MeanT_raster, Month02_MeanT_raster, Month03_MeanT_raster, Month04_MeanT_raster, Month05_MeanT_raster, Month06_MeanT_raster,
   Month07_MeanT_raster, Month08_MeanT_raster, Month09_MeanT_raster, Month10_MeanT_raster, Month11_MeanT_raster, Month12_MeanT_raster)


tasmin <- stack(x = c(Month01_MinT_raster, Month02_MinT_raster, Month03_MinT_raster, Month04_MinT_raster, Month05_MinT_raster, Month06_MinT_raster,
                      Month07_MinT_raster, Month08_MinT_raster, Month09_MinT_raster, Month10_MinT_raster, Month11_MinT_raster, Month12_MinT_raster))
rm(Month01_MinT_raster, Month02_MinT_raster, Month03_MinT_raster, Month04_MinT_raster, Month05_MinT_raster, Month06_MinT_raster,
   Month07_MinT_raster, Month08_MinT_raster, Month09_MinT_raster, Month10_MinT_raster, Month11_MinT_raster, Month12_MinT_raster)


tasmax <- stack(x = c(Month01_MaxT_raster, Month02_MaxT_raster, Month03_MaxT_raster, Month04_MaxT_raster, Month05_MaxT_raster, Month06_MaxT_raster,
                      Month07_MaxT_raster, Month08_MaxT_raster, Month09_MaxT_raster, Month10_MaxT_raster, Month11_MaxT_raster, Month12_MaxT_raster))
rm(Month01_MaxT_raster, Month02_MaxT_raster, Month03_MaxT_raster, Month04_MaxT_raster, Month05_MaxT_raster, Month06_MaxT_raster,
   Month07_MaxT_raster, Month08_MaxT_raster, Month09_MaxT_raster, Month10_MaxT_raster, Month11_MaxT_raster, Month12_MaxT_raster)


tasrng <- stack(x = c(Month01_RngT_raster, Month02_RngT_raster, Month03_RngT_raster, Month04_RngT_raster, Month05_RngT_raster, Month06_RngT_raster,
                      Month07_RngT_raster, Month08_RngT_raster, Month09_RngT_raster, Month10_RngT_raster, Month11_RngT_raster, Month12_RngT_raster))
rm(Month01_RngT_raster, Month02_RngT_raster, Month03_RngT_raster, Month04_RngT_raster, Month05_RngT_raster, Month06_RngT_raster,
   Month07_RngT_raster, Month08_RngT_raster, Month09_RngT_raster, Month10_RngT_raster, Month11_RngT_raster, Month12_RngT_raster)


names(tasmax) <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12")
names(tasmin) <- paste0("tasmin_", names(tasmax))
names(tasrng) <- paste0("tasrng_", names(tasmax))
names(tasmax) <- paste0("tasmax_", names(tasmax))


setwd(wdclimate) 
save(tas,
     file = "tas_rasters_VJF.rdata")
save(tasmin,
     file = "tasmin_rasters_VJF.rdata")
save(tasmax,
     file = "tasmax_rasters_VJF.rdata")
save(tasrng,
     file = "tasrng_rasters_VJF.rdata")






# create MAT raster ----------------------------------------
setwd("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/Climate/Data") 
load(file = "tas_rasters_VJF.rdata")
MAT <- mean(tas, na.rm = T)
plot(MAT)
dim(tas[[1]]) == dim(MAT) # looks good. 
save(MAT, file = "MAT_raster_VJF.rdata")
# ---------------------------------------------------------

# create VarT raster ----------------------------------------
temp <- brick(tas)
VarT <- calc(temp,var)
plot(VarT)
dim(tas[[1]]) == dim(VarT) # looks good. 
save(VarT, file = "VarT_raster_VJF.rdata")
# ---------------------------------------------------------

# create temp range raster ----------------------------------------
temp <- brick(tas)
MATrt <- calc(temp,max)
MATrb <- calc(temp,min)
MATr <- MATrt - MATrb
plot(MATr)
dim(tas[[1]]) == dim(MATr) # looks good. 
save(MATr, file = "MATr_raster_VJF.rdata")
# ---------------------------------------------------------


