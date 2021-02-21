rm(list=ls())
require(ncdf4)
require(raster)
wdPAM <- "/Users/boterolab1/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/PAM/Data"
wdclimate <- "/Users/boterolab1/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/Climate/Data"

# MEAN TEMPERATURE -------------------------------------------------------------
setwd(wdclimate) # read ncdf file with raw data
precip.nc = nc_open("pr_Amon_CCSM4_historical_r1i1p1_185001-200512.nc"); # print(precip.nc)

# extract coordinate system and temp data for each 0.5x0.5 cell -----
lat_t = ncvar_get( nc = precip.nc, varid = "lat")          
lon_t = ncvar_get( nc = precip.nc, varid = "lon") 
myprecip = ncvar_get( nc = precip.nc, varid = "pr")  # 3d array with 288 (lon) x 192 (lat) x 1872 (months in 114 years since 1901)
myprecip <- myprecip[,,601:1872] # restrict to years of interest

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
    
    P01_prmean[j,i] <- mean(unlist(Data[,c(1)]), na.rm = T)
    P02_prmean[j,i] <- mean(unlist(Data[,c(2)]), na.rm = T)
    P03_prmean[j,i] <- mean(unlist(Data[,c(3)]), na.rm = T)
    P04_prmean[j,i] <- mean(unlist(Data[,c(4)]), na.rm = T)
    P05_prmean[j,i] <- mean(unlist(Data[,c(5)]), na.rm = T)
    P06_prmean[j,i] <- mean(unlist(Data[,c(6)]), na.rm = T)
    P07_prmean[j,i] <- mean(unlist(Data[,c(7)]), na.rm = T)
    P08_prmean[j,i] <- mean(unlist(Data[,c(8)]), na.rm = T)
    P09_prmean[j,i] <- mean(unlist(Data[,c(9)]), na.rm = T)
    P10_prmean[j,i] <- mean(unlist(Data[,c(10)]), na.rm = T)
    P11_prmean[j,i] <- mean(unlist(Data[,c(11)]), na.rm = T)
    P12_prmean[j,i] <- mean(unlist(Data[,c(12)]), na.rm = T)
  } 
  print(paste(i*1.25, " degrees of lon traversed!"))
}


# Create rasters -------------------------------------------

setwd(wdPAM); load(file = "LonLat_BirdPAM_raster.rdata"); setwd(wdclimate) 
for (i in c("P01", "P02", "P03", "P04", "P05", "P06", 
            "P07", "P08", "P09", "P10", "P11", "P12")) {
  temp <- raster(ncol=288, nrow=192, xmn=-180, xmx=180, ymn=-90, ymx=90)
  values(temp) <- as.vector(t(get(paste0(i, "_prmean"))[seq(dim(get(paste0(i, "_prmean")))[1],1,by = -1),]))
  extent(temp) <- extent(c(0, 360, -90, 90))
  temp <- rotate(temp)
  extent(temp) <- extent(c(-180, 180, -90, 90))
  temp <- projectRaster(temp, crs = crs(LonLat_BirdPAM_raster), res = res(LonLat_BirdPAM_raster), method = "bilinear")
  plot(temp, main=paste0(i, "_prmean"))
  assign(paste0(i, "_prmean_raster"), temp)
}
res(temp);res(LonLat_BirdPAM_raster)
crs(temp);crs(LonLat_BirdPAM_raster)


rm(P01_prmean, P02_prmean, P03_prmean, P04_prmean, P05_prmean, P06_prmean,
   P07_prmean, P08_prmean, P09_prmean, P10_prmean, P11_prmean, P12_prmean,
   Data, temp, myprecip, precip.nc, i, j, lat_t, lon_t) # tidy.




pcp <- stack(x = c(P01_prmean_raster, P02_prmean_raster, P03_prmean_raster, P04_prmean_raster, P05_prmean_raster, P06_prmean_raster,
                   P07_prmean_raster, P08_prmean_raster, P09_prmean_raster, P10_prmean_raster, P11_prmean_raster, P12_prmean_raster))
rm(P01_prmean_raster, P02_prmean_raster, P03_prmean_raster, P04_prmean_raster, P05_prmean_raster, P06_prmean_raster,
   P07_prmean_raster, P08_prmean_raster, P09_prmean_raster, P10_prmean_raster, P11_prmean_raster, P12_prmean_raster)


setwd(wdclimate)
save(pcp,
     file = "pcp_rasters_VJF_BASIC.rdata")


# create PCP raster ----------------------------------------
setwd("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PREP/Climate/Data") 
load(file = "pcp_rasters_VJF_BASIC.rdata")
PCP <- calc(pcp, fun=mean, na.rm = T)
plot(PCP)
dim(pcp[[1]]) == dim(PCP) # looks good. 
save(PCP, file = "PCP_raster_VJF_BASIC.rdata")
# ---------------------------------------------------------

# create temp range raster ----------------------------------------
temp <- brick(pcp)
PCPrt <- calc(temp,max)
PCPrb <- calc(temp,min)
PCPr <- PCPrt - PCPrb
plot(PCPr)
dim(pcp[[1]]) == dim(PCPr) # looks good. 
save(PCPr, file = "PCPr_raster_VJF_BASIC.rdata")
# ---------------------------------------------------------


# create temp range raster ----------------------------------------
temp <- brick(pcp)
VarP <- calc(temp,var, na.rm = T)
plot(VarP)
dim(pcp[[1]]) == dim(VarP) # looks good. 
save(VarP, file = "VarP_raster_VJF_BASIC.rdata")
# ---------------------------------------------------------
