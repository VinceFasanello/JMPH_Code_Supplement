rm(list = ls())
wdPAM <- "~/JMPH/PREP/PAM/Data"
wdTopo <- "~/JMPH/PREP/Topo/Data"

setwd(wdPAM)
load(file = "LonLat_BirdPAM_raster.rdata")

setwd(wdTopo)
load(file = "TopoRasters.Rdata")

Elev_raster <- projectRaster(from = Elev_raster, to = LonLat_BirdPAM_raster)
values(Elev_raster)[is.na(values(Elev_raster) )] <- 0
plot(Elev_raster)
save(Elev_raster, file = "Elev_raster.rdata")


Aspect_raster <- projectRaster(from = Aspect_raster, to = LonLat_BirdPAM_raster)
plot(Aspect_raster)
save(Aspect_raster, file = "Aspect_raster.rdata")


Slope_raster <- projectRaster(from = Slope_raster, to = LonLat_BirdPAM_raster)
plot(Slope_raster)
save(Slope_raster, file = "Slope_raster.rdata")
