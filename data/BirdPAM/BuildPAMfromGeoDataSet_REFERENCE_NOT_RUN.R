# BuildPAMfromGeoDataSet.R
#
# Converts a series of species ranges (read as a single multi-layered shapefile) into a 
# Presence Absence Matrix (PAM)
# 
# Carlos Botero, Feb 15 2019 (c)

####################################################################################
# define usefule functions first

BuildPAMfromGeoDataSet <- function(fc, specieslabel, presencelabel, seasonallabel, PAMfname, myEnvRasters, is.bird = F) {
  ALLSPP <- as.character(unique(eval(parse(text=paste0('fc$',specieslabel)))))
  
  options(warn=-1)
  for (i in 1:length(ALLSPP)) {
    # read shapefile
    if (!is.bird) {
      mysp <- fc[which(eval(parse(text=paste0('fc$',specieslabel))) %in% ALLSPP[i]),]
    }
    else {
      # convert into spatialpolygon (and resolve problems with 
      # multisurface geometries)
      mysp <- as_Spatial(st_make_valid(fc[which(fc$SCINAME %in% ALLSPP[i]),]))
    }
    
    if (!is.na(presencelabel)) { 
      myeval <- sum(eval(parse(text=paste0('mysp$',presencelabel))) <= 3) & 
        sum(eval(parse(text=paste0('mysp$',seasonallabel))) <= 2)
    }
    else { myeval <- T }
    
    # use a resolution of 0.5 degrees at the equator (to match our climate data)
    # set boundaries to the lats/lons for which we actually have environmental data
    if (myeval) {
      if (!is.na(presencelabel))  {
        thisPAM <- lets.presab(mysp, crs.grid = CRS('+proj=wag4 +lon_0=0'), 
                               xmn = -17507938, xmx = 17628062,
                               ymn = -8961362, ymx = 8969638,
                               resol = 6.45e4, remove.cells = F,
                               presence = c(1,2,3), seasonal = c(1, 2), count = F, remove.sp = F)
      }
      else {
        thisPAM <- lets.presab(mysp, crs.grid = CRS('+proj=wag4 +lon_0=0'), 
                               xmn = -17507938, xmx = 17628062,
                               ymn = -8961362, ymx = 8969638,
                               resol = 6.45e4, remove.cells = F, count = F, remove.sp = F)
      }
      
      if ( i==1 ) {
        myPAM <- thisPAM
      }
      else {
        myPAM$Presence_and_Absence_Matrix <- cbind(myPAM$Presence_and_Absence_Matrix,
                                                   thisPAM$Presence_and_Absence_Matrix[,3])
        colnames(myPAM$Presence_and_Absence_Matrix)[dim(myPAM$Presence_and_Absence_Matrix)[2]] <- as.character(colnames(thisPAM$Presence_and_Absence_Matrix)[3])
        values(myPAM$Richness_Raster) <- values(myPAM$Richness_Raster) + values(thisPAM$Richness_Raster)
        myPAM$Species_name <- c(myPAM$Species_name, thisPAM$Species_name)
      }
    }
    if(i%%100 == 0) { 
      print(paste(i, "out of", length(ALLSPP))) 
      save(myPAM, file = PAMfname)
    }
  }
  
  # eliminate all coordinates for which there are no species in this group
  mydim <- dim(myPAM$Presence_and_Absence_Matrix)
  keepThese <- which(rowSums(myPAM$Presence_and_Absence_Matrix[,3:mydim[2]]) >=1)
  myPAM$Presence_and_Absence_Matrix <- myPAM$Presence_and_Absence_Matrix[keepThese,]
  
  # add reference niche space for ALL species in this group
  myPAM$Presence_and_Absence_Matrix <- getBands(myPAM$Presence_and_Absence_Matrix, c(-8647138,8647138), "AllSPPinClade")
  
  # add environmental data
  myPAM$Presence_and_Absence_Matrix <- lets.addvar(myPAM, myEnvRasters)
  
  save(myPAM, file = PAMfname)
  options(warn=0)
}

getBands <- function(mydata, mylats, mylabel) {
  curnames <- colnames(mydata)
  for ( i in 1:(length(mylats)-1) ) {
    mydata <- cbind(mydata, as.numeric(mydata[,'Latitude(y)'] >= mylats[i] & mydata[,'Latitude(y)'] < mylats[i+1]))
  }
  colnames(mydata) <- c(curnames, paste0(mylabel,'_B', 1:(length(mylats)-1)))
  return(mydata)
}
#############################################################################################################################
# set things up...
require(rgdal)
require(letsR)
require(sp)
require(sf)
require(lwgeom)
require(cleangeo)
require(letsR)
require(raster)

load("/Users/carlos/Box/Botero Lab Shared Files v. 2/Climate Data/BOTEROLAB.06Jan2019.ClimateRasters.1850.2005.CCSM4.WaterAndLand.RData")
load("/Users/carlos/Box/Botero Lab Shared Files v. 2/Topographic rasters/TopoRasters_LandAndWater.Rdata")

Sine_Aspect_raster <- sin(Aspect_raster)
Cos_Aspect_raster <- cos(Aspect_raster)

myEnvRasters <- stack(list(MeanT_raster, VarT_raster, Pt_raster,  
                           MeanP_raster, CvP_raster, Pp_raster, 
                           Slope_raster, MeanElevation_raster, 
                           Sine_Aspect_raster, Cos_Aspect_raster))

# reproject to equal area Wagner IV
myEnvRasters <-  projectRaster(myEnvRasters, crs = CRS('+proj=wag4 +lon_0=0'))

names(myEnvRasters) <- c("MeanT_raster", "VarT_raster", "Pt_raster",  
                         "MeanP_raster", "CvP_raster", "Pp_raster", 
                         "Slope", "MeanElevation", "Sine_Aspect", "Cos_Aspect")

#############################################################################
# now start computing PAMS

# Birds
dsn = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Original geodata by Class/Bird Ranges/BirdLife ORIGINAL data files 13 Feb 2019/BOTW/BOTW.gdb'
layer = 'All_Species'
fc <- st_read(dsn = dsn, layer = layer)

specieslabel = 'SCINAME'
PAMfname = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/BirdPAM.Rdata'
presencelabel = 'PRESENCE'
seasonallabel = 'SEASONAL'

BuildPAMfromGeoDataSet(fc, specieslabel, presencelabel, seasonallabel, PAMfname, myEnvRasters, is.bird = T)

####################
# Amphibians
dsn = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Original geodata by Class/Amphibian Ranges/AMPHIBIANS'
layer = 'AMPHIBIANS'
fc <- readOGR(dsn = dsn, layer = layer)
specieslabel = 'binomial'
PAMfname = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/AmphibianPAM.Rdata'
presencelabel = 'presence'
seasonallabel = 'seasonal'

BuildPAMfromGeoDataSet(fc, specieslabel, presencelabel, seasonallabel, PAMfname, myEnvRasters)

####################
# Terrestrial mammals
dsn = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Original geodata by Class/Terrestrial_mammal Ranges/TERRESTRIAL_MAMMALS'
layer = 'TERRESTRIAL_MAMMALS'
fc <- readOGR(dsn = dsn, layer = layer)
specieslabel = 'binomial'
PAMfname = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/TerrMammalPAM.Rdata'
presencelabel = 'presence'
seasonallabel = 'seasonal'

BuildPAMfromGeoDataSet(fc, specieslabel, presencelabel, seasonallabel, PAMfname, myEnvRasters)

####################
# Reptiles
dsn = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Original geodata by Class/Reptilian Ranges/GARD1.1_dissolved_ranges/'
layer = 'modeled_reptiles'
fc <- readOGR(dsn = dsn, layer = layer)
specieslabel = 'Binomial'
PAMfname = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/ReptilianPAM.Rdata'
presencelabel = NA
seasonallabel = NA

BuildPAMfromGeoDataSet(fc, specieslabel, presencelabel, seasonallabel, PAMfname, myEnvRasters)

#############################################################################################################################
# Create PAM for latitudinal bands in the niche space analysis
load('/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/TerrMammalPAM.Rdata')

# clean up
myPAM$Species_name <- NULL
myPAM$Presence_and_Absence_Matrix <- myPAM$Presence_and_Absence_Matrix[,1:2]

# now divide latitudinal range into bands of different width
myPoints <- SpatialPoints(coords = cbind(rep(0,length(seq(-90,90,by=5))), seq(-90,90,by=5)), proj4string=CRS("+proj=longlat"))
myPoints <- spTransform(myPoints, '+proj=wag4 +lon_0=0')                         
mylats <- coordinates(myPoints)[,2]

myPAM$Presence_and_Absence_Matrix <- getBands(myPAM$Presence_and_Absence_Matrix, mylats, "FiveDegBands")
myPAM$Presence_and_Absence_Matrix <- getBands(myPAM$Presence_and_Absence_Matrix, mylats[seq(1,length(mylats),by=2)], "TenDegBands")
myPAM$Presence_and_Absence_Matrix <- getBands(myPAM$Presence_and_Absence_Matrix, mylats[seq(1,length(mylats),by=3)], "FifteenDegBands")
myPAM$Presence_and_Absence_Matrix <- getBands(myPAM$Presence_and_Absence_Matrix, mylats[seq(1,length(mylats),by=4)], "TwentyDegBands")
myPAM$Presence_and_Absence_Matrix <- getBands(myPAM$Presence_and_Absence_Matrix, mylats[c(1,length(mylats))], "AllWorld")

save(myPAM, file = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/LatBandsPAM.Rdata')

####################################################
####################################################
####################################################


# Now add environmental data to those PAMs

# stack rasters to simplify coding
load('/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/AmphibianPAM.Rdata')
myPAM$Presence_and_Absence_Matrix <- lets.addvar(myPAM, myEnvRasters)
save(myPAM, file = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/AmphibianPAM.Rdata')

load('/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/TerrMammalPAM.Rdata')
myPAM$Presence_and_Absence_Matrix <- lets.addvar(myPAM, myEnvRasters)
save(myPAM, file = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/TerrMammalPAM.Rdata')

load('/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/ReptilianPAM.Rdata')
myPAM$Presence_and_Absence_Matrix <- lets.addvar(myPAM, myEnvRasters)
save(myPAM, file = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/ReptilianPAM.Rdata')

load('/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/BirdPAM.Rdata')
myPAM$Presence_and_Absence_Matrix <- lets.addvar(myPAM, myEnvRasters)
save(myPAM, file = '/Users/carlos/Box/Botero Lab Shared Files v. 2/Vertebrate distributional ranges/Presence Absence Matrices/BirdPAM.Rdata')
