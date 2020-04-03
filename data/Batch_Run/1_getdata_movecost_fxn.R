# dtm=raster_cost_longlat ; origin=coordsO ;  destin=coordsD
# funct = "t" ;  time = "h"; outp = "r"; sl.crit = 10; W = 70; L = 0; N = 1 ;
# V = 1.2; moves = 16; breaks = NULL; cont.lab = TRUE; destin.lab = TRUE
# cex.breaks = 0.6; cex.lcp.lab = 0.6; oneplot = TRUE; export = FALSE

movecostAngela<-function (dtm, slope = NULL, origin, destin = NULL, funct = "t", 
                          time = "h", outp = "r", sl.crit = 10, W = 70, L = 0, N = 1, 
                          V = 1.2, moves = 16, breaks = NULL, cont.lab = TRUE, destin.lab = TRUE, 
                          cex.breaks = 0.6, cex.lcp.lab = 0.6, oneplot = TRUE, export = FALSE) 
{
  if (is.null(slope) == TRUE) {
    slope <- raster::terrain(dtm, opt = "slope", unit = "degrees", 
                             neighbors = 8)
  }
  else {
    slope <- slope
  }
  
  if (funct == "t") {
    cost_function <- function(x) {
      6 * exp(-3.5 * abs(tan(x * pi/180) + 0.05))
    }
    main.title <- paste0("Walking-time isochrones (in ", 
                         time, ") around origin")
    sub.title <- "Walking-time based on the Tobler's on-path hiking function"
    legend.cost <- paste0("walking-time (", time, ")")
    sub.title.lcp.plot <- paste0("LCP(s) and walking-time distance(s) based on the Tobler's on-path hiking function (time in ", 
                                 time, ") \nblack dot=start location\n red dot(s)=destination location(s)")
  }
  
  
  
  
  if (funct == "t" | funct == "tofp" | funct == "mt" | funct == 
      "ic" | funct == "icofp") {
    speed.kmh <- raster::calc(slope, cost_function)
    speed.ms <- speed.kmh * 0.278
    cost.TRANS <- gdistance::transition(speed.ms, transitionFunction = mean, 
                                        directions = moves)
  }
  
  
  Conductance <- gdistance::geoCorrection(cost.TRANS)
  accum_final <- gdistance::accCost(Conductance, sp::coordinates(origin))
  if (funct == "t" | funct == "tofp" | funct == "mt" | funct == 
      "ic" | funct == "ug") {
    if (time == "h") {
      accum_final <- accum_final/3600
    }
    else {
      accum_final <- accum_final/60
    }
  }
  

  if (is.null(breaks) == TRUE) {
    breaks <- round((max(accum_final[][is.finite(accum_final[])]) - 
                       min(accum_final[][is.finite(accum_final[])]))/10, 
                    2)
  }
  levels <- seq(min(accum_final[][is.finite(accum_final[])]), 
                max(accum_final[][is.finite(accum_final[])]), breaks)
  if (is.null(destin) == FALSE & oneplot == TRUE) {
    m <- rbind(c(1, 2))
    layout(m)
  }
  # if (outp == "r") {
  #   raster::plot(accum_final, main = main.title, sub = sub.title, 
  #                cex.main = 0.95, cex.sub = 0.75, legend.lab = legend.cost, 
  #                col = terrain.colors(255))
  #   raster::contour(accum_final, add = TRUE, levels = levels, 
  #                   labcex = cex.breaks, drawlabels = cont.lab)
  #   raster::plot(origin, pch = 20, add = TRUE)
  # }
  # else {
  #   raster::contour(accum_final, levels = levels, main = main.title, 
  #                   sub = sub.title, cex.main = 0.95, cex.sub = 0.75, 
  #                   labcex = cex.breaks, drawlabels = cont.lab)
  #   raster::plot(origin, pch = 20, add = TRUE)
  # }
  
  
  if(class(tryCatch (isolines <- raster::rasterToContour(accum_final, levels = levels), error=function(e) 1))%in%"numeric")
  {results<-"Infvals"}
  if(!class(tryCatch (isolines <- raster::rasterToContour(accum_final, levels = levels), error=function(e) 1))%in%"numeric")
  {isolines <- raster::rasterToContour(accum_final, levels = levels)
  
  
  if (is.null(destin) == FALSE) {
    
    # if(class(tryCatch ( sPath <- gdistance::shortestPath(Conductance, sp::coordinates(origin), sp::coordinates(destin), output = "SpatialLines"), warning=function(w) 1))%in%"numeric")
    # {results<-"sPath_extentpoint"}
    # crashes
    
    # costDistance(Conductance, origin, destin)
    # result$LCPs$length
     
    sPath <- gdistance::shortestPath(Conductance, sp::coordinates(origin), 
                                     sp::coordinates(destin), output = "SpatialLines")
    if(identical(sPath@bbox[,1],sPath@bbox[,2]) ) {results<-"sPath_extentpoint"}
    if(!identical(sPath@bbox[,1],sPath@bbox[,2]) ) 
        # raster::plot(dtm, main = "Digital Terrain Model with Least-cost Path(s)", 
        #              sub = sub.title.lcp.plot, cex.main = 0.9, cex.sub = 0.7, 
        #              legend.lab = "Elevation (masl)")
        # raster::plot(origin, add = TRUE, pch = 20)
        # raster::plot(destin, add = TRUE, pch = 20, col = "red")
        # graphics::lines(sPath)
        
        # if(class(tryCatch (sPath$length <- rgeos::gLength(sPath, byid = TRUE), error=function(e) 1, warning=function(w) NULL))%in%"numeric")
        #     {results<-"sPath_extentpoint"}
        # if(!class(tryCatch (sPath$length <- rgeos::gLength(sPath, byid = TRUE), error=function(e) 1, warning=function(w) NULL))%in%"numeric")
    {
       sPath$length <- rgeos::gLength(sPath, byid = TRUE)
      
      destin$cost <- raster::extract(accum_final, destin)
      # if (destin.lab == TRUE) {
      #   raster::text(sp::coordinates(destin), labels = round(destin$cost, 
      #                                                        2), pos = 4, cex = cex.lcp.lab)
      #   if (export == TRUE) {
      #     rgdal::writeOGR(sPath, ".", paste0("LCPs_", funct), 
      #                     driver = "ESRI Shapefile")
      #   }
      # }
  }}
  
    else {
      sPath = NULL
      dest.loc.w.cost = NULL
    }
  
    if (export == TRUE) {
      raster::writeRaster(accum_final, paste0("accum_cost_surf_", 
                                              funct), format = "GTiff")
      rgdal::writeOGR(isolines, ".", paste0("isolines_", funct), 
                      driver = "ESRI Shapefile")
    }
    if (is.null(destin) == FALSE & oneplot == TRUE) {
      par(mfrow = c(1, 1))
    }
  if(!identical(sPath@bbox[,1],sPath@bbox[,2]) )     
   { results <- list(accumulated.cost.raster = accum_final, isolines = isolines, 
                    LCPs = sPath, dest.loc.w.cost = destin)} }
  return(results)
}


#dtm=raster_costO
#origin=coordsO
#destin=coordsD

movecostAngela_vOCTDontAdapt<-function(dtm,origin, destin)
{# v2
  r<-dtm
  values(r)[values(r)%in%0]<-1e-12 # account for 0
  fxn1D <- function(x){1/x[2]} # don t adapt
  
  T <- transition(r, fxn1D, 8, symm=FALSE)
  T <- geoCorrection(T)
  sPath2 <- shortestPath(T, origin, destin, output="SpatialLines") 
  
  if(identical(sPath2@bbox[,1],sPath2@bbox[,2]) ) {results<-"sPath_extentpoint"}
  if(!identical(sPath2@bbox[,1],sPath2@bbox[,2]) ) { sPath2$length <- rgeos::gLength(sPath2, byid = TRUE)
  mcost<-costDistance(T, origin, destin) 
  
  #plot(r) ; lines(sPath2)
  #title(main=paste("don t adapt",round(costDistance(T, origin, destin),digits=2), ";",round(rgeos::gLength(sPath2, byid = TRUE), digits=2)) )
  
  results<-list(sPath=sPath2,mcost=mcost)}
  return(results)  
  
}


movecostAngela_vOCTDontAdapt_multdestin<-function(dtm,origin, destin, geocorrect="yes")
{# v2
  r<-dtm
  values(r)[values(r)%in%0]<-1e-12 # account for 0
  fxn1D <- function(x){1/x[2]} # don t adapt
  
  T <- transition(r, fxn1D, 8, symm=FALSE)
  if(geocorrect == "yes"){T <- geoCorrection(T)}
  sPath2 <- shortestPath(T, origin, destin, output="SpatialLines") 
  
  spe<-numeric()
  plen<-numeric()
  for(j in 1: length(sPath2))
    {if (identical(sPath2[j]@bbox[,1],sPath2[j]@bbox[,2])) 
       {spe<-c(spe,j)
        plen[j]<-"sPath_extentpoint"}
    if (!identical(sPath2[j]@bbox[,1],sPath2[j]@bbox[,2])) 
       {plen[j]<-rgeos::gLength(sPath2[j], byid = TRUE)}
      } # from j
  
  mcost<-costDistance(T, origin, destin) # for spath 0 - cost is 0
  spe<-unlist(spe) ; mcost[spe]<-"sPath_extentpoint"
  
  #plot(r) ; lines(sPath2)
  #title(main=paste("don t adapt",round(costDistance(T, origin, destin),digits=2), ";",round(rgeos::gLength(sPath2, byid = TRUE), digits=2)) )
  
  results<-list(sPath=sPath2,mcost=mcost, plen=plen)
  return(results)  
  
}