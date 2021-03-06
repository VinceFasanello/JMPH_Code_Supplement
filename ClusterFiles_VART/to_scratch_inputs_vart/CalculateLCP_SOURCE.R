CalculateLCP <- function(dtm,origin, destin, geocorrect="yes") {
  r<-dtm
  
  if(sum(is.na(values(r))) == length(values(r))){
    results<-list(sPath=NA,mcost=NA, plen=NA)
    return(results)
  } else {
    values(r)[values(r)%in%0]<-1e-12 # account for 0
    fxn1D <- function(x){1/x[2]} # don t adapt
    
    T <- transition(r, fxn1D, 8, symm=FALSE)
    if(geocorrect == "yes"){T <- geoCorrection(T)}
    sPath2 <- shortestPath(T, origin, destin, output="SpatialLines") 
    
    spe<-numeric(); plen<-numeric()
    for(j in 1: length(sPath2))
    {if (identical(sPath2[j]@bbox[,1],sPath2[j]@bbox[,2])) 
    {spe<-c(spe,j)
    plen[j]<-"sPath_extentpoint"}
      if (!identical(sPath2[j]@bbox[,1],sPath2[j]@bbox[,2])) 
      {plen[j]<-rgeos::gLength(sPath2[j], byid = TRUE)}
    } # from j
    
    mcost<-costDistance(T, origin, destin) # for spath 0 - cost is 0
    spe<-unlist(spe) ; mcost[spe]<-"sPath_extentpoint"
    
    results<-list(sPath=sPath2,mcost=mcost, plen=plen)
    return(results)  
    
  }

  
}

