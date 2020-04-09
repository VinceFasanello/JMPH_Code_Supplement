# clear workspace and load all required packages -------------------------------
rm(list=ls())
options(scipen=999)
require(rgdal)
require(raster)
require(dplyr)
require(movecost)
require(sp)
require(raster)
require(gdistance)
require(reshape2)
library(broom)
library(ggplot2)
library(rgdal)
require(ggplot2)
require(viridis)
require(gridExtra)
require(mapproj)

# open and concatonate all results file sets -------------------------------------------
inwd <- "~/Box Sync/JMPH/data/Batch_Run"
setwd(inwd)
load(file = "lcp_elevation_results_1rowper_sampledgridcell.rdata")
load(file = "lcp_elevation_results_1rowper_origin.rdata")


templ <- aggregate(abs(mydata$m_a_lats_ag) ~ mydata$Species.1bl, FUN = mean, na.rm = T)
tempc <- aggregate(mydata$m_aB_mcosts_ag ~ mydata$Species.1bl, FUN = mean, na.rm = T)
tempe <- aggregate(mydata$m_a_eles_ag ~ mydata$Species.1bl, FUN = mean, na.rm = T)
templen <- aggregate(mydata$m_aB_plengths_ag ~ mydata$Species.1bl, FUN = mean, na.rm = T)
temp <- cbind(templ, tempc[,2], tempe[,2], templen[,2]); colnames(temp) <- c("sp", "lat", "cost", "ele", "len")


data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# Load elevation data and view -------------------------------------------------
setwd(inwd)
load(file = "000_Setup_elev_raster_outputs.Rdata")
elev_raster_hold <- elev_raster
elev_raster[elev_raster$layer <= 0,] <- NA
elev_raster <- as.data.frame(elev_raster, xy=TRUE)
back_raster <- elev_raster
back_raster$layer[!is.na(back_raster$layer)] <- "darkgray"


# lets look at LCP COST! --------------------------------------------------------
# plot LCP COST --------------------------------------------
pdata <- myworld
pdata$value <- pdata$m_aB_mcosts_ag
toremove <- 0.01

pdata$value <- log(pdata$value + 1)
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[1]  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[2]  # hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
pdata <- pdata[pdata$value < mymax & pdata$value > mymin,]
pdata$value <- scale(pdata$value)
p1 <- ggplot(data = back_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = back_raster$layer)+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = value), inherit.aes = F)+
  coord_equal()+
  theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_color_viridis()+
  ggtitle("Barrier Magnitude Sufficient for Speciation") + xlab("") +ylab("")
p1

# models ---------------------------------------------------
# mymod <- lmer(scale(m_aB_mcosts_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|pairID/Species.1bl), data = mydata); summary(mymod); # hist(residuals(mymod), breaks = 100)
# mymod <- lmer(scale(m_aB_mcosts_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|Species.1bl), data = mydata); summary(mymod); # hist(residuals(mymod), breaks = 100)
# mymod <- lm(scale(m_aB_mcosts_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F), data = myworld); summary(mymod); # hist(residuals(mymod), breaks = 100)
mymod <- lm(scale(temp$cost, center = F) ~ scale(temp$ele, center = F) + scale(abs(temp$lat), center = F)); summary(mymod); # hist(residuals(mymod), breaks = 50)


# model plots ------------------------------------
# full data, y = cost, x = latitude ----
pdata <- temp
pdata$value <- pdata$cost
pdata$value <- scale(pdata$value, center = F)
myx1 <- scale(abs(pdata$lat), center = F)
myx2 <- scale(pdata$ele, center = F)
myy <- pdata$value
x <- lm(myy ~ myx1 + myx2); summary(x)
myxlab <- "Absolute Latitude"
myylab <- "Barrier Magnitude Sufficient for Speciation"
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
plot(myy ~ myx1, col = rgb(0,0,0,0.25), pch = 19, xlab = myxlab, ylab = myylab)
points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)


# zoom bottom, y = cost, x = latitude --
pdata <- temp
pdata$value <- pdata$cost
toretain <- 0.70
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, 0.00)  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, toretain)  # hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
pdata <- pdata[pdata$value < mymax & pdata$value > mymin,]
pdata$value <- scale(pdata$value, center = F)
myx1 <- scale(abs(pdata$lat), center = F)
myx2 <- scale(pdata$ele, center = F)
myy <- pdata$value
x <- lm(myy ~ myx1 + myx2); summary(x)
myxlab <- "Absolute Latitude"
myylab <- "Barrier Magnitude Sufficient for Speciation"
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
plot(myy ~ myx1, col = rgb(0,0,0,0.25), pch = 19, xlab = myxlab, ylab = myylab)
points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)



# full data, y = cost, x = elevation ----
pdata <- temp
pdata$value <- pdata$cost
pdata$value <- scale(pdata$value, center = F)
myx2 <- scale(abs(pdata$lat), center = F)
myx1 <- scale(pdata$ele, center = F)
myy <- pdata$value
x <- lm(myy ~ myx1 + myx2); summary(x)
myxlab <- "Elevation"
myylab <- "Barrier Magnitude Sufficient for Speciation"
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
plot(myy ~ myx1, col = rgb(0,0,0,0.25), pch = 19, xlab = myxlab, ylab = myylab)
points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)



















# lets look at LCP LENGTH! --------------------------------------------------------
# plot LCP LENGTH --------------------------------------------
pdata <- myworld
pdata$value <- pdata$m_aB_plengths_ag
toremove <- 0.01

pdata$value <- log(pdata$value + 1)
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[1]  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[2]  # hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
pdata <- pdata[pdata$value < mymax & pdata$value > mymin,]
pdata$value <- scale(pdata$value)
p1 <- ggplot(data = back_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = back_raster$layer)+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = value), inherit.aes = F)+
  coord_equal()+
  theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_color_viridis()+
  ggtitle("LCP LENGTH") + xlab("") +ylab("")
p1

# models ---------------------------------------------------
# mymod <- lmer(scale(m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|pairID/Species.1bl), data = mydata); summary(mymod); # hist(residuals(mymod), breaks = 100)
# mymod <- lmer(scale(m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|Species.1bl), data = mydata); summary(mymod); # hist(residuals(mymod), breaks = 100)
# mymod <- lm(scale(m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F), data = myworld); summary(mymod); # hist(residuals(mymod), breaks = 100)
mymod <- lm(scale(temp$len, center = F) ~ scale(temp$ele, center = F) + scale(abs(temp$lat), center = F)); summary(mymod); # hist(residuals(mymod), breaks = 50)


# model plots ------------------------------------
# full data, y = cost, x = latitude ----
pdata <- temp
pdata$value <- pdata$len
pdata$value <- scale(pdata$value, center = F)
myx1 <- scale(abs(pdata$lat), center = F)
myx2 <- scale(pdata$ele, center = F)
myy <- pdata$value
x <- lm(myy ~ myx1 + myx2); summary(x)
myxlab <- "Absolute Latitude"
myylab <- "LCP Length"
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
plot(myy ~ myx1, col = rgb(0,0,0,0.25), pch = 19, xlab = myxlab, ylab = myylab)
points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)


# zoom bottom, y = len, x = latitude --
pdata <- temp
pdata$value <- pdata$len
toretain <- 0.70
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, 0.00)  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, toretain)  # hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
pdata <- pdata[pdata$value < mymax & pdata$value > mymin,]
pdata$value <- scale(pdata$value, center = F)
myx1 <- scale(abs(pdata$lat), center = F)
myx2 <- scale(pdata$ele, center = F)
myy <- pdata$value
x <- lm(myy ~ myx1 + myx2); summary(x)
myxlab <- "Absolute Latitude"
myylab <- "LCP Length"
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
plot(myy ~ myx1, col = rgb(0,0,0,0.25), pch = 19, xlab = myxlab, ylab = myylab)
points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)


# # probably see similarity in wedges because.....
# summary(lm(cost ~ len, data = temp)) # higher cost with higher length.. makes sense. .....could also be that we need MORE SAMPLING in northern areas to catch shorter paths (this is more likely...)


# full data, y = len, x = elevation ----
pdata <- temp
pdata$value <- pdata$len
pdata$value <- scale(pdata$value, center = F)
myx2 <- scale(abs(pdata$lat), center = F)
myx1 <- scale(pdata$ele, center = F)
myy <- pdata$value
x <- lm(myy ~ myx1 + myx2); summary(x)
myxlab <- "Elevation"
myylab <- "LCP Length"
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
plot(myy ~ myx1, col = rgb(0,0,0,0.25), pch = 19, xlab = myxlab, ylab = myylab)
points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)








# lets look at LCP DIRECTNESS! --------------------------------------------------------
# plot LCP DIRECTNESS --------------------------------------------
pdata <- myworld
pdata$value <- pdata$m_aB_mcosts_ag / pdata$m_aB_plengths_ag
toremove <- 0.01

pdata$value <- log(pdata$value + 1)
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[1]  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[2]  # hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
pdata <- pdata[pdata$value < mymax & pdata$value > mymin,]
pdata$value <- scale(pdata$value)
p1 <- ggplot(data = back_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = back_raster$layer)+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = value), inherit.aes = F)+
  coord_equal()+
  theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_color_viridis()+
  ggtitle("LCP DIRECTNESS") + xlab("") +ylab("")
p1

# models ---------------------------------------------------
# mymod <- lmer(scale(pdata$m_aB_mcosts_ag / pdata$m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|pairID/Species.1bl), data = mydata); summary(mymod); # hist(residuals(mymod), breaks = 100)
# mymod <- lmer(scale(pdata$m_aB_mcosts_ag / pdata$m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|Species.1bl), data = mydata); summary(mymod); # hist(residuals(mymod), breaks = 100)
# mymod <- lm(scale(pdata$m_aB_mcosts_ag / pdata$m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F), data = myworld); summary(mymod); # hist(residuals(mymod), breaks = 100)
mymod <- lm(scale((temp$cost/temp$len), center = F) ~ scale(temp$ele, center = F) + scale(abs(temp$lat), center = F)); summary(mymod); # hist(residuals(mymod), breaks = 50)


# # model plots ------------------------------------
# # full data, y = Directness, x = latitude ----
# pdata <- temp
# pdata$value <- temp$cost/temp$len
# pdata$value <- scale(pdata$value, center = F)
# myx1 <- scale(abs(pdata$lat), center = F)
# myx2 <- scale(pdata$ele, center = F)
# myy <- pdata$value
# x <- lm(myy ~ myx1 + myx2); summary(x)
# myxlab <- "Absolute Latitude"
# myylab <- "LCP Directness"
# plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
# plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
# plot(myy ~ myx1, col = rgb(0,0,0,0.25), pch = 19, xlab = myxlab, ylab = myylab)
# points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)


# # full data, y = Directness, x = elevation ----
# pdata <- temp
# pdata$value <- temp$cost/temp$len
# pdata$value <- scale(pdata$value, center = F)
# myx2 <- scale(abs(pdata$lat), center = F)
# myx1 <- scale(pdata$ele, center = F)
# myy <- pdata$value
# x <- lm(myy ~ myx1 + myx2); summary(x)
# myxlab <- "elevation"
# myylab <- "LCP Directness"
# plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
# plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
# plot(myy ~ myx1, col = rgb(0,0,0,0.25), pch = 19, xlab = myxlab, ylab = myylab)
# points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)














# LETS LOOK AT SOME BIOMES -------------
tempbiome <- sort(tapply(scale(myworld$m_aB_mcosts_ag, center = F), myworld$BIOME, mean), decreasing = F)# cost by biome
hold <- names(tempbiome)
tempbiome <- data.frame(tempbiome); colnames(tempbiome) <- c("cost")
tempbiome$name <- hold; rm(hold)

hold <- tapply(scale(myworld$m_a_eles_ag, center = F), myworld$BIOME, mean)
for (i in 1: nrow(tempbiome)) {
  tempbiome$ele[i] <- hold[which(names(hold) == tempbiome$name[i])]
}





pdata <- myworld[!is.na(myworld$BIOME),]
pdata$BIOME <- factor(pdata$BIOME, levels = names(sort(tapply(scale(pdata$m_aB_mcosts_ag, center = F), pdata$BIOME, mean), decreasing = F))) # reorder the treat factor
p1 <- ggplot(pdata, aes(x=BIOME, y=scale(m_aB_mcosts_ag, center = F))) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = rgb(0,0,1,0.5))+
  geom_violin(scale = "width", fill = rgb(0,0,0,0.1))+
  stat_summary(fun.data=data_summary, color = "darkred")+
  geom_hline(yintercept = 12 )+
  coord_flip()+
  theme_minimal() + theme(axis.text.x = element_blank()) + ylab( "Barrier Magnitude Sufficient for Speciation") +
  annotate(geom="text", x=c(seq(1:14)), y=c(-1), label=round(tempbiome$cost, 3), color="darkred")
 # p1 

 pdata <- myworld[!is.na(myworld$BIOME),]
 pdata$BIOME <- factor(pdata$BIOME, levels = names(sort(tapply(scale(pdata$m_aB_mcosts_ag, center = F), pdata$BIOME, mean), decreasing = F))) # reorder the treat factor
 p2 <- ggplot(pdata, aes(x=BIOME, y=scale(m_a_eles_ag, center = F))) + 
   geom_jitter(width = 0.05, alpha = 0.05, color = rgb(0,0,1,0.5))+
   geom_violin(scale = "width", fill = rgb(0,0,0,0.1))+
   stat_summary(fun.data=data_summary, color = "darkred")+
   coord_flip()+
   theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) + xlab("") + ylab("ELEVATION")+
   annotate(geom="text", x=c(seq(1:14)), y=c(-1), label=round(tempbiome$ele, 3), color="darkred")
 # p2 

 grid.arrange(p1,p2,layout_matrix = rbind(c(1,1,1,2,2)))
 
 
 
 # Look at biomes map --------------------------------------------
# pdata <- myworld
# myworld$biome_cost <- NA
# # myworld$biome_ele <- NA
#  
#  for (i in 1:nrow(myworld)) {
#    if (!is.na(myworld$BIOME[i])){
#      myworld$biome_cost[i] <-  tempbiome$cost[tempbiome$name == myworld$BIOME[i]]
#      # myworld$biome_ele[i] <-  tempbiome$ele[tempbiome$name == myworld$BIOME[i]]
#    }
#    
#  }
#  pdata$value  <- myworld$biome_cost
#  pdata$value <- scale(log10(pdata$value+1))
#  p1 <- ggplot(data = back_raster, aes(x = x , y =y))+
#    geom_point(cex = 0.5, color = back_raster$layer)+
#    geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = value), inherit.aes = F)+
#    coord_equal()+
#    theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
#    scale_color_viridis()+
#    ggtitle( "Barrier Magnitude Sufficient for Speciation") + xlab("") +ylab("")
#  p1
# 
#  pdata$value  <- as.factor(myworld$BIOME)
#  p1 <- ggplot(data = back_raster, aes(x = x , y =y))+
#    geom_point(cex = 0.5, color = back_raster$layer)+
#    geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = value), inherit.aes = F)+
#    coord_equal()+
#    theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
#    ggtitle( "BIOMES OF THE WORLD") + xlab("") +ylab("")+
#    scale_color_viridis(discrete = TRUE, option = "C")+ 
#    guides(color = guide_legend(override.aes = list(size=5)))
#  p1

