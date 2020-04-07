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
load(file = "lcp_elevation_results_1rowper_origin.rdata")


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

# # view elevations
# ggplot(data = elev_raster, aes(x = x , y =y, color = (layer)))+
#   geom_point(cex = 0.5)+
#   coord_equal()+
#   theme_bw()+
#   scale_color_viridis()
# 
# # view points where it is POSSIBLE to have an ai (will be background on our plots below.)
# ggplot(data = back_raster, aes(x = x , y =y, color = layer))+
#   geom_point(cex = 0.5)+
#   coord_equal()+
#   theme_bw()

# MODELS ----------------------------------
mymod <- lmer(scale(m_aB_mcosts_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|pairID/Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
mymod <- lmer(scale(m_aB_mcosts_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
# plot(scale(m_aB_mcosts_ag, center = F) ~ scale(m_a_eles_ag, center = F), data = mydata, col = rgb(0,0,0,0.5), pch = 19)
# plot(scale(m_aB_mcosts_ag, center = F) ~ scale(abs(m_a_lats_ag), center = F), data = mydata, col = rgb(0,0,0,0.15), pch = 19)


myx1 <- scale(abs(mydata$m_a_lats_ag), center = F)
myx2 <- scale(mydata$m_a_eles_ag, center = F)
myy <- scale(mydata$m_aB_mcosts_ag, center = F); hist(myy)
myr <- mydata$Species.1bl
x <- summary(lmer(myy ~ myx1 + myx2 + (1|myr))); x
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
plotline$y <- x$coefficients["(Intercept)","Estimate"] + plotline$x*x$coefficients["myx1", "Estimate"]
plot(jitter(myy[myy < 5]) ~ myx1[myy < 5], col = rgb(0,0,0,0.025), pch = 19)
points(plotline$x, plotline$y, type = "l", col = "red")
















mymod <- lmer(scale(m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|pairID/Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
mymod <- lmer(scale(m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
# plot(scale(m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F), data = mydata, col = rgb(0,0,0,0.5), pch = 19)
# plot(scale(m_aB_plengths_ag, center = F) ~ scale(abs(m_a_lats_ag), center = F), data = mydata, col = rgb(0,0,0,0.15), pch = 19)

mymod <- lmer(scale(m_aB_mcosts_ag/m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|pairID/Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
mymod <- lmer(scale(m_aB_mcosts_ag/m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F) + (1|Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
# plot(scale(m_aB_mcosts_ag/m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F), data = mydata, col = rgb(0,0,0,0.5), pch = 19)
# plot(scale(m_aB_mcosts_ag/m_aB_plengths_ag, center = F) ~ scale(abs(m_a_lats_ag), center = F), data = mydata, col = rgb(0,0,0,0.15), pch = 19)


mymod <- lmer(scale(div, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center=F)+scale(m_aB_mcosts_ag, center = F) + (1|pairID/Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
mymod <- lmer(scale(div, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center=F)+scale(m_aB_mcosts_ag, center = F) + (1|Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
car::vif(mymod)
plot(scale(div) ~ scale(m_a_eles_ag, center = F), data = mydata, col = rgb(0,0,0,0.5), pch = 19)
plot(scale(div) ~ scale(abs(m_a_lats_ag), center = F), data = mydata, col = rgb(0,0,0,0.15), pch = 19)
plot(scale(div) ~ scale(m_aB_mcosts_ag, center = F), data = mydata, col = rgb(0,0,0,0.15), pch = 19)
mymod <- lmer(scale(div, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center=F)+scale(m_aB_mcosts_ag, center = F)+scale(m_aB_plengths_ag, center = F) + (1|pairID/Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
mymod <- lmer(scale(div, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center=F)+scale(m_aB_mcosts_ag, center = F)+scale(m_aB_plengths_ag, center = F) + (1|Species.1bl), data = mydata); summary(mymod); hist(residuals(mymod), breaks = 100)
plot(scale(div) ~ scale(m_aB_plengths_ag, center = F), data = myworld, col = rgb(0,0,0,0.15), pch = 19)


mymod <- lm(scale(m_aB_mcosts_ag, center = F) ~scale(m_aB_plengths_ag, center = F), data = myworld); summary(mymod); hist(residuals(mymod), breaks = 100)






























# some models
plot(mymelt$div[mymelt$m_aB_mcosts_ag < 8000000]~ mymelt$m_aB_mcosts_ag[mymelt$m_aB_mcosts_ag < 8000000]) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
summary(glm(mymelt$div[mymelt$m_aB_mcosts_ag < 8000000] ~mymelt$m_aB_mcosts_ag[mymelt$m_aB_mcosts_ag < 8000000] )) # negative relationship between diversity and cost
summary(glm(mymelt$div[mymelt$m_aB_mcosts_ag < 8000000] ~mymelt$m_aB_mcosts_ag[mymelt$m_aB_mcosts_ag < 8000000] + I(mymelt$m_aB_mcosts_ag[mymelt$m_aB_mcosts_ag < 8000000]^2) )) # true here as well











# Exploratory World Plots ------------------------------------------------------
# LEAST COST PATH SUM COST -------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata$m_aB_mcosts_ag <- scale(pdata$m_aB_mcosts_ag, center = F)
plot(pdata$m_aB_mcosts_ag~pdata$m_a_lats_ag) # notice that there are some natural breaks that will influence our interpretations...


# exclude top 0.01% of data points -------------------------
pd1 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.9999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p1 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd1, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 0.10% of data points -------------------------
pd2 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p2 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd2, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 1.00% of data points -------------------------
pd3 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.99),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p3 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd3, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# exclude top 10.0% of data points -------------------------
pd4 <- pdata[pdata$m_aB_mcosts_ag <= quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = 0.9),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p4 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd4, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_aB_mcosts_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

lay <- rbind(c(1),
             c(2),
             c(3),
             c(4))
x <- grid.arrange(p1, p2, p3, p4, layout_matrix = lay)
grid::grid.draw(x)
pdf(file = paste("000_test.pdf"),height = 36, width = 18); grid::grid.draw(x); dev.off()










# Look at geographic region...-------------------------
# These data are flawed...geographic region was for sp1, or the pair centroid, does not work here. 
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
plot(pdata$m_aB_mcosts_ag~pdata$m_a_lats_ag)
pd1 <- pdata[pdata$Geographic.region == "Africa",]
pd2 <- pdata[pdata$Geographic.region == "Eurasia",]
pd3 <- pdata[pdata$Geographic.region == "South America",]
pd4 <- pdata[pdata$Geographic.region == "North America",]
pd5 <- pdata[pdata$Geographic.region == "Oceania",]
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = Geographic.region), inherit.aes = F)+
  coord_equal()+
  theme_bw()












# Assign lat groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$m_a_lats_ag, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$latgroup <- NA
pdata$latgroup[pdata$m_a_lats_ag < qs[1]] <- 1
pdata$latgroup[pdata$m_a_lats_ag >= qs[1] & pdata$m_a_lats_ag < qs[2]] <- 2
pdata$latgroup[pdata$m_a_lats_ag >= qs[2] & pdata$m_a_lats_ag < qs[3]] <- 3
pdata$latgroup[pdata$m_a_lats_ag >= qs[3] & pdata$m_a_lats_ag < qs[4]] <- 4
pdata$latgroup[pdata$m_a_lats_ag >= qs[4] & pdata$m_a_lats_ag < qs[5]] <- 5
pdata$latgroup[pdata$m_a_lats_ag >= qs[5] & pdata$m_a_lats_ag < qs[6]] <- 6
pdata$latgroup[pdata$m_a_lats_ag >= qs[6] & pdata$m_a_lats_ag < qs[7]] <- 7
pdata$latgroup[pdata$m_a_lats_ag >= qs[7] & pdata$m_a_lats_ag < qs[8]] <- 8
pdata$latgroup[pdata$m_a_lats_ag >= qs[8] & pdata$m_a_lats_ag < qs[9]] <- 9
pdata$latgroup[pdata$m_a_lats_ag >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = latgroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# Assign ele groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$m_a_eles_ag, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$elegroup <- NA
pdata$elegroup[pdata$m_a_eles_ag < qs[1]] <- 1
pdata$elegroup[pdata$m_a_eles_ag >= qs[1] & pdata$m_a_eles_ag < qs[2]] <- 2
pdata$elegroup[pdata$m_a_eles_ag >= qs[2] & pdata$m_a_eles_ag < qs[3]] <- 3
pdata$elegroup[pdata$m_a_eles_ag >= qs[3] & pdata$m_a_eles_ag < qs[4]] <- 4
pdata$elegroup[pdata$m_a_eles_ag >= qs[4] & pdata$m_a_eles_ag < qs[5]] <- 5
pdata$elegroup[pdata$m_a_eles_ag >= qs[5] & pdata$m_a_eles_ag < qs[6]] <- 6
pdata$elegroup[pdata$m_a_eles_ag >= qs[6] & pdata$m_a_eles_ag < qs[7]] <- 7
pdata$elegroup[pdata$m_a_eles_ag >= qs[7] & pdata$m_a_eles_ag < qs[8]] <- 8
pdata$elegroup[pdata$m_a_eles_ag >= qs[8] & pdata$m_a_eles_ag < qs[9]] <- 9
pdata$elegroup[pdata$m_a_eles_ag >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = elegroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# Assign div groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$div, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$divgroup <- NA
pdata$divgroup[pdata$div < qs[1]] <- 1
pdata$divgroup[pdata$div >= qs[1] & pdata$div < qs[2]] <- 2
pdata$divgroup[pdata$div >= qs[2] & pdata$div < qs[3]] <- 3
pdata$divgroup[pdata$div >= qs[3] & pdata$div < qs[4]] <- 4
pdata$divgroup[pdata$div >= qs[4] & pdata$div < qs[5]] <- 5
pdata$divgroup[pdata$div >= qs[5] & pdata$div < qs[6]] <- 6
pdata$divgroup[pdata$div >= qs[6] & pdata$div < qs[7]] <- 7
pdata$divgroup[pdata$div >= qs[7] & pdata$div < qs[8]] <- 8
pdata$divgroup[pdata$div >= qs[8] & pdata$div < qs[9]] <- 9
pdata$divgroup[pdata$div >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = divgroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# Assign cost groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$m_aB_mcosts_ag, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$costgroup <- NA
pdata$costgroup[pdata$m_aB_mcosts_ag < qs[1]] <- 1
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[1] & pdata$m_aB_mcosts_ag < qs[2]] <- 2
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[2] & pdata$m_aB_mcosts_ag < qs[3]] <- 3
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[3] & pdata$m_aB_mcosts_ag < qs[4]] <- 4
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[4] & pdata$m_aB_mcosts_ag < qs[5]] <- 5
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[5] & pdata$m_aB_mcosts_ag < qs[6]] <- 6
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[6] & pdata$m_aB_mcosts_ag < qs[7]] <- 7
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[7] & pdata$m_aB_mcosts_ag < qs[8]] <- 8
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[8] & pdata$m_aB_mcosts_ag < qs[9]] <- 9
pdata$costgroup[pdata$m_aB_mcosts_ag >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = costgroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()


# Assign length groups by quantile..-----------------------------------------------
pdata <- mymelt[order(mymelt$m_aB_plengths_ag),]
pdata <- myworld[order(myworld$m_aB_mcosts_ag),]
qs <- quantile(pdata$m_aB_plengths_ag, na.rm = T, probs = c( .1, .2, .3, .4, .5, .6, .7, .8, .9))
pdata$lengroup <- NA
pdata$lengroup[pdata$m_aB_plengths_ag < qs[1]] <- 1
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[1] & pdata$m_aB_plengths_ag < qs[2]] <- 2
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[2] & pdata$m_aB_plengths_ag < qs[3]] <- 3
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[3] & pdata$m_aB_plengths_ag < qs[4]] <- 4
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[4] & pdata$m_aB_plengths_ag < qs[5]] <- 5
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[5] & pdata$m_aB_plengths_ag < qs[6]] <- 6
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[6] & pdata$m_aB_plengths_ag < qs[7]] <- 7
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[7] & pdata$m_aB_plengths_ag < qs[8]] <- 8
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[8] & pdata$m_aB_plengths_ag < qs[9]] <- 9
pdata$lengroup[pdata$m_aB_plengths_ag >= qs[9]] <- 10

# viz
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = lengroup), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()







pdata <- mymelt[order(mymelt$m_aB_mcosts_ag),]
pdata$m_a_eles_ag <- scale(pdata$m_a_eles_ag, center = F)
plot(pdata$m_a_eles_ag~pdata$m_a_lats_ag) # notice that there are some natural breaks that will influence our interpretations...


# exclude top 0.01% of data points -------------------------
pd1 <- pdata[pdata$m_a_eles_ag <= quantile(pdata$m_a_eles_ag, na.rm = T, probs = 0.9999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p1 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd1, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_a_eles_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

pd2 <- pdata[pdata$m_a_eles_ag <= quantile(pdata$m_a_eles_ag, na.rm = T, probs = 0.999),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p2 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd2, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_a_eles_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

pd3 <- pdata[pdata$m_a_eles_ag <= quantile(pdata$m_a_eles_ag, na.rm = T, probs = 0.99),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p3 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd3, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_a_eles_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

pd4 <- pdata[pdata$m_a_eles_ag <= quantile(pdata$m_a_eles_ag, na.rm = T, probs = 0.9),];# plot(pd$m_aB_mcosts_ag~pd$m_a_lats_ag)
p4 <- ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  geom_point(cex = 0.5, data = pd4, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = m_a_eles_ag), inherit.aes = F)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

