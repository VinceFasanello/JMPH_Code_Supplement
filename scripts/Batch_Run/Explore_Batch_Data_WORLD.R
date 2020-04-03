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
inwd <- "~/Box Sync/JMPH_2020/data/Batch_Run"
setwd(inwd)






# Load elevation data and view -------------------------------------------------
setwd(inwd)
load(file = "000_Setup_elev_raster_outputs.Rdata")
elev_raster_hold <- elev_raster
elev_raster[elev_raster$layer <= 0,] <- NA
elev_raster <- as.data.frame(elev_raster, xy=TRUE)

# view elevations
ggplot(data = elev_raster, aes(x = x , y =y, color = (layer)))+
  geom_point(cex = 0.5)+
  coord_equal()+
  theme_bw()+
  scale_color_viridis()

# view points where it is POSSIBLE to have an ai (will be background on our plots below.)
ggplot(data = elev_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = "darkgray")+
  coord_equal()+
  theme_bw()























































data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

sort(tapply(myworld$n_sp_sampled, myworld$BIOME, mean)) # so some variation in # species sampled per biome. 
sort(tapply(myworld$n_sp_sampled, myworld$BIOME, sum)) # so some variation in # total cells sampled per biome as well. 
# with these caveats, lets look at some more interesting things...
sort(tapply(myworld$m_aB_mcosts_ag, myworld$BIOME, mean))# cost by biome
myorder <- names(sort(tapply(myworld$m_aB_mcosts_ag[myworld$m_aB_mcosts_ag < quantile(myworld$m_aB_mcosts_ag, 0.9999)], myworld$BIOME[myworld$m_aB_mcosts_ag < quantile(myworld$m_aB_mcosts_ag, 0.9999)], mean)))

myworldp <- myworld[!is.na(myworld$BIOME),]
myworldp$BIOME <- factor(myworldp$BIOME, levels = myorder) # reorder the treat factor
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=BIOME, y=m_aB_mcosts_ag)) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = "blue")+
  geom_violin()+
  stat_summary(fun.data=data_summary, color = "red")+
  coord_flip()
p



p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = BIOME)) + 
  geom_point(size = 1.25, shape = "square")
p
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = BIOME)) + 
  geom_point(size = 1.25, shape = "square")+
  coord_equal()
p
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = BIOME)) + 
  geom_point(size = 1.25, shape = "square")+
  coord_cartesian()
p
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = BIOME)) + 
  geom_point(size = 1.25, shape = "square")+
  coord_polar()
p




sort(tapply(myworld$n_sp_sampled, myworld$REALM, mean)) # so some variation in # species sampled per biome. 
sort(tapply(myworld$n_sp_sampled, myworld$REALM, sum)) # so some variation in # total cells sampled per biome as well. 
# with these caveats, lets look at some more interesting things...
sort(tapply(myworld$m_aB_mcosts_ag, myworld$REALM, mean))# cost by biome
myorder <- names(sort(tapply(myworld$m_aB_mcosts_ag, myworld$REALM, median)))

myworldp <- myworld[!is.na(myworld$REALM),]
myworldp$REALM <- factor(myworldp$REALM, levels = myorder) # reorder the treat factor
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.999),], aes(x=REALM, y=m_aB_mcosts_ag)) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = "blue")+
  geom_violin()+
  stat_summary(fun.data=data_summary, color = "red")+
  coord_flip()
p



p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = REALM)) + 
  geom_point(size = 1.25, shape = "square")
p

p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = -GBL_STAT)) + 
  geom_point(size = 1.25, shape = "square")
p

mycol <- viridis(n = length(unique(myworld$ECO_NAME)), option = "C")
p <- ggplot(myworldp[myworldp$m_aB_mcosts_ag < quantile(myworldp$m_aB_mcosts_ag, 0.9999),], aes(x=m_a_lons_ag, y=m_a_lats_ag, color = ECO_NAME)) + 
  geom_point(size = 1.25, shape = "square")+ 
  theme(legend.position = "none") +
  scale_color_manual(values = mycol)

p


myworldp$REALM <- relevel(myworldp$REALM, ref = "Oceania") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Australasia") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "IndoMalay") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Nearctic") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Afrotropics") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Palearctic") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))

myworldp$REALM <- relevel(myworldp$REALM, ref = "Neotropics") # reorder the treat factor
summary(glm(myworldp$m_aB_mcosts_ag ~ myworldp$REALM ))




# some models with the world data
plot(myworld$div~ myworld$m_aB_mcosts_ag, col =rgb(0,0,0,0.1), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
plot(myworld$div[myworld$m_aB_mcosts_ag < 8000000]~ myworld$m_aB_mcosts_ag[myworld$m_aB_mcosts_ag < 8000000], col =rgb(0,0,0,0.25), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
summary(glm(myworld$div ~myworld$m_aB_mcosts_ag)) # negative relationship between diversity and cost
summary(glm(myworld$div ~myworld$m_aB_mcosts_ag + I(myworld$m_aB_mcosts_ag^2) )) # true here as well

plot(myworld$m_aB_mcosts_ag~ myworld$m_a_lats_ag, col =rgb(0,0,0,0.1), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
plot(myworld$m_aB_mcosts_ag[myworld$m_aB_mcosts_ag < 8000000]~ myworld$m_a_lats_ag[myworld$m_aB_mcosts_ag < 8000000], col =rgb(0,0,0,0.4), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
summary(glm(myworld$m_aB_mcosts_ag ~myworld$m_a_lats_ag )) # negative relationship between diversity and cost















# Exploratory World Plots ------------------------------------------------------
# LEAST COST PATH SUM COST -------------------------------------------
pdata <- myworld[order(mymelt$m_aB_mcosts_ag),]
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
pdf(file = paste("000_test_world.pdf"),height = 36, width = 18); grid::grid.draw(x); dev.off()







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