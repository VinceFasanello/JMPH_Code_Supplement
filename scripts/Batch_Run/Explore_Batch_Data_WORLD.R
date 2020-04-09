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
require(ggthemes)

# open and concatonate all results file sets -------------------------------------------
inwd <- "~/Box Sync/JMPH/data/Batch_Run"
setwd(inwd)
load(file = "lcp_elevation_results_1rowper_sampledgridcell.rdata")


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





# plot LCP COST ----------------------------------------------------------------
pdata <- myworld
pdata$value <- pdata$m_aB_mcosts_ag
toremove <- 0.01

pdata$value <- log(pdata$value + 1)
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[1]  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[2] 
# hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
pdata <- pdata[pdata$value < mymax & pdata$value > mymin,]
pdata$value <- scale(pdata$value)
p1 <- ggplot(data = back_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = back_raster$layer)+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = value), inherit.aes = F)+
  coord_equal()+
  theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_color_viridis()+
  ggtitle("LCP COST") + xlab("") +ylab("")
p1


# plot LCP Length ----------------------------------------------------------------
pdata <- myworld
pdata$value <- pdata$m_aB_plengths_ag
toremove <- 0.01

pdata$value <- log(pdata$value + 1)
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[1]  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[2] 
# hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
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

# plot LCP DIRECTNESS ----------------------------------------------------------------
pdata <- myworld
pdata$value <- pdata$m_aB_mcosts_ag / pdata$m_aB_plengths_ag
toremove <- 0.01

pdata$value <- log(pdata$value + 1)
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[1]  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, c(0 + toremove, 1 - toremove))[2] 
# hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
pdata <- pdata[pdata$value < mymax & pdata$value > mymin,]
pdata$value <- scale(pdata$value)
p1 <- ggplot(data = back_raster, aes(x = x , y =y))+
  geom_point(cex = 0.5, color = back_raster$layer)+
  geom_point(cex = 0.5, data = pdata, aes(x = m_a_lons_ag , y =m_a_lats_ag, color = value), inherit.aes = F)+
  coord_equal()+
  theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_color_viridis()+
  ggtitle("LCP DIRECTNESS (COST / LENGTH)") + xlab("") +ylab("")
p1



# MODELS ----------------------------------
mymod <- lm(scale(m_aB_mcosts_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F), data = myworld); summary(mymod); hist(residuals(mymod), breaks = 100)
plot(scale(m_aB_mcosts_ag, center = F) ~ scale(m_a_eles_ag, center = F), data = myworld, col = rgb(0,0,0,0.5), pch = 19)
plot(scale(m_aB_mcosts_ag, center = F) ~ scale(abs(m_a_lats_ag), center = F), data = myworld, col = rgb(0,0,0,0.15), pch = 19)

mymod <- lm(scale(m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F), data = myworld); summary(mymod); hist(residuals(mymod), breaks = 100)
plot(scale(m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F), data = myworld, col = rgb(0,0,0,0.5), pch = 19)
plot(scale(m_aB_plengths_ag, center = F) ~ scale(abs(m_a_lats_ag), center = F), data = myworld, col = rgb(0,0,0,0.15), pch = 19)

# mymod <- lm(scale(m_aB_mcosts_ag/m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F)+I(scale(abs(m_a_lats_ag), center = F)^2), data = myworld); summary(mymod); hist(residuals(mymod), breaks = 100)
mymod <- lm(scale(m_aB_mcosts_ag/m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center = F), data = myworld); summary(mymod); hist(residuals(mymod), breaks = 100)
plot(scale(m_aB_mcosts_ag/m_aB_plengths_ag, center = F) ~ scale(m_a_eles_ag, center = F), data = myworld, col = rgb(0,0,0,0.5), pch = 19)
plot(scale(m_aB_mcosts_ag/m_aB_plengths_ag, center = F) ~ scale(abs(m_a_lats_ag), center = F), data = myworld, col = rgb(0,0,0,0.15), pch = 19)


mymod <- lm(scale(div, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center=F)+scale(m_aB_mcosts_ag, center = F), data = myworld); summary(mymod); hist(residuals(mymod), breaks = 100)
car::vif(mymod)
plot(scale(div) ~ scale(m_a_eles_ag, center = F), data = myworld, col = rgb(0,0,0,0.5), pch = 19)
plot(scale(div) ~ scale(abs(m_a_lats_ag), center = F), data = myworld, col = rgb(0,0,0,0.15), pch = 19)
plot(scale(div) ~ scale(m_aB_mcosts_ag, center = F), data = myworld, col = rgb(0,0,0,0.15), pch = 19)
mymod <- lm(scale(div, center = F) ~ scale(m_a_eles_ag, center = F)+scale(abs(m_a_lats_ag), center=F)+scale(m_aB_mcosts_ag, center = F)+scale(m_aB_plengths_ag, center = F), data = myworld); summary(mymod); hist(residuals(mymod), breaks = 100)
plot(scale(div) ~ scale(m_aB_plengths_ag, center = F), data = myworld, col = rgb(0,0,0,0.15), pch = 19)


mymod <- lm(scale(m_aB_mcosts_ag, center = F) ~scale(m_aB_plengths_ag, center = F), data = myworld); summary(mymod); hist(residuals(mymod), breaks = 100)





# LETS LOOK AT SOME BIOMESSSS -------------
# first, look at how many species per biome
# sort(tapply(myworld$n_sp_sampled, myworld$BIOME, mean)) # so some variation in # species sampled per biome. (could also be driven by variation in richness!)
# sort(tapply(myworld$n_sp_sampled, myworld$BIOME, sum)) # so some variation in # total cells sampled per biome as well. (could also be driven by variation in area covered by each biome!)


# with these caveats, lets look at some more interesting things...
sort(tapply(scale(myworld$m_aB_mcosts_ag), myworld$BIOME, mean), decreasing = T)# cost by biome
sort(tapply(scale(myworld$m_a_eles_ag), myworld$BIOME, mean), decreasing = T)# elevation by biome


pdata <- myworld[!is.na(myworld$BIOME),]
pdata$BIOME <- factor(pdata$BIOME, levels = names(sort(tapply(scale(pdata$m_aB_mcosts_ag), pdata$BIOME, mean), decreasing = F))) # reorder the treat factor
p1 <- ggplot(pdata, aes(x=BIOME, y=m_aB_mcosts_ag)) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = rgb(0,0,1,0.5))+
  geom_violin(scale = "width", fill = rgb(0,0,0,0.1))+
  stat_summary(fun.data=data_summary, color = "darkred")+
  coord_flip()+
  theme_minimal() + theme(axis.text.x = element_blank()) + ylab("COST")
# p1

pdata <- myworld[!is.na(myworld$BIOME),]
pdata$BIOME <- factor(pdata$BIOME, levels = names(sort(tapply(scale(pdata$m_aB_mcosts_ag), pdata$BIOME, mean), decreasing = F))) # reorder the treat factor
p2 <- ggplot(pdata, aes(x=BIOME, y=m_a_eles_ag)) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = rgb(0,0,1,0.5))+
  geom_violin(scale = "width", fill = rgb(0,0,0,0.1))+
  stat_summary(fun.data=data_summary, color = "darkred")+
  coord_flip()+
  theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) + xlab("") + ylab("ELEVATION")
# p2

grid.arrange(p1,p2,layout_matrix = rbind(c(1,1,1,2,2)))






# LETS LOOK AT SOME REALM DATA -------------
# first, look at how many species per biome
sort(tapply(myworld$n_sp_sampled, myworld$REALM, mean)) # so some variation in # species sampled per biome. (could also be driven by variation in richness!)
sort(tapply(myworld$n_sp_sampled, myworld$REALM, sum)) # so some variation in # total cells sampled per REALM as well. (could also be driven by variation in area covered by each REALM!)


# with these caveats, lets look at some more interesting things...
sort(tapply(scale(myworld$m_aB_mcosts_ag), myworld$REALM, mean), decreasing = T)# cost by REALM
sort(tapply(scale(myworld$m_a_eles_ag), myworld$REALM, mean), decreasing = T)# elevation by REALM


pdata <- myworld[!is.na(myworld$REALM),]; pdata <- pdata[pdata$REALM != "Oceania",]
pdata$REALM <- factor(pdata$REALM, levels = names(sort(tapply(scale(pdata$m_aB_mcosts_ag), pdata$REALM, mean), decreasing = F))) # reorder the treat factor
p1 <- ggplot(pdata, aes(x=REALM, y=m_aB_mcosts_ag)) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = rgb(0,0,1,0.5))+
  geom_violin(scale = "width", fill = rgb(0,0,0,0.1))+
  stat_summary(fun.data=data_summary, color = "darkred")+
  coord_flip()+
  theme_minimal() + theme(axis.text.x = element_blank()) + ylab("COST")
p1

pdata <- myworld[!is.na(myworld$REALM),]; pdata <- pdata[pdata$REALM != "Oceania",]
pdata$REALM <- factor(pdata$REALM, levels = names(sort(tapply(scale(pdata$m_aB_mcosts_ag), pdata$REALM, mean), decreasing = F))) # reorder the treat factor
p2 <- ggplot(pdata, aes(x=REALM, y=m_a_eles_ag)) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = rgb(0,0,1,0.5))+
  geom_violin(scale = "width", fill = rgb(0,0,0,0.1))+
  stat_summary(fun.data=data_summary, color = "darkred")+
  coord_flip()+
  theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y = element_blank()) + xlab("") + ylab("ELEVATION")
p2

grid.arrange(p1,p2,layout_matrix = rbind(c(1,1,1,2,2)))
























# quick models look good here too! 
pdata <- myworld[!is.na(myworld$REALM),]; pdata <- pdata[pdata$REALM != "Oceania",]
pdata$value <- scale(pdata$m_aB_mcosts_ag)
pdata$REALM <- as.character(pdata$REALM); pdata$REALM <- factor(pdata$REALM, levels = c(names(sort(tapply(scale(pdata$m_aB_mcosts_ag, center = F), pdata$REALM, mean), decreasing = F))))

pdata$REALM <- relevel(pdata$REALM, ref = "Australasia") # reorder the treat factor
mymod <- glm(pdata$value ~ pdata$REALM )

mysumm <- summary(mymod)$coefficients
myres <- matrix(nrow = 6, ncol=6)
rownames(myres) <- c("pdata$REALMAustralasia","pdata$REALMNeotropics","pdata$REALMIndoMalay", "pdata$REALMAfrotropics","pdata$REALMNearctic","pdata$REALMPalearctic")
colnames(myres) <- rownames(myres)
myres_p <- myres; myres_est <- myres
for(i in c("pdata$REALMNeotropics","pdata$REALMIndoMalay", "pdata$REALMAfrotropics","pdata$REALMNearctic","pdata$REALMPalearctic")){
  myres_est["pdata$REALMAustralasia",i] <- mysumm[i, "Estimate"]
  myres_p["pdata$REALMAustralasia",i] <- mysumm[i, "Pr(>|t|)"]
}



pdata$REALM <- relevel(pdata$REALM, ref = "Neotropics") # reorder the treat factor
mymod <- glm(pdata$value ~ pdata$REALM )

mysumm <- summary(mymod)$coefficients
for(i in c("pdata$REALMAustralasia","pdata$REALMIndoMalay", "pdata$REALMAfrotropics","pdata$REALMNearctic","pdata$REALMPalearctic")){
  myres_est["pdata$REALMNeotropics",i] <- mysumm[i, "Estimate"]
  myres_p["pdata$REALMNeotropics",i] <- mysumm[i, "Pr(>|t|)"]
}



pdata$REALM <- relevel(pdata$REALM, ref = "IndoMalay") # reorder the treat factor
mymod <- glm(pdata$value ~ pdata$REALM )

mysumm <- summary(mymod)$coefficients
for(i in c("pdata$REALMAustralasia","pdata$REALMNeotropics", "pdata$REALMAfrotropics","pdata$REALMNearctic","pdata$REALMPalearctic")){
  myres_est["pdata$REALMIndoMalay",i] <- mysumm[i, "Estimate"]
  myres_p["pdata$REALMIndoMalay",i] <- mysumm[i, "Pr(>|t|)"]
}

pdata$REALM <- relevel(pdata$REALM, ref = "Afrotropics") # reorder the treat factor
mymod <- glm(pdata$value ~ pdata$REALM )

mysumm <- summary(mymod)$coefficients
for(i in c("pdata$REALMAustralasia","pdata$REALMNeotropics","pdata$REALMIndoMalay","pdata$REALMNearctic","pdata$REALMPalearctic")){
  myres_est["pdata$REALMAfrotropics",i] <- mysumm[i, "Estimate"]
  myres_p["pdata$REALMAfrotropics",i] <- mysumm[i, "Pr(>|t|)"]
}

pdata$REALM <- relevel(pdata$REALM, ref = "Nearctic") # reorder the treat factor
mymod <- glm(pdata$value ~ pdata$REALM )

mysumm <- summary(mymod)$coefficients
for(i in c("pdata$REALMAustralasia","pdata$REALMNeotropics","pdata$REALMIndoMalay", "pdata$REALMAfrotropics","pdata$REALMPalearctic")){
  myres_est["pdata$REALMNearctic",i] <- mysumm[i, "Estimate"]
  myres_p["pdata$REALMNearctic",i] <- mysumm[i, "Pr(>|t|)"]
}

pdata$REALM <- relevel(pdata$REALM, ref = "Palearctic") # reorder the treat factor
mymod <- glm(pdata$value ~ pdata$REALM )

mysumm <- summary(mymod)$coefficients
for(i in c("pdata$REALMAustralasia","pdata$REALMNeotropics","pdata$REALMIndoMalay", "pdata$REALMAfrotropics","pdata$REALMNearctic")){
  myres_est["pdata$REALMPalearctic",i] <- mysumm[i, "Estimate"]
  myres_p["pdata$REALMPalearctic",i] <- mysumm[i, "Pr(>|t|)"]
}




myp <- 0.05
myres_est[myres_p >= myp] <- NA
m <- round(myres_est, 2)
colnames(m) <- c("Australasia", "Neotropics", "IndoMalay", "Afrotropics", "Nearctic", "Palearctic")
rownames(m) <- colnames(m)
# m <- get_lower_tri(m) # uncomment for just lower triangle
melt_m <- reshape::melt(m)
melt_m$X1 <- factor(melt_m$X1, levels = c("Australasia", "Neotropics", "IndoMalay", "Afrotropics", "Nearctic", "Palearctic"))
melt_m$X2 <- factor(melt_m$X2, levels = c("Australasia", "Neotropics", "IndoMalay", "Afrotropics", "Nearctic", "Palearctic"))

pp <- ggplot(melt_m, aes(x = X1, y = X2, fill = value)) + 
  geom_tile(show.legend = F)+
  geom_text(aes(X1, X2, label = value), color = "black", size = 2, angle = 0) +
  theme_tufte(base_family="Helvetica")+
  scale_y_discrete(position = "right")+
  theme(axis.ticks = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.background = element_rect(colour = NA, fill = "transparent"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, margin = margin(t=-2.5, b = -2.5)),
        axis.text.y.left = element_text(angle = 0, hjust = 0, vjust = 0.5, margin = margin(l=-2.5, r = -2.5), debug = T))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(min(melt_m$value),max(melt_m$value)), space = "Lab", 
                       name="Estimate", na.value = "transparent")
pp

m2 <- m; m2[1:length(m2)] <- 0
# m2 <- get_lower_tri(m2) # uncomment for just lower triangle
melt_m2 <- reshape::melt(m2)
melt_m2$X1 <- factor(melt_m2$X1, levels = c("Australasia", "Neotropics", "IndoMalay", "Afrotropics", "Nearctic", "Palearctic"))
melt_m2$X2 <- factor(melt_m2$X2, levels = c("Australasia", "Neotropics", "IndoMalay", "Afrotropics", "Nearctic", "Palearctic"))

ppp <- ggplot(melt_m2, aes(x = X1, y = X2, fill = value)) + 
  geom_tile(show.legend = F)+
  geom_text(aes(X1, X2, label = value), color = "gray", size = 2, angle = 0) +
  theme_tufte(base_family="Helvetica")+
  scale_y_discrete(position = "right")+
  theme(axis.ticks = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.background = element_rect(colour = NA, fill = "transparent"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.title = element_blank(),
        axis.text = element_text(size = 6, colour = "white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, margin = margin(t=-2.5, b = -2.5)),
        axis.text.y.left = element_text(angle = 0, hjust = 0, vjust = 0.5, margin = margin(l=-2.5, r = -2.5), debug = T))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "gray", 
                       midpoint = 0, limit = c(min(melt_m$value),max(melt_m$value)), space = "Lab", 
                       name="Estimate", na.value = "transparent")
ppp


tempbiome <- sort(tapply(scale(pdata$m_aB_mcosts_ag, center = F), pdata$REALM, mean), decreasing = F)# cost by biome
hold <- names(tempbiome)
tempbiome <- data.frame(tempbiome); colnames(tempbiome) <- c("cost")
tempbiome$name <- hold; rm(hold)

hold <- tapply(scale(myworld$m_a_eles_ag, center = F), myworld$REALM, mean)
for (i in 1: nrow(tempbiome)) {
  tempbiome$ele[i] <- hold[which(names(hold) == tempbiome$name[i])]
}

p <- ggplot(pdata, aes(x=REALM, y=scale(m_aB_mcosts_ag, center = F))) + 
  geom_jitter(width = 0.05, alpha = 0.05, color = rgb(0,0,1,0.5))+
  geom_violin(scale = "width", fill = rgb(0,0,0,0.1))+
  stat_summary(fun.data=data_summary, color = "darkred")+
  geom_hline(yintercept = 12 )+
  coord_flip()+
  theme_minimal() + theme(axis.text.x = element_blank()) + ylab( "Barrier Magnitude Sufficient for Speciation") +
  annotate(geom="text", x=c(seq(1:6)), y=c(-1), label=round(tempbiome$cost, 3), color="darkred")
p


pppp <- p + annotation_custom(ggplotGrob(ppp), xmin = 1, xmax = 3, ymin = 7, ymax = 12)
pppp <- pppp + annotation_custom(ggplotGrob(pp), xmin = 1, xmax = 3, ymin =7, ymax = 12)
pppp












































































pdata <- myworld
pdata$value <- pdata$m_aB_mcosts_ag
pdata$value <- scale(pdata$value, center = F)

myx1 <- scale(abs(pdata$m_a_lats_ag), center = F)
myx2 <- scale(pdata$m_a_eles_ag, center = F)
myy <- pdata$value
# myr <- myworld$Species.1bl
# x <- summary(lmer(myy ~ myx1 + myx2 + (1|myr))); x
x <- lm(myy ~ myx1 + myx2)
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
# plotline$y <- x$coefficients["(Intercept)","Estimate"] + plotline$x*x$coefficients["myx1", "Estimate"]
plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
plot(jitter(myy[myy < 5]) ~ myx1[myy < 5], col = rgb(0,0,0,0.1), pch = 19)
points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)




pdata <- myworld
pdata$value <- pdata$m_aB_mcosts_ag
toretain <- 0.999

pdata$value <- log(pdata$value + 1)
pdata <- pdata[order(pdata$value),]
mymin <- quantile(pdata$value, 0.01)  # remove outliers for color scaling ease. 
mymax <- quantile(pdata$value, toretain)  # hist(pdata$value, breaks = 100); abline(v=mymin, col = "blue"); abline(v=mymax, col= "red")
pdata <- pdata[pdata$value < mymax & pdata$value > mymin,]
pdata$value <- scale(pdata$value, center = F)


myx1 <- scale(abs(pdata$m_a_lats_ag), center = F)
myx2 <- scale(pdata$m_a_eles_ag, center = F)
myy <- pdata$value; # hist(myy)
# myr <- myworld$Species.1bl
# x <- summary(lmer(myy ~ myx1 + myx2 + (1|myr))); x
x <- lm(myy ~ myx1 + myx2); summary(x)
plotline <- data.frame(seq(min(myx1, na.rm = T),max(myx1, na.rm = T), length.out = 1000), seq(min(myx1, na.rm = T),max(myx1, na.rm = T),length.out = 1000)); colnames(plotline) <- c("x", "y")
# plotline$y <- x$coefficients["(Intercept)","Estimate"] + plotline$x*x$coefficients["myx1", "Estimate"]
plotline$y <- x$coefficients["(Intercept)"] + plotline$x*x$coefficients["myx1"]
plot(jitter(myy[myy < 5]) ~ myx1[myy < 5], col = rgb(0,0,0,0.1), pch = 19)
points(plotline$x, plotline$y, type = "l", col = "red", lwd = 2)





















# some models with the world data
plot(myworld$div~ myworld$m_aB_mcosts_ag, col =rgb(0,0,0,0.1), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
plot(myworld$div[myworld$m_aB_mcosts_ag < 8000000]~ myworld$m_aB_mcosts_ag[myworld$m_aB_mcosts_ag < 8000000], col =rgb(0,0,0,0.25), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
summary(glm(myworld$div ~myworld$m_aB_mcosts_ag)) # negative relationship between diversity and cost
summary(glm(myworld$div ~myworld$m_aB_mcosts_ag + I(myworld$m_aB_mcosts_ag^2) )) # true here as well

plot(myworld$m_aB_mcosts_ag~ myworld$m_a_lats_ag, col =rgb(0,0,0,0.1), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
plot(myworld$m_aB_mcosts_ag[myworld$m_aB_mcosts_ag < 8000000]~ myworld$m_a_lats_ag[myworld$m_aB_mcosts_ag < 8000000], col =rgb(0,0,0,0.4), pch = 19) # interesting lines...these exist at all scales when you zoom in and out. I think they are wide ranging species.
summary(glm(myworld$m_aB_mcosts_ag ~myworld$m_a_lats_ag )) # negative relationship between diversity and cost
















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