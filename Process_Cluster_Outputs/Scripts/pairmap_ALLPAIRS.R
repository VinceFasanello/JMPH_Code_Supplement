setwd("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/Process_Cluster_Outputs/Data")
load(file =  "Pair_Barrier_Data_FEB2021.rdata")


# get world data -----------------------
world <- ne_coastline(scale = "medium", returnclass = "sf")

# set plotting parameters --------------
size = 1.75
alpha = 0.25
i <- "lat_mean_pair"


# Visualize --------------------------------------------------------------------
# Panel A - World Plot -----------------
pA <- ggplot(world)+ geom_sf() + geom_point(data = mypairdata[order(mypairdata[, i], decreasing = F),],
                                            aes(y=lat_mean_pair, x=lon_mean_pair),
                                            color = rgb(1,0,0,0.5), alpha = alpha, size = size, pch = 20)+
  labs(tag = "A")+xlab("Longitude")+ylab("Latitude")+
  coord_sf(expand = FALSE)+
  scale_y_continuous(breaks = seq(-100, 100, by = 20)) + 
  scale_x_continuous(breaks = seq(-200, 200, by = 50)) +
  theme_VJF()

# Panel B - hist/dens ------------------
pB <- ggplot(data = mypairdata, aes(x = lat_mean_pair))+
  geom_histogram(aes(y=..density..),binwidth = 1)+
  geom_density(color = "red", fill = rgb(1,0,0,0.1), trim = T)+
  xlim(-90, 90)+
  scale_x_continuous(limits = c(-85, 85), breaks = c(-80, -60, -40, -20, 0, 20, 40, 60, 80), expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_VJF() +
  xlab("") + ylab("Density")+
  coord_flip()+
  labs(tag = "B")

# output viz ---------------------------
pdf("Latpts_ALLPAIRS.pdf", width = 7*1.5, height = 2.4*1.5)
lay <- matrix(data = c(1,1,2), nrow = 1)
grid.arrange(pA, pB, layout_matrix = lay)
dev.off()

png("Latpts_ALLPAIRS.png", width = 7*1.5, height = 2.4*1.5, units = "in", res = 300)
lay <- matrix(data = c(1,1,2), nrow = 1)
grid.arrange(pA, pB, layout_matrix = lay)
dev.off()
rm(pA, pB, i, size, alpha, world, lay)