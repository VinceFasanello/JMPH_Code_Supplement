rm(list = ls()); gc()
require(ggplot2) # load packages
require(EnvStats)
require(psych)
require(geiger)
require(phylolm)
require(sensiPhy)
require(rnaturalearth)
require(rnaturalearthdata)
require(viridis)
require(tidyr)

vlog <- function(x){
  log( x + abs(min( x , na.rm = T)) + 1)
}

myBCtransform <- function(myvector) {
  # shift scale to positive numbers and identify optimal lambda for box-cox transformation
  mylambda <- boxcox(as.numeric(myvector)-min(as.numeric(myvector))+1, optimize = T)$lambda
  
  # transform
  myvector <- scale(boxcoxTransform(as.numeric(myvector)-min(as.numeric(myvector))+1, mylambda))
  return (scale(myvector))
}

load("/Users/boterolab1/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/Analyze_Processed_Cluster_Outputs/dataforpca.Rdata")
rm(mydata_m)



mydata <- mydata_e
rm(mydata_e)
######################################
# prepare raw predictors for PCA
hist(mydata$tas_breadth)
mydata$tas_breadth <- scale(myBCtransform(mydata$tas_breadth))
hist(mydata$tas_breadth)

hist(mydata$tas_position)
mydata$tas_position <- scale(myBCtransform(mydata$tas_position))
hist(mydata$tas_position)

hist(mydata$pcp_breadth)
mydata$pcp_breadth <- scale(mydata$pcp_breadth)

hist(mydata$pcp_position)
mydata$pcp_position <- scale(mydata$pcp_position)

hist(mydata$mtn_mass)
mydata$mtn_mass <- scale(myBCtransform(mydata$mtn_mass))
hist(mydata$mtn_mass)

hist(mydata$wtr_mass)
mydata$wtr_mass <- scale(myBCtransform(mydata$wtr_mass))
hist(mydata$wtr_mass)

hist(mydata$dispersal_ability)
mydata$dispersal_ability <- scale(myBCtransform(mydata$dispersal_ability))
hist(mydata$dispersal_ability)

hist(mydata$pair_age)
mydata$pair_age <- scale(myBCtransform(mydata$pair_age))
hist(mydata$pair_age)

hist(mydata$distance)
mydata$distance <- scale(myBCtransform(mydata$distance))
hist(mydata$distance)

hist(mydata$boundary_length)
mydata$boundary_length <- scale(myBCtransform(mydata$boundary_length))
hist(mydata$boundary_length)

######################################
# Now run PCA of raw predictors
myPCA <- principal(mydata[,c('tas_breadth', 'tas_position', 'pcp_breadth',
                             'pcp_position', 'mtn_mass', 'wtr_mass',
                             'dispersal_ability', 'pair_age','distance', 
                             'boundary_length')], nfactors = 9, rotate = "none")
myPCA$loadings

# looks like the first four components properly load all the variables in the set 
# at least once (I label them for now simply using the highest loaded variable)

mydata$PC1 <- myPCA$scores[,'PC1']
mydata$PC2 <- myPCA$scores[,'PC2']
mydata$PC3 <- myPCA$scores[,'PC3']
mydata$PC4 <- myPCA$scores[,'PC4']

######################################
# now we can run the pGLS
# load phylogenetic hypotheses
load("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/Other_Input_Data/BirdTrees/BirdTrees.Rdata")
# first we deal with phylogenetic non-independence by using the tip of one of the
# species in each pair as the placement of the pair in the tree
CHK <- geiger::name.check(trees[[1]], mydata, data.names = mydata$Species.1)

# sensimod <- my_tree_phylm(formula = I(log(cost)) ~ PC1 + PC2 + PC3 + PC4,
#               data = mydata,
#               phy = trees,
#               n.tree = 2, 
#               model = "lambda",
#               track = TRUE,
#               add_median_residuals = TRUE)
# summary(sensimod)
# my_sensimod(sensimod)

# change the 'size' to iterate through different topology options
All.mods <- list()
counter <- 1
for (i in sample(x = 1:length(trees), size = 1)) {
  i <- 1 # always run on tree one for now.
  mytree <- drop.tip(trees[[i]], CHK$tree_not_data)
  mymod <- phylolm(I(log(cost)) ~ PC1 + PC2 + PC3 + PC4,
                   phy = mytree, model = 'lambda', data = mydata)
  All.mods[[counter]] <- mymod
  counter <- counter+1
}

# still need to figure out how to summarize across trees (talk to Angela about it
# as she and I have already discussed how to do this). For now...

summary (All.mods[[1]])



# global plots of PCs for interpretation aid. 
world <- ne_coastline(scale = "medium", returnclass = "sf")
size = 2
alpha = 0.8
# PC1 increases with latitude (relationship is weakest in Afrotropics)
x1 <- ggplot(world)+ geom_sf() + geom_point(data = mydata[order(mydata$PC1, decreasing = T),], aes(y=lat, x=lon, color = PC1), alpha = alpha, size = size)+ 
  ggtitle("PC1")+scale_color_viridis()
print(x1)


# PC2 is high near mountains
x2 <- ggplot(world)+ geom_sf() + geom_point(data = mydata[order(mydata$PC2, decreasing = F),], aes(y=lat, x=lon, color = PC2), alpha = alpha, size = size)+
  ggtitle("PC2")+scale_color_viridis()
print(x2)
mtns <- ggplot(world)+ geom_sf() + geom_point(data = mydata[order(mydata$mtn_mass, decreasing = F),], aes(y=lat, x=lon, color = mtn_mass), alpha = alpha, size = size)+
  ggtitle("mtns") + scale_color_viridis()
print(mtns)

# PC3 decreases with latitude.
x3 <- ggplot(world)+ geom_sf() + geom_point(data = mydata[order(mydata$PC3, decreasing = F),], aes(y=lat, x=lon, color = PC3), alpha = alpha, size = size)+
  ggtitle("PC3")+scale_color_viridis()
print(x3)

# PC4 is non-significant and does not have a strong global pattern. 
x4 <- ggplot(world)+ geom_sf() + geom_point(data = mydata[order(mydata$PC4, decreasing = T),], aes(y=lat, x=lon, color = PC4), alpha = 0.8) + 
  ggtitle("PC4") + scale_color_viridis()
print(x4)




# # high correspondance between variables in the mydata_e and mydata_m datasets.
# plot(scale(vlog(mydata_e$cost)) ~ scale(vlog(mydata_m$cost))) # cost
# abline(0,1)
# 
# plot(scale((mydata_e$lat)) ~ scale((mydata_m$lat))) # latitude for dispersal subsection of range
# abline(0,1)
# 
# plot(scale((mydata_e$distance)) ~ scale((mydata_m$distance))) # distance for dispersal subsection of range
# abline(0,1)

