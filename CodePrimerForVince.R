load("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/PCA_DATA_SEND_TO_CB/ELE_COST_DATAFRAME_FOR_CB.Rdata")


######################################
# BoxCox function
require(EnvStats)

myBCtransform <- function(myvector) {
  # shift scale to positive numbers and identify optimal lambda for box-cox transformation
  mylambda <- boxcox(as.numeric(myvector)-min(as.numeric(myvector))+1, optimize = T)$lambda
  
  # transform
  myvector <- scale(boxcoxTransform(as.numeric(myvector)-min(as.numeric(myvector))+1, mylambda))
  return (scale(myvector))
}

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

hist(mydata$water_buffering)
mydata$water_buffering <- scale(myBCtransform(mydata$water_buffering))
hist(mydata$water_buffering)

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
require(psych)
myPCA <- principal(mydata[,c('tas_breadth', 'tas_position', 'pcp_breadth',
                             'pcp_position', 'mtn_mass', 'water_buffering',
                             'dispersal_ability', 'pair_age','distance', 
                             'boundary_length')], nfactors = 9, rotate = "none")
myPCA$loadings

# looks like the first four components properly load all the variables in the set 
# at least once (I label them for now simply using the highest loaded variable)

mydata$PC1.tasbreadth <- myPCA$scores[,'PC1']
mydata$PC2.mtnmass <- myPCA$scores[,'PC2']
mydata$PC3.boundarylength <- myPCA$scores[,'PC3']
mydata$PC4.dispersalability <- myPCA$scores[,'PC4']

######################################
# now we can run the pGLS
# load phylogenetic hypotheses
load("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/Other_Input_Data/BirdTrees/BirdTrees.Rdata")
# first we deal with phylogenetic non-independence by using the tip of one of the
# species in each pair as the placement of the pair in the tree
require(geiger)
require(phylolm)

CHK <- name.check(trees[[1]], mydata, data.names = mydata$Species.1)

# change the 'size' to iterate through different topology options
All.mods <- list()
counter <- 1
for (i in sample(x = 1:length(trees), size = 1)) {
  i <- 1
  mytree <- drop.tip(trees[[i]], CHK$tree_not_data)
  
  mymod <- phylolm(I(log(cost)) ~ PC1.tasbreadth + PC2.mtnmass + 
                     PC3.boundarylength + PC4.dispersalability,
                   phy = mytree, model = 'lambda', data = mydata)
  
  All.mods[[counter]] <- mymod
  counter <- counter+1
}

# still need to figure out how to summarize across trees (talk to Angela about it
# as she and I have already discussed how to do this). For now...

summary (All.mods[[1]])

layout(matrix(1:4, 2, 2, byrow = T))

par(mar = c(4.1,4.1,1,1))
plot(mydata$PC1.tasbreadth, log(mydata$cost), pch = 16, col = rgb(0,0,0,0.3),
     xlab = 'Breadth of temperature niche (PC1)', ylab = 'log(Elevational cost)')
abline(a=mymod$coefficients['(Intercept)'], 
       b = mymod$coefficients['PC1.tasbreadth'],
       col = 'red')

plot(mydata$PC2.mtnmass, log(mydata$cost), pch = 16, col = rgb(0,0,0,0.3),
     xlab = 'Mass of separating mountain (PC2)', ylab = 'log(Elevational cost)')
abline(a=mymod$coefficients['(Intercept)'], 
       b = mymod$coefficients['PC2.mtnmass'],
       col = 'red')

plot(mydata$PC3.boundarylength, log(mydata$cost), pch = 16, col = rgb(0,0,0,0.3),
     xlab = 'Length of boundary (PC3)', ylab = 'log(Elevational cost)')
abline(a=mymod$coefficients['(Intercept)'], 
       b = mymod$coefficients['PC3.boundarylength'],
       col = 'red')

plot(mydata$PC4.dispersalability, log(mydata$cost), pch = 16, col = rgb(0,0,0,0.3),
     xlab = 'Dispersal ability (PC4)', ylab = 'log(Elevational cost)')

# these findings make perfect sense... elevational barriers between sister
# species are significantly higher when (1) they have broader temperature niches
# (note that PC1 loadings indicate that the same is NOT true for the breadth 
# of precipitation niches), and (2) when the pairs live in SMALLER mountains, where
# elevational gradients are ore pronounced. The 3rd finding needs a little bit of 
# thinking, as it indicates that costs tend to be higher when boundaries are long.
# It could be that this is a case of reverse causation... i.e., that the boundaries 
# are longer because the barriers are higher. It could also simply be that 
# what drives this result is that the boundaries are longer when species are farther
# apart (distance also loads almost equally well as length of boundary). If that
# is the case, then it makes total sense (perhaps also with a reverse causation deal)...
# sister species that are physically farther apart tend to be so separated because 
# the elevational barriers between them ar more massive. Let's think about this more deeply 
# and discuss... makes me wonder whether boundary length and distance are 
# appropriate predictors for his model (leaning toward no but could be convinced 
# otherwise)



