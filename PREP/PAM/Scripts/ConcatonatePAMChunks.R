# setup ------------------------------------------------------------------------
rm(list = ls())
wdPAM <- "~/JMPH_1.1/PREP/PAM/Data"
wdSpeciesNames <- "~/JMPH_1.1/PREP/SpeciesNames/Data"

setwd(wdSpeciesNames)
load("cooney.rdata")

setwd(wdPAM)
load("LonLat_bPAM_1.rdata")
cbPAM <- LonLat_bPAM_1[,c("Longitude(x)", "Latitude(y)")]
LonLat_bPAM_1 <- LonLat_bPAM_1[,colnames(LonLat_bPAM_1) %in% cooney$Species.1bl]; ncol(LonLat_bPAM_1)
cbPAM <- cbind(cbPAM, LonLat_bPAM_1); rm(LonLat_bPAM_1)

load("LonLat_bPAM_2.rdata")
LonLat_bPAM_2 <- LonLat_bPAM_2[,colnames(LonLat_bPAM_2) %in% cooney$Species.1bl]; ncol(LonLat_bPAM_2)
cbPAM <- cbind(cbPAM, LonLat_bPAM_2); rm(LonLat_bPAM_2)

load("LonLat_bPAM_3.rdata")
LonLat_bPAM_3 <- LonLat_bPAM_3[,colnames(LonLat_bPAM_3) %in% cooney$Species.1bl]; ncol(LonLat_bPAM_3)
cbPAM <- cbind(cbPAM, LonLat_bPAM_3); rm(LonLat_bPAM_3)

load("LonLat_bPAM_4.rdata")
LonLat_bPAM_4 <- LonLat_bPAM_4[,colnames(LonLat_bPAM_4) %in% cooney$Species.1bl]; ncol(LonLat_bPAM_4)
cbPAM <- cbind(cbPAM, LonLat_bPAM_4); rm(LonLat_bPAM_4)

load("LonLat_bPAM_5.rdata")
LonLat_bPAM_5 <- LonLat_bPAM_5[,colnames(LonLat_bPAM_5) %in% cooney$Species.1bl]; ncol(LonLat_bPAM_5)
cbPAM <- cbind(cbPAM, LonLat_bPAM_5); rm(LonLat_bPAM_5)

load("LonLat_bPAM_6.rdata")
LonLat_bPAM_6 <- LonLat_bPAM_6[,colnames(LonLat_bPAM_6) %in% cooney$Species.1bl]; ncol(LonLat_bPAM_6)
cbPAM <- cbind(cbPAM, LonLat_bPAM_6); rm(LonLat_bPAM_6)

load("LonLat_bPAM_7.rdata")
LonLat_bPAM_7 <- LonLat_bPAM_7[,colnames(LonLat_bPAM_7) %in% cooney$Species.1bl]; ncol(LonLat_bPAM_7)
cbPAM <- cbind(cbPAM, LonLat_bPAM_7); rm(LonLat_bPAM_7)

load("LonLat_bPAM_8.rdata")
LonLat_bPAM_8 <- LonLat_bPAM_8[,colnames(LonLat_bPAM_8) %in% cooney$Species.1bl]; ncol(LonLat_bPAM_8)
cbPAM <- cbind(cbPAM, LonLat_bPAM_8); rm(LonLat_bPAM_8)

ncol(cbPAM)
setwd(wdPAM)
save(cbPAM, file = "cbPAM.rdata")