# setup ------------------------------------------------------------------------
rm(list = ls())
wdSpeciesNames <- "~/JMPH/PREP/SpeciesNames/Data"
wdPAM <- "~/JMPH/PREP/PAM/Data"

# obtain species names from PAM chunks -----------------------------------------
setwd(wdPAM)
load(file = "LonLat_bPAM_1.rdata"); pam_names1 <- colnames(LonLat_bPAM_1)[4:ncol(LonLat_bPAM_1)];rm(LonLat_bPAM_1)
load(file = "LonLat_bPAM_2.rdata"); pam_names2 <- colnames(LonLat_bPAM_2)[3:ncol(LonLat_bPAM_2)];rm(LonLat_bPAM_2)
load(file = "LonLat_bPAM_3.rdata"); pam_names3 <- colnames(LonLat_bPAM_3)[3:ncol(LonLat_bPAM_3)];rm(LonLat_bPAM_3)
load(file = "LonLat_bPAM_4.rdata"); pam_names4 <- colnames(LonLat_bPAM_4)[3:ncol(LonLat_bPAM_4)];rm(LonLat_bPAM_4)
load(file = "LonLat_bPAM_5.rdata"); pam_names5 <- colnames(LonLat_bPAM_5)[3:ncol(LonLat_bPAM_5)];rm(LonLat_bPAM_5)
load(file = "LonLat_bPAM_6.rdata"); pam_names6 <- colnames(LonLat_bPAM_6)[3:ncol(LonLat_bPAM_6)];rm(LonLat_bPAM_6)
load(file = "LonLat_bPAM_7.rdata"); pam_names7 <- colnames(LonLat_bPAM_7)[3:ncol(LonLat_bPAM_7)];rm(LonLat_bPAM_7)
load(file = "LonLat_bPAM_8.rdata"); pam_names8 <- colnames(LonLat_bPAM_8)[3:ncol(LonLat_bPAM_8)];rm(LonLat_bPAM_8)
pam_names <- c(pam_names1, pam_names2, pam_names3, pam_names4, pam_names5, pam_names6, pam_names7, pam_names8)
rm(pam_names1, pam_names2, pam_names3, pam_names4, pam_names5, pam_names6, pam_names7, pam_names8)
pam_names <- gsub(" ", "_", pam_names)
setwd(wdSpeciesNames); save(pam_names, file = "PAM_Names.Rdata")


# obtain species names from COONEY dataset -------------------------------------
setwd(wdSpeciesNames)
cooney <- read.csv("Cooney_Pair_Data.csv", header = T, stringsAsFactors = F); colnames(cooney)[1] <- "Species.1"
cooney <- cooney[cooney$Tree == "mcc",]
cooney_names <- c(cooney$Species.1, cooney$Species.2); if(length(unique(cooney_names)) != length(cooney_names)){print("Warning! all names are not unique")}
save(cooney_names, file = "Cooney_Names.Rdata")

# load Angela Chira's name key -------------------------------------------------
setwd(wdSpeciesNames)
key_a <- read.csv(file = "AC_NameKey.csv", stringsAsFactors = F)
key_a[,1] <- gsub(" ", "_", key_a[,1])
key_a[,4] <- gsub(" ", "_", key_a[,4])


# Create a species names synonyms frame ----------------------------------------
matchednames <- as.data.frame(cooney_names, stringsAsFactors =F); colnames(matchednames) <- "cooney_name"

# add the AC synonyms -----------
matchednames$a_BL_latin_1 <- NA
matchednames$a_BL_latin_2 <- NA
for (i in 1:nrow(matchednames)) {
  matchednames$a_BL_latin_1[i] <- key_a$BL_latin[key_a$matching.btree_spp == matchednames$cooney_name[i]][1]
  matchednames$a_BL_latin_2[i] <- key_a$BL_latin[key_a$matching.btree_spp == matchednames$cooney_name[i]][2]
}

# remove entries with no ac synonym (shouldnt be any in this class) -----
matchednames <- matchednames[!is.na(matchednames$a_BL_latin_1) | !is.na(matchednames$a_BL_latin_2),]


# sort names alphebetically for species with two names in the AC key -----
for (i in 1:nrow(matchednames)){
  a <- matchednames$a_BL_latin_1[i]
  b <- matchednames$a_BL_latin_2[i]
  ab <- c(a,b)
  ab <- sort(ab)
  matchednames$a_BL_latin_1[i] <- ab[1]
  matchednames$a_BL_latin_2[i] <- ab[2]
}

# remove entries where two cooney names map to the same latin name / names
matchednames$dupcheck <- paste0(matchednames$a_BL_latin_1, matchednames$a_BL_latin_2)
matchednames$dup <- F
matchednames$dup[duplicated(matchednames$dupcheck)] <- T 
matchednames$order <- seq(1, nrow(matchednames)); matchednames <- matchednames[seq(dim(matchednames)[1],1),]
matchednames$dup[duplicated(matchednames$dupcheck)] <- T 
matchednames <- matchednames[matchednames$dup == F,] 
matchednames <- matchednames[,1:3]

# remove entries where there are two names in birdlife (latin names) for a single cooney name
matchednames <- matchednames[is.na(matchednames$a_BL_latin_2),] 
setwd(wdSpeciesNames); save(matchednames, file = "Matched_Names.Rdata") # hold onto just the good names.


# subset the cooney frame to match the good names -----------------------------
cooney <- cooney[cooney$Species.1 %in% matchednames$cooney_name & cooney$Species.2 %in% matchednames$cooney_name,]


# Add species 1 and species 2 latin names that will match the PAM
cooney$Species.1bl <- NA; cooney$Species.2bl <- NA
for (i in 1:nrow(cooney)) { # populate the new names columns with the bl names that match the pam
  cooney$Species.1bl[i] <- matchednames$a_BL_latin_1[matchednames$cooney_name == cooney$Species.1[i]]
  cooney$Species.2bl[i] <- matchednames$a_BL_latin_1[matchednames$cooney_name == cooney$Species.2[i]]
}
rm(i)


# subset cooney frame to only those entries with name matches for both species in pam chunks
cooney <- cooney[cooney$Species.1bl %in% pam_names & cooney$Species.2bl %in% pam_names,] 


# now double so that there is one entry per SPECIES rather than one entry per PAIR
cooney$uniquePair <- paste0(cooney$Species.1bl, cooney$Species.2bl)
cooney$uniquePairId <- seq(1, nrow(cooney))
cooney2 <- cooney
cooney2$Species.1 <- cooney$Species.2
cooney2$Species.2 <- cooney$Species.1
cooney2$Species.1bl <- cooney$Species.2bl
cooney2$Species.2bl <- cooney$Species.1bl
cooney <- rbind(cooney, cooney2)
cooney <- cooney[order(cooney$uniquePairId),]

# save the cooney frame. this will be used in PrepMainDataFrame.R AND in ConcatonatePAM.R
cooney[,"Species.1bl"] <- gsub("_", " ", cooney[,"Species.1bl"]) # replace underscores with spaces for the bl names that match the pam
cooney[,"Species.2bl"] <- gsub("_", " ", cooney[,"Species.2bl"])
setwd(wdSpeciesNames); save(cooney, file = "cooney.rdata")










