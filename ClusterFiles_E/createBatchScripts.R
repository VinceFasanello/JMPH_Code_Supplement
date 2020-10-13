rm(list=ls()); gc()

# create 200 batch scripts
for(i in 1: 200) 
  { tx  <- readLines(paste0("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/ClusterFiles_E/Main_E.R"))
    tx[4] <-paste0("batchNumber <- ",i)
    writeLines(tx, paste0("~/Box Sync/CB_VF_Shared/Dry_Lab/Projects/JMPH/ClusterFiles_E/Main_E",i,".R"))
}
