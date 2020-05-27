rm(list=ls()); gc()

# create 200 batch scripts
for(i in 1: 200) 
  { tx  <- readLines(paste0("~/Box Sync/JMPH/ClusterFiles_P/Main_P.R"))
    tx[4] <-paste0("batchNumber <- ",i)
    writeLines(tx, paste0("~/Box Sync/JMPH/ClusterFiles_P/Main_P",i,".R"))
}
