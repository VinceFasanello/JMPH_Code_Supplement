rm(list=ls()); gc()

# create 200 batch scripts
for(i in 1: 200) 
  { tx  <- readLines(paste0("~/Box Sync/JMPH/ClusterFiles_T_RUNONJGPACCOUNT/Main_T.R"))
    tx[4] <-paste0("batchNumber <- ",i)
    writeLines(tx, paste0("~/Box Sync/JMPH/ClusterFiles_T_RUNONJGPACCOUNT/Main_T",i,".R"))
}
