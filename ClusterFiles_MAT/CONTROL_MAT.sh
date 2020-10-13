#!/bin/bash

# Give the job a name to help keep track of running jobs (optional)
#PBS -N MAT${PBS_ARRAYID}

# Specify the resources needed
#PBS -l nodes=1:ppn=1,walltime=167:00:00

#PBS -l pmem=8gb 

#PBS -t 1-200

export LD_LIBRARY_PATH=/export/geos-3.7.2/lib:${LD_LIBRARY_PATH}

# Load the environmental variables necessary for running R
module load R-3.6.1

# Finally run the R benchmark with the command
Rscript /home/cbotero/main_mat/Main_MAT${PBS_ARRAYID}.R