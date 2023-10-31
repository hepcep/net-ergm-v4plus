#!/bin/sh

#SBATCH --time=60:00:00
#SBATCH --mem=60000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu


module load R/4.3.1

R CMD BATCH --no-restore fit-ergms/ergm-estimation-with-meta-data.R 
