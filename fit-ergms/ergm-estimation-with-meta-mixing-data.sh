#!/bin/sh

#SBATCH --time=60:00:00
#SBATCH --mem=60000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log



module load R/4.3.1
module load gcc/10.2 pcre2/10.35 intel/2020.2 texlive/2018

R CMD BATCH --no-restore fit-ergms/ergm-estimation-with-meta-data.R 
