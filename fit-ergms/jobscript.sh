#!/bin/sh

#SBATCH --time=10:00:00
#SBATCH --mem=60000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log

module load r/4.3.1-lmofgb4  
module load glpk/5.0-zifs7bb

R CMD BATCH --no-restore fit-ergms/ergm-estimation-with-meta-data.R slurm_output/$SLURM_JOB_ID.Rout
