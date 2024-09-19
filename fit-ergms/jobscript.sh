#!/bin/sh

#SBATCH --job-name="non-empty-net-only-san-control-params"
#SBATCH --time=30:00:00
#SBATCH --mem=10000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log

module load r/4.4.0-yycctsj  
module load glpk/5.0-zifs7bb

R CMD BATCH --no-restore fit-ergms/ergm-estimation-with-meta-data.R slurm_output/$SLURM_JOB_ID_$SLURM_JOB_NAME.Rout
