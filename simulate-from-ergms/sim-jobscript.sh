#!/bin/sh

#SBATCH --job-name="simulate-networks-new-mixing-data-w-dnf"
#SBATCH --time=5:00:00
#SBATCH --mem=64000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log

module load r/4.5.1-iikl
module load glpk/5.0-55rr

R CMD BATCH --no-restore simulate-from-ergms/simulate-networks-from-meta-data-ergm-fit.R slurm_output/$SLURM_JOB_ID_$SLURM_JOB_NAME.Rout
