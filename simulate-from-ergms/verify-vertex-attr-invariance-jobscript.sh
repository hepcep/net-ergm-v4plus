#!/bin/sh

#SBATCH --job-name="verify-vertex-attr-invariance-new-mixing-data-w-dnf"
#SBATCH --time=00:30:00
#SBATCH --mem=24 o000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log

module load r/4.5.1-iikl
module load glpk/5.0-55rr

R CMD BATCH --no-restore simulate-from-ergms/verify-vertex-attr-invariance.R slurm_output/$SLURM_JOB_ID_$SLURM_JOB_NAME.Rout
