#!/bin/sh

#SBATCH --job-name="new-mixing-data-w-dnf"
#SBATCH --time=5:00:00
#SBATCH --mem=20000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log
 
module load r/4.5.1-iikl
module load glpk/5.0-55rr



# COMMENT OUT ONE BATCH COMMAND BELOW AS NEEDED

## fit stepwise original
R CMD BATCH --no-restore fit-ergms/fit-stepwise-ergms.R slurm_output/$SLURM_JOB_ID_$SLURM_JOB_NAME.Rout

## fit starting with indegrees
##R CMD BATCH --no-restore fit-ergms/fit-stepwise-indegree-first.R slurm_output/$SLURM_JOB_ID_$SLURM_JOB_NAME.Rout
