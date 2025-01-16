#!/bin/sh

#SBATCH --job-name="new-mixing-data-stepwise-dist-odeg-ideg0-1"
#SBATCH --time=30:00:00
#SBATCH --mem=20000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log

module load r/4.4.0-yycctsj  
module load glpk/5.0-zifs7bb

## fit stepwise original
R CMD BATCH --no-restore fit-ergms/fit-stepwise-ergms.R slurm_output/$SLURM_JOB_ID_$SLURM_JOB_NAME.Rout

## fit starting with indegrees
### NEW BATCH COMMAND
