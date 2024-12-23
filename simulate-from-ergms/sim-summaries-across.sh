#!/bin/sh

#SBATCH --job-name="summaries-from-updated-synthpop-2023-10-12-w-dist-all-plos-one-1e6-hotelling"
#SBATCH --time=1:00:00
#SBATCH --mem=64000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log

module load r/4.4.0-yycctsj  
module load glpk/5.0-zifs7bb

R CMD BATCH --no-restore simulate-from-ergms/summaries-across-simulated-distributions.R slurm_output/$SLURM_JOB_ID_$SLURM_JOB_NAME.Rout
