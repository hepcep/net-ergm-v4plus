#!/bin/sh

#SBATCH --time=60:00:00
#SBATCH --mem=60000
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=aditya_khanna@brown.edu
#SBATCH --output=slurm_output/output_%j.log
#SBATCH --error=slurm_output/error_%j.log


export PATH=/oscar/home/akhann16/sfw/glpk-5.0/bin:$PATH #for rglpk solver
export LD_LIBRARY_PATH=/oscar/home/akhann16/sfw/glpk-5.0/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/oscar/home/akhann16/sfw/glpk-5.0/lib/pkgconfig:$PKG_CONFIG_PATH

module load r/4.3.1-lmofgb4  
#module load gcc/10.2 pcre2/10.35 intel/2020.2 texlive/2018
module load glpk/5.0-zifs7bb

R CMD BATCH --no-restore fit-ergms/ergm-estimation-with-meta-data.R slurm_output/$SLURM_JOB_ID.Rout
