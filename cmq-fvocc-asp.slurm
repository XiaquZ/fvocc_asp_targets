#! /bin/bash
#SBATCH --job-name=cmq-climate-velocity
#SBATCH --ntasks=1 --cpus-per-task=10 --mem-per-cpu=7000mb
#SBATCH --time=26:35:00

source $VSC_HOME/.bashrc

conda activate VoCC_R_new

cd $VSC_DATA/fvocc_asp_targets/

Rscript targets_run_cmq.R
