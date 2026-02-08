#!/bin/bash
#SBATCH --job-name=bootSim
#SBATCH --partition=short-cpu
#SBATCH --output=log/%x_%A_%a.out
#SBATCH --error=log/%x_%A_%a.err
#SBATCH --mem=1G

# usage: sbatch scripts/submit_one_scenario.slurm <n> <beta> <err>
N=$1
BETA=$2
ERR=$3

REP_ID=${SLURM_ARRAY_TASK_ID}

module load R
Rscript run_rep.R ${N} ${BETA} ${ERR} ${REP_ID} 500 100 0.05 731
