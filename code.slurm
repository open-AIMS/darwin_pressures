#!/bin/bash
#SBATCH --job-name=dh_press
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --partition=cpuq
#SBATCH --mem=400GB
#SBATCH --output=dh_press_%j.log
#SBATCH --error=dh_press_%j.stderr

module load singularity

make code_singularity
