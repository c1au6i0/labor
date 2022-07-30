#!/bin/bash
#SBATCH --job-name=master
#SBATCH --output=./log/master_%A.out
#SBATCH --partition=scu-cpu   # cluster-specific
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
#SBATCH --time=48:00:00   # HH/MM/SS
#SBATCH --mem=16G   # memory requested, units available: K,M,G,T
#SBATCH --mail-user=clz4002@med.cornell.edu
#SBATCH --mail-type=ALL

echo   $(date)
R -e "targets::tar_make_future(workers = 5)"
