#!/bin/sh
#
#SBATCH --account=def-mpennell
#SBATCH --job-name=seed_ord    		# Job name
#SBATCH --error=jobname.%j.txt 	# Stderr (%j expands to jobId)
#SBATCH --output=file_output.txt
#
#SBATCH --ntasks-per-node=10
#SBATCH --nodes=1
#SBATCH --time=00-05:00 		# time (DD-HH:MM)
#SBATCH --mem-per-cpu=15000		# memory (per cpu)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=fhenaod@zoology.ubc.ca

echo "Running on hostname `hostname`"

cd $SLURM_SUBMIT_DIR
echo "Working directory is `pwd`"
echo "Starting R at `date`."

module load gcc/5.4.0
module load r/3.5.0
R --vanilla 
Rscript seed_ords2cluster.R

echo "Finished R at `date`."

