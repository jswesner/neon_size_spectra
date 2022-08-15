#!/bin/bash

# Example job submission script

# ensure anaconda is installed
# install with /apps/install-anaconda.sh

# This is a comment.
# Lines beginning with the # symbol are comments and are not interpreted by
# the Job Scheduler.

# Lines beginning with #SBATCH are special commands to configure the job.

### Job Configuration Starts Here #############################################


# Export all current environment variables to the job (Don't change this)
#SBATCH --get-user-env

# The default is one task per node
#SBATCH --ntasks=12
#SBATCH --nodes=1

#request 48 hours of runtime - the job will be killed if it exceeds this
#SBATCH --time=48:00:00

# Change email@example.com to your real email address
#SBATCH --mail-user=james.junker1@gmail.com
#SBATCH --mail-type=END



### Commands to run your program start here ####################################

pwd
echo "This is the R-batch-job running bayesian models of stream metabolism"

Rscript code/remote-met-mm-summ.R output/models/REDB_mm.rds

echo "Script finished"
