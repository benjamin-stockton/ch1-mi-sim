#!/bin/bash 

    # #SBATCH --partition=general
    # 
    # #SBATCH --constraint='epyc128'
    # 
    # #SBATCH --cpus-per-task=50
    # 
    # #SBATCH --ntasks=1
    # 
    # #SBATCH --nodes=1
    # 
    # #SBATCH --mail-type=ALL
    # 
    # #SBATCH --mail-user=benjamin.stockton@uconn.edu

    cd .. 


    # source /etc/profile.d/modules.sh
    # 
    # module purge
    # 
    # module load r/4.3.2 mpi/openmpi/4.1.4
echo "Running sim_scripts/test-pn-mi-sim-setting-1.R" 
time Rscript "sim_scripts/test-pn-mi-sim-setting-1.R"
echo "Running sim_scripts/test-pn-mi-sim-setting-2.R" 
time Rscript "sim_scripts/test-pn-mi-sim-setting-2.R"

echo "Done!"
