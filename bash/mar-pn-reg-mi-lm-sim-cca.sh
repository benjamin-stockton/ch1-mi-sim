#!/bin/bash 
#SBATCH --partition=general
#SBATCH --constraint='epyc128'
#SBATCH --cpus-per-task=25
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=benjamin.stockton@uconn.edu
cd .. 

source /etc/profile.d/modules.sh
module purge
module load r/4.3.2
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-1-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-1-cca.R"
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-2-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-2-cca.R"
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-3-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-3-cca.R"
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-4-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-4-cca.R"
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-5-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-5-cca.R"
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-6-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-6-cca.R"
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-7-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-7-cca.R"
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-8-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-8-cca.R"
echo "Running sim_scripts/mar-pn-reg-mi-sim_setting-9-cca.R" 
time Rscript "sim_scripts/mar-pn-reg-mi-sim_setting-9-cca.R"

echo "Done!"
