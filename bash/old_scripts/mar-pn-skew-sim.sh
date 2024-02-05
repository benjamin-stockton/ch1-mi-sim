#!/usr/bin/env bash 

cd .. 

echo "Running sim_scripts/pn-skew-mi-sim_setting-1.R" 
time Rscript "sim_scripts/pn-skew-mi-sim_setting-1.R"
echo "Running sim_scripts/pn-skew-mi-sim_setting-2.R" 
time Rscript "sim_scripts/pn-skew-mi-sim_setting-2.R"

echo "Done!"
