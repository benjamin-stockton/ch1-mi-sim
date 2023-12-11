#!/usr/bin/env bash 

cd .. 

echo "Running sim_scripts/test-pn-mi-sim-setting-1.R" 
time Rscript "sim_scripts/test-pn-mi-sim-setting-1.R"
echo "Running sim_scripts/test-pn-mi-sim-setting-2.R" 
time Rscript "sim_scripts/test-pn-mi-sim-setting-2.R"

echo "Done!"
