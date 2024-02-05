#!/usr/bin/env bash

echo "Running CCA sims"

# echo "High Concentration"
# Rscript cca_simulation_script.R 1000 "PN High Conc"
# echo "Low Concentration"
# Rscript cca_simulation_script.R 1000 "PN Low Conc"
echo "Bi-modal"
Rscript cca_simulation_script.R 1000 "PN Bi modal"
# echo "PN Skewed"
# Rscript cca_simulation_script.R 1000 "PN Skewed"
# echo "PN Reg"
# Rscript cca_simulation_script.R 1000 "PN Reg"
# echo "WN Reg"
# Rscript cca_simulation_script.R 1000 "WN Reg"
# echo "vM Reg"
# Rscript cca_simulation_script.R 1000 "vM Reg"
