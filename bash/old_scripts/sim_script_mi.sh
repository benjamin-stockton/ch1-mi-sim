#!/usr/bin/env bash

echo "Running MI sims"

# echo "Test"
# Rscript pn_mi_simulation_script.R 15 "Test"
# echo "High Concentration"
# Rscript pn_mi_simulation_script.R 1000 "PN High Conc"
echo "Low Concentration"
Rscript pn_mi_simulation_script.R 250 "PN Low Conc"
# echo "Bi-modal"
# Rscript pn_mi_simulation_script.R 1000 "PN Bi modal"
# echo "PN Skewed"
# Rscript pn_mi_simulation_script.R 1000 "PN Skewed"
# echo "PN Reg"
# Rscript pn_mi_simulation_script.R 1000 "PN Reg"
# echo "WN Reg"
# Rscript pn_mi_simulation_script.R 1000 "WN Reg"
# echo "vM Reg"
# Rscript pn_mi_simulation_script.R 1000 "vM Reg"
