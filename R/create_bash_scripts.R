create_bash_scripts <- function(sim_file_path, script_base_name, sh_name, setting) {
    bash_script_content <- 
    "#!/bin/bash \n#SBATCH --partition=general\n#SBATCH --constraint='epyc128'\n#SBATCH --cpus-per-task=50\n#SBATCH --ntasks=1\n#SBATCH --nodes=1\n#SBATCH --mail-type=ALL\n#SBATCH --mail-user=benjamin.stockton@uconn.edu\ncd .. \n\nsource /etc/profile.d/modules.sh\nmodule purge\nmodule load r/4.3.2\n"

    for (i in 1:length(setting$set_n)) {
        sc <- setting$set_n[i]
        
        script_name <- file.path(sim_file_path,
                                 sprintf(script_base_name,
                                         sc)
        )
        new_lines <- paste0('echo "Running ', script_name, '" \ntime Rscript "', script_name, '"\n')
        bash_script_content <- paste0(bash_script_content, new_lines)
    }
    
    bash_script_content <- paste0(bash_script_content, '\necho "Done!"')
    
    sh_name <- file.path("bash", sh_name)
    
    writeLines(bash_script_content, sh_name)
    
    print(paste0("Script at: ", sh_name))
}

