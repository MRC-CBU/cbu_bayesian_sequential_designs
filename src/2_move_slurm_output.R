# Description ############################################################

# This script will move the folder that slurm created in the working directory.
# This is done so that the working directory doesn't get cluttered.

move_slurm_output = function(saveDF,folder){
        # Libraries #####################################################
        pacman::p_load(data.table,
                       tidyverse,
                       rio)
        
        # Flags, folder names, etc #####################################################
        
        # Flag to save the data
        if (missing(saveDF)){
                saveDF <- TRUE
        }
        # This name should correspond to the name used in 1_simulation_parameters.R script
        # So that the correct files are loaded and preprocessed.
        if (missing(folder)){
                folder <- 'results_1'
        }
        
        # Name of the file that has the simulation output
        filename <- paste('./data/',
                          folder,
                          '/results_0.RDS',
                          sep = '')
        
        # Move the slurm output ################################################
        # from the working directory to the ./data dir
        # This is done so that the working directory isn't cluttered with slurm output
        
        old_path <- paste('./_rslurm_',folder,sep = '')
        new_path <- file.path('./data',folder)
        
        # Create the new path if it doesn't exist
        ifelse(!dir.exists(new_path),
               dir.create(new_path, recursive = T),
               '')
        
        # Get the list of files
        current_files = list.files(old_path, full.names = TRUE)
        
        # Check that results_0.RDS is part of the list of files! If its not, 
        # then slurm isn't done with the simulations. Alert the user to wait.
        if (file.path(old_path,'results_0.RDS') %in% current_files){
        
                print('Moving the files...')
                
                # Copy them
                file.copy(from = current_files, 
                          to = new_path, 
                          overwrite = TRUE, 
                          recursive = FALSE, 
                          copy.mode = TRUE,
                          copy.date = TRUE)
                
                # Now delete the original data
                unlink(old_path, recursive = TRUE)
                
        } else {
                stop(paste('\nCould not find the results_0.RDS file.\n',
                'This probably means that the simulations are not yet finished.\n', 
                'Or they have already been moved to the ./data folder',
                sep=''))
                
        }
}
