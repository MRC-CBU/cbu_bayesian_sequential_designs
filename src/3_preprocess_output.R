# This script will:

# Preprocess the output of slurm and create a nice dataframe with each row 
# being a step taken during the simulations, i.e. another batchSize and the 
# resulting bayes factor.

# The created dataframe can then be used to apply the desired analysis to get 
# certain summary statistics.

preprocess_output = function(saveDF,folder){
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
        
        # The new folder where to save the preprocessed data
        saveFolder <- file.path('./analysis_results',
                                folder)
        
        # Name of the preprocessed data file.
        saveName <- paste(saveFolder,
                          '/sims_preprocessed.RData',
                          sep = '')
        
        # If the save directory doesn't exist, create it
        ifelse(!dir.exists(saveFolder), 
               dir.create(saveFolder, recursive = T), 'Save directory already exists!')
        
        # Name of the file that has the simulation output
        filename <- paste('./data/',
                          folder,
                          '/results_0.RDS',
                          sep = '')
                
        # Import the file and concatenate #############################################
        tempList <- import(filename)
        
        sims_preprocessed <- rbindlist(tempList, idcol = 'id')
        
        # Save the file #####################################################
        if (saveDF){
                save(sims_preprocessed, file = saveName)
        }        
        
        return(sims_preprocessed)
}