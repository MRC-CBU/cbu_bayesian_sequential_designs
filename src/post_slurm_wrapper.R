# Description #################################################################

# This is a wrapper code for running steps 2-5 in one go. Make sure to change 
# the nFrom, nTo, nBy, and "folder" variables to approapriate values corresponding
# to which simulation you want to preprocess and plot.


# Clean the environment and source the subfunctions ###########################
rm(list=ls())

# Source the functions
source('./src/2_move_slurm_output.R')
source('./src/3_preprocess_output.R')
source('./src/4_summary_stats_many_altNs.R')
source('./src/5_plot_results.R')

# Define parameters ###########################################################
saveData <- TRUE

# For the summary stats
nFrom <- 20
nTo   <- 100
nBy   <- 10

# Folder where the slurm output is
folder <- 'results_interview'

# Now, call each function ######################################################

## 2_move_slurm_output --------------------------------------------------------
move_slurm_output(saveData,folder)

## 3_preprocess_output --------------------------------------------------------
sims_preprocessed <- preprocess_output(saveData,folder)

## 4_summary_stats_many_altNs -------------------------------------------------
power_table <- summary_stats(saveData,nFrom,nTo,nBy,folder)

## 5_plot_results -------------------------------------------------------------
plot_results(folder)