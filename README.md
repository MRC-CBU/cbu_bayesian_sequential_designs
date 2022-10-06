# About: 

For theoretical and practical guide to using this codebase, please see the following webinar (internal to the CBU):
- [10/01/2022, Bayesian Sequential Designs (Rik Henson & Levan Bokeria)](http://intranet.mrc-cbu.cam.ac.uk/home/mmm-talks/)

For a quick video introduction to Bayesian sequential designs see: [Alexander Quent: "Plan to produce strong evidence: Bayesian Sequential Designs"](https://www.youtube.com/watch?v=FGaxLQ_o8D4)

For background literature on Bayesian inference and sequential designs in neuroscience and psychology, see: 

- [Schönbrodt et al., 2017:  Sequential hypothesis testing with Bayes factors: Efficiently testing mean differences]( 10.1037/met0000061)
- [Keysers et al., 2020: Using Bayes factor hypothesis testing in neuroscience to establish evidence of absence](https://doi.org/10.1038/s41593-020-0660-4)


# Folder structure: 

## data:
A folder where the automatic output of simulations should be moved to. The output data from local simulations or slurm gets dumped in the project folder. Use the _2_move_slurm_output.R_ script to automatically move them to ./data

## scripts:
Folder for all the scripts. Has a ./utils subfolder that contains some helper functions for various purposes.

## analysis_output:
A folder to store preprocessed data and other stats results from various analysis. Contains an example analyzed output in the "results_1" folder, where we simulated all the combination of the following factors: d=0 or d=0.5, minN=24, batchSize=12, maxN=600, BF10 criteria of 6 or 10, BF01 criteria of 6 or 10, one-tailed and two-tailed, paired and unpaired t-test.

# How to run simulations:

## Downloading the codebase:

Please fork the repository to your own account to keep track of usage and update suggestions. Alternatively, you can download a standalone .zip file of the full codebase. 

## Before running the simulations:

First, please open the .Rproj file. Having the project started makes sure the working directory is set correctly.

Then, please make sure Rstudio packages are up-to-date. Go to "Tools" and "Check for Package Updates."

Please make sure to install the "pacman" package using install.packages('pacman').

## The main sequence of steps:

_1_simulation_parameters.R_: specify various types of simulations you'd like to run. You can either run on the cluster or locally.

To run on the cluster, set the submitJob flag to TRUE and simLocal flag to FALSE in this script. The R script will automatically create a batch job and submit it to the slurm job manager on the cluster. No additional steps are needed, you don't need to write any shell scripts yourself. Once slurm is done, it will dump the results in a new folder in the main working directory. The main file containing results is the results_0.RDS.

The rest of the scripts can be run on your local machine, after you have synced the simulation results from the cluster to your local machine. Running these scripts on the login nodes will generally be slower. 

_2_move_slurm_output.R_: slurm will dump the results in the main working directory. To avoid cluttering the space, this script will move the specified folder to the ./data folder. Subsequent scripts will look for data there. 

_3_preprocess_output.R_: slurm outputs the results in a list format. This script will bind it into a nice dataframe, and save the dataframe into specified subfolder of the ./analysis_results/ folder. This dataframe will have each row represent one "step" in every simulation, i.e. one addition of a batch of participants and checking of the Bayes factor.

_4_summary_stats_many_altNs.R_: This script takes the dataframe generated by the 3_preprocess_output.R script. It will calculate the probabilities of supporting H1/H0/undecided, depending on various maxN limits.

_5_plot_results.R_: Warning: this script will NOT work at all on login nodes. Perform this step on your local PC. This script will take the power_table.RData file produced by 4_summary_stats_many_altNs.R script. For every unique combination of factors you specified in the initial job, it will produce two plots: (1) percentage of simulations supporting H1/H0/undecided, and (2) mean and median number of participants needed to run to achieve a certain "power".
The code optionally allows you to create these plots only for certain combination of the factors, instead of all of them. Otherwise, sometimes, if there are way too many combinations, you'll get lost in the plethora of plots that are produced.

_6_post_slurm_wrapper.R_: This script is a wrapper to run scripts 2 through 5, so you don't have to run them separately. It cannot run the 1st script, because the 2nd script must wait until slurm is done with the simulations.

# Support:

For any questions please contact:

- Levan Bokeria: levan.bokeria@mrc-cbu.cam.ac.uk
- Rik Henson: rik.henson@mrc-cbu.cam.ac.uk

# Authors and acknowledgment:

This codebase is inspired and based on Alex Quent's [earlier work found here](https://github.com/JAQuent/bayesianSequentialDesign):

This repository was created by Levan Bokeria and is maintained by:

- Levan Bokeria
- Alex Quent
- Rik Henson
- Andrea Greve

# License: 

This project is licensed under the terms of the MIT license. See LICENSE.md for details.