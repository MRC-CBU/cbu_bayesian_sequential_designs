# Description:

# This is the main script where you setup simulation parameters, and they get
# passed to slurm to perform fast computation.

# You can also run the simulations locally, without submitting to slurm. But 
# that will be much, much slower. Its advised to do this only for small jobs.


# Global parameters and libraries #######################################

# Clear the environment
rm(list=ls())

# Setting seed
set.seed(789654)

# Libraries
pacman::p_load(rslurm,
               BayesFactor,
               tidyverse)

# Setup simulation parameters and flags #######################################

# Slurm job parameters
n_nodes       <- 1
cpus_per_node <- 16
nIter         <- 100

# Sequential design parameters. 
# For d, crit1, and crit2 you can enter a vector of numbers.

nLimit    <- 100 # maximum number of participants to run
d         <- c(0.3) # various effect sizes to consider
crit1     <- c(6) # criteria for stopping for BF10
crit2     <- c(1/6) # criteria for stopping for BF01
minN      <- 20 # Initial minimum number of participants per group
batchSize <- 10 # How many participants to add per group when neither of the criteria are reached.

# Note: if various batchSizes are simulated the post-processing scripts
# might not work.

# What type of test is it?
test_types <- c('paired')
side_types <- c('one_tailed')

# Name for saving folder
saveFolder <- 'results_interview'

# Submit the slurm job?
submitJob <- FALSE

# Simulate locally? This will take much longer for large jobs
simLocal <- TRUE

# Define the function ########################################################
# This function will be applied to specified parameters many times by slurm.

helperfunction <- function(minN, d, crit1, crit2, batchSize, limit, 
                           test_type, side_type){
        
        # Subfunction to efficiently report the BF
        # From https://github.com/JAQuent/assortedRFunctions/R/reportBF.R
        reportBF = function(x, digits){
                round(as.numeric(as.vector(x)), digits)
        }
        
        bf      <- c()
        results <- data.frame()
        i       <- 1
        n       <- as.numeric(minN)        
        
        # Is this one-sided or two sided
        if (side_type == 'two_tailed'){
                null_interval <- NULL
        } else if (side_type == 'one_tailed'){
                null_interval <- c(0,Inf)
        }
        
        # Is this paired or unpaired?
        if (test_type == 'unpaired'){
                
                dataG1 <- rnorm(n, 0, 1)
                dataG2 <- rnorm(n, d, 1)

                # Calculate the initial bf
                bf <- reportBF(ttestBF(
                        dataG2,
                        dataG1,
                        nullInterval = null_interval
                )[1],4)
                
        } else if (test_type == 'paired'){
                
                dataG1 <- rnorm(n,d,1)
                
                # Calculate the initial bf
                bf <- reportBF(ttestBF(
                        dataG1,
                        nullInterval = null_interval
                )[1],4)                
                
        }
        
        # Within simulation loop
        while(bf[length(bf)] < crit1 & bf[length(bf)] > crit2 & n < limit){
                
                # If neither of the criteria is reached and the limit isn't reached
                # increase the n by batchsize, generate additional data, and check 
                # the BFs again
                n <- n + batchSize
                
                
                if (test_type == 'unpaired'){
                        
                        dataG1      <- c(dataG1, rnorm(batchSize, 0, 1))
                        dataG2      <- c(dataG2, rnorm(batchSize, d, 1))
                        
                        bf[i + 1] <- reportBF(ttestBF(
                                dataG2, 
                                dataG1,
                                nullInterval = null_interval
                                )[1],4)
                        
                } else if (test_type == 'paired'){
                        
                        dataG1    <- c(dataG1, rnorm(batchSize, d, 1))
                        
                        bf[i + 1] <- reportBF(ttestBF(
                                dataG1,
                                nullInterval = null_interval
                                )[1],4)
                }
                
                i <- i + 1
        }

        results <- as.data.frame(bf)  
        
        # Return results
        results$minN      <- minN
        results$d         <- d
        results$n         <- seq(minN,n,batchSize)
        results$crit1     <- crit1
        results$crit2     <- crit2
        results$batchSize <- batchSize
        results$limit     <- limit
        results$test_type <- test_type
        results$side_type <- side_type
        return(results)
}

# Create parameters #########################################################
# slurm will iterate over these with the helperfunction


# First, create all the combinations
cart_prod <- expand.grid(minN,
                     d,
                     crit1,
                     crit2,
                     batchSize,
                     nLimit,
                     test_types,
                     side_types)
names(cart_prod) <- c('minN','d','crit1','crit2','batchSize','limit',
                      'test_type','side_type')

# Now, repeat each of these combinations nIter times
params <- cart_prod %>% 
        slice(rep(1:n(), each=nIter)) %>%
        mutate(across(c('test_type','side_type'),as.character))

# Run the simulation ##########################################################


## Try locally -------------------------------------------------------------
if (simLocal){
        print('Simulating locally...')
        
        results <- do.call(Map, c(f=helperfunction,params))
        
        # If the save directory doesn't exist, create it
        ifelse(!dir.exists(paste('./_rslurm_',saveFolder,sep='')),
               dir.create(paste('./_rslurm_',saveFolder,sep=''), recursive = T),
               'Save directory already exists!')
        
        
        saveRDS(results, file = paste(
                './_rslurm_', 
                saveFolder,
                '/results_0.RDS',sep = ''))
}

## Or try SLURM  ------------------------------------------------------------

# Create job
if (submitJob){
        print('Submitting to the cluster...\n')
        
        sjob1 <- slurm_apply(helperfunction,
                             params, 
                             jobname = saveFolder,
                             nodes = n_nodes, 
                             cpus_per_node = cpus_per_node, 
                             submit = submitJob)        
}


