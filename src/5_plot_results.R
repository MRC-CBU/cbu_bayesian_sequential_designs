# This script will take the preprocessed data and for each unique combination of
# factors that were simulated, it will produce two plots:

# 1. A "Power" plot: probabilities of supporting H1/H0 or undecided
# 2. Mean/median number of participants needed to reach support for H1/H0 or undecided

plot_results = function(folderName){
        # Load libraries, etc ###############################
        pacman::p_load(tidyverse,
                       rio)
        
        # Load the data ################################################################
        
        # Load the file
        
        # This must correspond to the variable given to the previous scripts
        if (missing(folderName)){
                folderName <- 'results_1'                
        }       
        
        power_table <- import(file.path('./analysis_results',
                         folderName,
                         'power_table.RData'))
        
        # How many unique combination of factors are here? 
        # For each, make a separate plot
        unique_combs <-
                power_table %>%
                select(minN,
                       d,
                       crit1,
                       crit2,
                       batchSize,
                       limit,
                       test_type,
                       side_type) %>% 
                distinct()
                
        n_combs <- nrow(unique_combs)        
        
        print(paste('There are ', 
                    n_combs, 
                    ' unique combination of factors. They are:',
                    sep=''))
        print(unique_combs)        
        
        # Optionally, filter the unique_combs. Sometimes, there are too many
        # combinations while you're only interested in plotting some of them
        
        # print('WARNING: you are filtering the results and only looking at some of the simulations')
        # unique_combs <- unique_combs %>%
        #         filter(crit1 == 10,
        #                crit2 == 1/6,
        #                test_type == 'paired',
        #                side_type == 'two_tailed') %>%
        #         droplevels()
        # n_combs <- nrow(unique_combs)
        
        # Create the plot #############################################################
        
        # For each of the simualtions, make a separate plot.
        # The user can change this part of the code to plot the data whichever way they 
        # want
        
        
        # Turn it into a long form version. Works better with ggplot
        power_table_long <- 
                power_table %>%
                select(-starts_with('n_sim')) %>%
                pivot_longer(cols = c(perc_simulations_supports_H1,
                                      perc_simulations_supports_H0,
                                      perc_simulations_supports_undecided),
                             names_to = 'bf_status',
                             values_to = 'perc_simulations')
        
        # x tick marks?
        x_ticks <- seq(power_table$minN[1],power_table$limit[1],power_table$batchSize[1])
        
        for (iComb in seq(1,n_combs)){
                
                print(unique_combs[iComb,])
                
                title_string <- paste(
                        'Power curves for the following simulation:',
                        '\n',
                        'd = ',unique_combs$d[iComb],
                        '; crit1 = ',round(unique_combs$crit1[iComb],4),
                        '; crit2 = ',round(unique_combs$crit2[iComb],4),
                        '\n',
                        'minN = ',unique_combs$minN[iComb],
                        '; batchSize = ',unique_combs$batchSize[iComb],
                        '; limit = ',unique_combs$limit[iComb],
                        '\n',
                        'test_type = ',unique_combs$test_type[iComb],
                        '; side_type = ',unique_combs$side_type[iComb],
                        sep=''
                )
                
                fig <- power_table_long %>%
                        filter(d == unique_combs$d[iComb],
                               minN == unique_combs$minN[iComb],
                               crit1 == unique_combs$crit1[iComb],
                               crit2 == unique_combs$crit2[iComb],
                               batchSize == unique_combs$batchSize[iComb],
                               limit == unique_combs$limit[iComb],
                               test_type == unique_combs$test_type[iComb],
                               side_type == unique_combs$side_type[iComb]) %>%
                        ggplot(aes(x=altMaxN,
                                   y=perc_simulations,
                                   group=bf_status,
                                   color=bf_status)) +
                        geom_line() +
                        geom_point() +
                        scale_x_continuous(breaks=x_ticks) +
                        scale_y_continuous(breaks=seq(0,100,10)) +  
                        theme(axis.text.x = element_text(angle = 90)) + 
                        ylab('% of simulations') +
                        xlab('max N per group') +                 
                        ggtitle(title_string)
        
                print(fig)
                
                ## Also plot power by mean/median n to run
                fig2 <- power_table_long %>%
                        filter(d == unique_combs$d[iComb],
                               minN == unique_combs$minN[iComb],
                               crit1 == unique_combs$crit1[iComb],
                               crit2 == unique_combs$crit2[iComb],
                               batchSize == unique_combs$batchSize[iComb],
                               limit == unique_combs$limit[iComb],
                               test_type == unique_combs$test_type[iComb],
                               side_type == unique_combs$side_type[iComb]) %>%
                        ggplot(aes(x=perc_simulations,
                                   y=mean_n)) +
                        geom_line(aes(color='mean')) +
                        geom_point(color='blue') +
                        geom_line(aes(y=median_n,color='median')) +
                        geom_point(aes(y=median_n),
                                   color='orange') +
                        scale_x_continuous(breaks=seq(0,100,10)) +
                        scale_colour_manual("", 
                                            values = c("mean"="blue", 
                                                       "median"="orange")) + 
                        ylab('Number of participants') +
                        xlab('"Power"') +
                        facet_wrap(~bf_status,
                                   labeller = labeller(
                                           bf_status = c("perc_simulations_supports_H0" = '"Power" for H0',
                                                         "perc_simulations_supports_H1" = '"Power" for H1',
                                                         "perc_simulations_supports_undecided" = '"Power" for undecided'))) + 
                        ggtitle(paste('Mean and median participants to achieve a certain power.\n',
                                      title_string,
                                      sep=''))
                
                print(fig2)
        
        }

        return(power_table)
}