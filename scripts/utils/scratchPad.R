# Save 

rm(list=ls())

library(tidyverse)
library(rio)

load("C:/Users/levan/GitHub/bayesian_sequential_design_simulations/analysis_results/results_1/sims_preprocessed.RData")
load("C:/Users/levan/GitHub/bayesian_sequential_design_simulations/analysis_results/results_1/power_table.RData")


d_filt <- c(0,0.5)
crit1_filt <- 10
crit2_filt <- 1/6
test_type_filt <- c('paired','unpaired')
side_type_filt <- 'two_tailed'

sims_preprocessed <- sims_preprocessed %>%
        filter(d %in% d_filt,
               crit1 == crit1_filt,
               crit2 == crit2_filt,
               test_type %in% test_type_filt,
               side_type == side_type_filt)

power_table <- power_table %>%
        filter(d %in% d_filt,
               crit1 == crit1_filt,
               crit2 == crit2_filt,
               test_type %in% test_type_filt,
               side_type == side_type_filt) %>%
        arrange(altMaxN)

# Save these 
save(sims_preprocessed,
     file = './analysis_results/results_mmm_2_large/sims_preprocessed.RData')
save(power_table,
     file = './analysis_results/results_mmm_2_large/power_table.RData')










# rm(list=ls())
# 
# pacman::p_load(tidyverse,
#                rio)
# 
# 
# 
# # Load results 1
# res1 <- import('./analysis_results/results_1/power_table.RData')
# 
# 
# # Load results 2
# res2 <- import('./analysis_results/results_2/power_table.RData')
# 
# mg <- merge(res1,res2,
#             by = c('minN',
#                    'd',
#                    'crit1',
#                    'crit2',
#                    'limit',
#                    'test_type',
#                    'side_type',
#                    'altMaxN',
#                    'batchSize'))
# 
# 
# # Calc the differences
# mg$h0diff <- mg$n_simulations_supports_H0.x - mg$n_simulations_supports_H0.y
# mg$h1diff <- mg$n_simulations_supports_H1.x - mg$n_simulations_supports_H1.y
# mg$unddiff <- mg$n_simulations_supports_undecided.x - mg$n_simulations_supports_undecided.y
# 
# mg$mean_n_diff <- mg$mean_n.x - mg$mean_n.y
# mg$median_n_diff <- mg$median_n.x - mg$median_n.y
# 
# 
# 
# # Plot
# fig <- mg %>%
#         ggplot(aes(x=altMaxN)) +
#         geom_line(aes(y=h0diff),color='red') +
#         geom_line(aes(y=h1diff),color='blue') +
#         geom_line(aes(y=unddiff),color='black') +
#         facet_grid(d+crit1~test_type+crit2+side_type)
# 
# print(fig)

