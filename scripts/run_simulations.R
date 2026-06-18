#################################################################
##                       Run simulations                       ##
#################################################################

## load packages
pacman::p_load(tidyverse, 
               igraph,
               here,
               DT,
               doParallel,
               parallel,
               foreach,
               ggrain,
               ggthemes,
               gghalves,
               tidybayes)

## function for later
sample.vec <- function(x, ...) x[sample(length(x), ...)]

## load data
folder <- 'data_experiments/networks/'
full_edge_list <- list()

## load functions to perform simulations
source('scripts/functions/simulate_spread_1_infector_fun_with_full_network.R')

## Load the networks
## go through different datasets and load classes

list_edge_files <- list.files(path = folder)
  
for (t in 1: length(list_edge_files)){
    this_list <- list_edge_files[t]
    edge_list <- read.table(paste0(folder, this_list), header = TRUE)
    edge_list$classNr <-  as.numeric(substr(as.character(edge_list$IDself[1]), 1, 3))
    full_edge_list <- c(full_edge_list, list(edge_list) )
    }

## Simulation loop 1: High vs Low

## how many networks
length(full_edge_list)

## how many treatments
treatments <- data.frame(treatment = c("no_structure_no_status", "structure_no_status", "no_structure_status", "structure_status"),
                         p_copy_high = c(.5, .5, .54, .54),
                         p_copy_low =  c(.5, .5, .46, .46)
)

## how many runs (in the long term, 1000)
## needs to be even, otherwise treatments are not split equally
sim_runs <- 3000

## set seed
set.seed(1818)

## how many cores
cl <- makePSOCKcluster(detectCores() - 1)

## register cores, adapt to the machine where the code is running
registerDoParallel(cores = cl)

## start parallel loop
simulations_data_list <- foreach(
  net = unique(full_edge_list), #for each network
  .packages = c("dplyr")
) %dopar% {
  ## function to run simulation
  simulate_diffusion(net = net, sim_runs = sim_runs, treatments = treatments)
}

## save final output
save(list = c("simulations_data_list"), file = "simulations/sim_result_3000_1_infector_4treatments.rda")