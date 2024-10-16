###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###      SIMLATE SPREAD OF TRAIT THROUGH NETWORK, 1 INFECTOR MODEL      ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------

## function: parameters are:
# net = network to simulate over
# number of runs

simulate_diffusion <- function(net = NULL, sim_runs = 10, p_copy_high = .5, p_copy_low = .5, age_diff = FALSE, treatments = treatments) {
  sim_dat_all <- list()
  
  for (p in 1:nrow(treatments)){
    
    ## load edge table
    net <- net %>% 
      filter(!if_any(everything(), is.na))  
    
      ## remove NAs
    classNr <- net$classNr[1]
    
    ## create directed network
    network <- graph_from_data_frame(d = net, directed = "TRUE") #directed
    
    ## remove participants who did not fill nominations (number starting with 88)
    IDs <- as.numeric(V(network)$name)
    toRemove <- which(IDs > as.numeric(200) * classNr)
    network <- delete_vertices(network, toRemove)
    network <- simplify(network) ## remove self-nominations
    
    ## is there at least an outgoing connection for each node?
    ## we need this so that people can change their mind.
    
    ## until each node in the netwrok has outdegree >= 1
    while (any(degree(network, mode = "out") < 1)){
      
      ## remove nodes with outdegree < 1
      unconnected <- which(degree(network, mode = "out") < 1)
      network <- delete_vertices(network, unconnected)
    }
    
    ## in the treatments without structure, make the network fully connected
    
    if (treatments$treatment[p] != 'structure_no_status' & treatments$treatment[p] != 'structure_status' ) {
      
      ## create fully connected, directed network
      network <- make_full_graph(length(V(network)$name), directed = TRUE, loops = FALSE)
    }
    
    ## the above process can result in disconnected components.
    ## only proceed to simulation if there is 1 main component, 
    ## otherwise, behavior also cannot spread
    
    if (components(network)$no < 2) {
      
      ## network size 
      n_nodes <- gorder(network)
      
      ## replace unique IDs with numbers 1:N
      V(network)$name <- c(1:n_nodes)
      
      ## if there is a structure, look into it and identify nodes with high or low indegree
      
      if (treatments$treatment[p] == 'structure_no_status' | treatments$treatment[p] == 'structure_status' ) {
        
      ## calculate in-degree of each node
      inDegree = degree(network, mode = "in")
      degreeDF <- data.frame(inDegree)
      V(network)$indeg <- inDegree # label vertices in the network object
      
      ## split the network in 2 by degree @andrea: does this do anything?
      mediandeg <- median(degreeDF$inDegree)
      degreeDF$rank <-
        cut(degreeDF$inDegree, c(-Inf, mediandeg, +Inf), c("L", "H")) ##add rank
      
      ## Rank the nodes by indegree
      indegrees <- degree(network, mode = "in")
      ranked_nodes <- order(indegrees, decreasing = TRUE)
      
      ## Split the nodes by median indegree
      median_indegree <- median(indegrees)
      high_status_nodes <- ranked_nodes[1:median_indegree]
      low_status_nodes <- ranked_nodes[(median_indegree + 1):n_nodes]
      
      } else {
        
        ## just split network in two halfes with no indegree differences
        high_status_nodes <- 1:round(n_nodes/2)
        low_status_nodes <- (round(n_nodes/2) + 1): n_nodes
        
      }
      
      ## create empty dataframe to fill during simulation
      sim_dat <- data.frame(classNr = rep(classNr, sim_runs),
                            size = numeric(sim_runs),
                            indegree = numeric(sim_runs),
                            status = character(sim_runs),
                            origin_node = numeric(sim_runs),
                            iter = numeric(sim_runs),
                            t = numeric(sim_runs),
                            result = numeric(sim_runs),
                            node = numeric(sim_runs), 
                            selected_node = numeric(sim_runs),
                            initial_p = numeric(sim_runs),
                            dynamic = numeric(sim_runs),
                            net_size = rep(n_nodes, sim_runs),
                            n_runs = rep(sim_runs, sim_runs),
                            treatment = character(sim_runs))
      
      
      ## loop through n of simulations to run
      for (t in 1:sim_runs){
        
        ## Initialize the node states (A or B): everyone is A, one is B
        node_states <- rep(0, n_nodes)
        
        ## half simulations with high status as seed - half with low status as seed
        if (t<=sim_runs/2) {
          
          ## give B to one of top 2 status individuals 
          origin_node_index <-  sample(1:2, 1)
          node_states[high_status_nodes[origin_node_index]]  <- 1
          sim_dat$size[t] <- n_nodes
          sim_dat$status[t] <- "High"
          sim_dat$origin_node[t] <- high_status_nodes[origin_node_index]
          
          if (treatments$treatment[p] == 'structure_no_status' | treatments$treatment[p] == 'structure_status' ) {
            sim_dat$indegree[t] <-  degreeDF$inDegree[high_status_nodes[origin_node_index]] }
          else {
            sim_dat$indegree[t] <- n_nodes
          }
          
          } else {
          
          # sample nodes at end of low status distribution
          length(low_status_nodes)    
          to_sample <- c(
            length(low_status_nodes) - 2,
            length(low_status_nodes) - 1
          )
          
          # give B to one of bottom 2 low status individuals
          
          origin_node_index <-  sample(to_sample, 1)
          node_states[low_status_nodes[origin_node_index]] <- 1
          sim_dat$size[t] <- n_nodes
          sim_dat$status[t] <- "Low"
          sim_dat$origin_node[t] <- low_status_nodes[origin_node_index]
          
          if (treatments$treatment[p] == 'structure_no_status' | treatments$treatment[p] == 'structure_status' ) {
            sim_dat$indegree[t] <-  degreeDF$inDegree[low_status_nodes[origin_node_index]]
            }
          else {
            sim_dat$indegree[t] <- n_nodes
          }          
        }
        
        ## reset simulation counter
        num_rounds <- 0
        sim_dat$t[t] <- t # save simulation run
        
        ## initial node_states
        prop_b <- length(which(node_states == 1))/n_nodes 
        sim_dat$initial_p[t] <- prop_b
        sim_dat$treatment[t] <-  treatments$treatment[p]
        proportion_B = prop_b
        ## main simulation loop
        #while (length(unique(node_states)) > 1 & num_rounds < 10000) {
        while ((sum(node_states)>0 & sum(node_states)<n_nodes) & num_rounds < 10000) {
          
          ## increment the round count
          num_rounds <- num_rounds + 1
          
          ## select a random node
          selected_node <- sample(V(network), 1)
          
          ## get its neighbors
          in_neighbors <- neighbors(network, selected_node, mode = "out")
          
          # Select a random neighbor
          if (length(in_neighbors) > 0) {
            neighbor <- sample.vec(in_neighbors, 1)
            
            if(num_rounds == 1) {
              sim_dat$node[t] <- neighbor
              sim_dat$selected_node[t] <- selected_node
            }
            
            # determine the probability of being infected based on the neighbor's status
            if (neighbor %in% high_status_nodes ) {
              probability_copy <- treatments$p_copy_high[p]
            } else {
              probability_copy <-  treatments$p_copy_low[p]
            }
            
            ## copy the state of the selected neighbor with the determined probability
            if (runif(1) < probability_copy) {
              node_states[selected_node] <- node_states[neighbor]
            }
            
            ## calculate the proportion of nodes with state 1
            proportion_B <- sum(node_states == 1) / n_nodes
            prop_b <- c(prop_b, proportion_B)
          }
        }
        
        ## print the final state of the network
        cat("\nNetwork", n, "size =", n_nodes, ";simulation", t, "completed in", num_rounds, "rounds.\n")
        
        ## how long did it take  
        sim_dat$iter[t] <- num_rounds
        sim_dat$result[t] <- proportion_B
        #sim_dat$dynamic[t] <- list(prop_b)
        
        
      }}
    
    else{
      
      ## just save empty line
      sim_dat <- data.frame(classNr = classNr,
                            size = NA,
                            indegree = NA,
                            status = NA,
                            origin_node = NA,
                            iter = NA,
                            t = NA,
                            result = NA,
                            node = NA, 
                            selected_node = NA,
                            initial_p = NA,
                            dynamic = NA,
                            net_size = NA,
                            n_runs = NA,
                            treatment = NA)
      
      
    }
    
    ## add to list of networks
    sim_dat_all[[p]] <- sim_dat
    
  }
  
  sim_dat_all
  
}



