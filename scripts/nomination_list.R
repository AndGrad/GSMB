## create variable for wether a friend is the social source

## load network data
folder <- 'data_experiments/networks/'
full_edge_list <- list()


## go through different datasets and load classes

list_edge_files <- list.files(path = folder)

for (t in 1: length(list_edge_files)){
  this_list <- list_edge_files[t]
  edge_list <- read.table(paste0(folder, this_list), header = TRUE)
  edge_list$classNr <-  as.numeric(substr(as.character(edge_list$IDself[1]), 1, 3))
  full_edge_list <- c(full_edge_list, list(edge_list) )
}

names(full_edge_list) <- sapply(full_edge_list, function(df) as.character(df$classNr[1]))

full_edge_list[1]

## unique classroom IDs
classroom_IDs <- unique(all_data$classNr)

## empty list
friend_lists <- list()

## loop through classroom IDs
for (x in classroom_IDs) {
  
  ## create a friend list for a classroom
  friend_list <- full_edge_list[[as.character(x)]][,1:2] %>%
    group_by(IDself) %>%
    summarise(friends = paste(IDother, collapse = ","))
  
  ## store it in the list
  friend_lists[[as.numeric(x)]] <- friend_list
}

## append the list to the rest of the classrooms
all_friend_lists <- bind_rows(friend_lists)

## filter only ppts who completed the task
all_friend_lists <- bind_rows(friend_lists) %>% 
  filter(IDself %in% all_data$IDself)
