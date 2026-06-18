##################################################################
##                    Load data and packages                    ##
##################################################################
## load packages 
source("scripts/functions/check_pkgs.R")

## load complete dataset
all_data <- read.csv(file = 'data_experiments/full_dataset_session2.csv') %>% 
  mutate(follow = ifelse(choice == social_info, 1, 0),
         social_info_factor = factor(social_info),
         age_scaled = scale(age, scale = FALSE))  ## center but not standardize 
       
source("scripts/nomination_list.R")

## merge nominations with the behavioural dataset
all_data <- all_data %>%
  left_join(all_friend_lists, by = "IDself")

## create a variable to check if IDother is a friend
all_data <- 
  all_data |> 
  rowwise() |> 
  mutate(peerisfriend = as.integer(IDother %in% as.numeric(strsplit(friends, ",")[[1]]))) |> 
  mutate(peerisfriend = factor(peerisfriend)) |> 
  ungroup()

## collect all IDs from wave 1
unique_ids <- unique(unlist(lapply(full_edge_list, function(x) x$IDself)))

## identify participants who only took part in wave 2
only_wave2_ppts <- unique(all_data$IDself)[!unique(all_data$IDself) %in% unique_ids]

all_data <- all_data |> 
dplyr::filter(!IDself %in% only_wave2_ppts) ## exclude wave 2 ppts who were not in wave 1

## subset experimental treatments
all_data_treatment <- all_data %>% 
  dplyr::filter(treatment != 2 & treatment != 4 & treatment != 5 ) %>% 
  mutate(follow = ifelse(choice == social_info, 1, 0),
         social_info_factor = factor(social_info))

## control condition: social info == choice wave 1
all_data_control <- all_data %>% 
  dplyr::filter(treatment == 2)

## part 2, only added in 2020 and 2022 waves
all_data_intervention <- all_data %>% 
  dplyr::filter(treatment > 3)

## load data of session 1
data_donations <- read.csv("data_experiments/individual_donations_session1.csv")

