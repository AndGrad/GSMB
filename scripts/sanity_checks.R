#################################################################
##                        Sanity Checks                        ##
#################################################################

## load environment
source("scripts/load_environment.R")

## plot in-degree of the observed peers in the experimental conditions as they are defined in the experiment
all_data_treatment %>% 
  filter(!is.na(status_social_info)) %>% 
  ggplot( aes(x = stdinDegreepeer, fill = status_social_info)) +
  
  geom_histogram(color = "black",
                 position = "dodge",
                 binwidth = .20) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(x =
         "In-degree", y = "Count") +
  theme(text = element_text(size = 20)) +
  scale_fill_viridis_d(option = "D", name = 'Peer status', labels = c('High', 'Low')) 

ggsave("plots/treatment_separation.png", height = 4)


## plot choices in the control condition

## summary of the choices
all_data_control %>%
  group_by(factor(follow), wave) %>%
  reframe(count = n(),
          follow = follow) %>% 
  distinct() %>% 
  group_by(wave) %>%
  reframe(perc = count / sum(count),
          follow = factor(follow)) %>% 
  group_by(follow) %>% 
  mutate(mean_across_waves = mean(perc))

## plot raw count of conformity choices in the control condition
ggplot(data = all_data_control) +
  geom_bar(aes(x = factor(follow))) +
  facet_grid(~ wave) +
  labs(title = "Count of conform (1) vs ignore (0) in control condition.")

## make a plot
all_data_control %>%
  group_by(IDself) %>%
  reframe(mean_follow = mean(follow),
          wave = wave) %>%
  ggplot(aes(
    x = factor(wave),
    y = mean_follow,
    fill = factor(wave)
  )) +
  geom_half_boxplot() +
  geom_half_point() +
  stat_summary() +
  labs(
    x = "Study year", y = "Consistency with wave 1") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  scale_fill_viridis_d(option = "D", name = 'year') 

ggsave("plots/control_condition.png", height = 4)
