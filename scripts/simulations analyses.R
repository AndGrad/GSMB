#################################################################
##                     Analyze simulations                     ##
#################################################################

## load data
base::load("data_simulations/sim_result_3000_1_infector_4treatments.rda")

## create data frame from list
df_sims <- bind_rows(simulations_data_list, .id = "column_label") %>% 
  filter(!if_any(everything(), is.na))  %>% 
  filter(classNr != 103 & classNr!=105) %>% ## remove classes with separate components where diffusion cannot happen
  group_by(classNr) %>% ## group by class to separate network size
  mutate(fixp = 1/net_size, ## theoretical fixation probabily, 1/N
         fixt = 4*net_size) %>% ## theoretical fixation time, 4N
  ungroup()  

## calculate the mean time of fixation in the benchmark treatment with no status and no network structure
benchmark_t <- df_sims %>% 
  group_by(classNr) %>% 
  select(iter, classNr, treatment) %>% 
  filter(treatment == "no_structure_no_status")  %>% 
  summarise(benchmark_t_sims = mean(iter)) 

## calculate the mean time of fixation in the benchmark treatment with no status and no network structure, conditional on the networks reaching fixation
benchmark_t_conditional <- df_sims %>% 
  group_by(classNr) %>% 
  select(iter, classNr, treatment, result) %>% 
  filter(treatment == "no_structure_no_status" & result == 1)  %>% 
  summarise(benchmark_t_sims_conditional = mean(iter)) 

df_sims <- left_join(df_sims, benchmark_t, by = "classNr")
df_sims <- left_join(df_sims, benchmark_t_conditional, by = "classNr")

df_sims[!complete.cases(df_sims), ]

## check that treatment recording worked correctly
df_sims %>% 
  ungroup() %>% 
  select(treatment) %>% 
  distinct()

## calcualte range of networks
range(df_sims$net_size)

## check theoretical fixp by network
df_sims %>% 
  select(classNr, net_size, fixp) %>% 
  distinct() %>% 
  ggplot(aes(x = net_size, y = fixp)) +
  geom_point(aes(color = factor(classNr)))

## check theoretical fixt by network
df_sims %>% 
  select(classNr, net_size, fixt) %>% 
  distinct() %>% 
  ggplot(aes( x = net_size, y = fixt)) +
  geom_point(aes(color = factor(classNr)))

## create outcome variables
results <- df_sims %>% 
  group_by(treatment, status, classNr) %>% 
  mutate(tot_B = sum(result), ## total fixations on B
         tot_A = (n_runs[1]/2) - tot_B,
         perc_B = tot_B/(n_runs[1]/2), ## %success
         S = perc_B/fixp,
         T = iter/benchmark_t_sims,
         T_conditional = iter/benchmark_t_sims_conditional) 

## select network that reached fixation of trait B
fixated_B <- results %>% 
  filter(iter < 10000 & result == 1) %>% 
  ungroup()

#### VISUAL EXPLORATION
results %>% 
  select(fixp, perc_B, status, treatment, classNr) %>% 
  distinct() %>% 
  ggplot(aes(x = fixp, y = perc_B)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~ treatment+status, nrow = 1)

## fix time
results %>% 
  filter(result == 1) %>% 
  select(iter, benchmark_t_sims_conditional, status, treatment, classNr) %>% 
  distinct() %>% 
  ggplot(aes(x = benchmark_t_sims_conditional, y = iter)) +
  stat_summary() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~ treatment+status, nrow = 1)

panel_labels <- c(
  `no_structure_no_status` = "Simulated benchmark",
  `no_structure_status` = "Status bias",
  `structure_no_status` = "Network structure",
  `structure_status` = "Network structure\nand status bias"
)

###---------- Supplement figure: intervention success across treatments---------

## prepare data
plot_fixation_proporion_dataset <- 
  fixated_B %>%
  group_by(classNr) %>% 
  mutate(side = ifelse(status == "High", -1, 1)) %>%
  group_by(status, treatment, classNr) %>%
  reframe(
    S,
    net_size = net_size,
    classNr = as.character(classNr),
  ) %>%
  filter(treatment != "no_structure_no_status") %>% 
  distinct() 

## make plot
ggplot(data = plot_fixation_proporion_dataset, aes(x = status, y = S)) +
  geom_hline(aes(yintercept = 1), color = 'blue') +
  geom_half_boxplot(
    data = plot_fixation_proporion_dataset[which(plot_fixation_proporion_dataset$status == "High"), ],
    nudge = .1,
    width = .6,
    outlier.shape = NA,
    errorbar.length = .2
  ) +
  geom_half_boxplot(
    data = plot_fixation_proporion_dataset[which(plot_fixation_proporion_dataset$status == "Low"), ],
    side = "r",
    nudge = .1,
    width = .6,
    outlier.shape = NA,
    errorbar.length = .2
  ) +
  geom_point(aes(shape = status)) +
  stat_summary(data = plot_fixation_proporion_dataset[which(plot_fixation_proporion_dataset$status == "High"), ],
               shape = 21,
               fill = 'white',
               size = 1.1,
               stroke = 2,
               position = position_nudge(x = -.1)) +
  stat_summary(data = plot_fixation_proporion_dataset[which(plot_fixation_proporion_dataset$status == "Low"), ],
               shape = 24,
               fill = 'white',
               size = 1.1,
               stroke = 2,
               position = position_nudge(x = .1)) +
  geom_line(aes(group = classNr), color = 'grey' ) +
   geom_text(x = 2.5, y = 1, label = "benchmark", color = "blue", size = 4, hjust = 1.2, vjust = -0.2) +
  geom_hline(aes(yintercept = 1), color = 'blue') +
  theme_tidybayes() +
  theme(text = element_text(size = 18)) +
  labs(y = 'Succesful interventions relative to benchmark (S)', 
       x = '"Influencer" status') +
  guides(shape = 'none') +
  facet_wrap( ~ treatment, nrow = 1, labeller = as_labeller(panel_labels))

ggsave('plots/simulations_results_full_supplement.png', height = 6.5, width = 10)



###-------------Supplement figure: fixation time across treatments--------------

## dataset with means
plot_fixation_time_dataset <-
  fixated_B %>%
  group_by(classNr) %>% 
  mutate(side = ifelse(status == "High", -1, 1)) %>%
  group_by(status, treatment, classNr) %>%
  filter(treatment != "no_structure_no_status") %>% 
  select(
    T_conditional,
  ) %>%
  summarise(meanT = mean(T_conditional)) %>% 
  distinct() 

## make plot
ggplot(data = plot_fixation_time_dataset, aes(x = status, y = meanT)) +
  geom_hline(aes(yintercept = 1), color = 'blue') +
  geom_half_boxplot(
    data = plot_fixation_time_dataset[which(plot_fixation_time_dataset$status == "High"), ],
    nudge = .1,
    width = .6,
    outlier.shape = NA,
    errorbar.length = .2
  ) +
  geom_half_boxplot(
    data = plot_fixation_time_dataset[which(plot_fixation_time_dataset$status == "Low"), ],
    side = "r",
    nudge = .1,
    width = .6,
    outlier.shape = NA,
    errorbar.length = .2
  ) +
  geom_point(aes(shape = status)) +
  stat_summary(data = plot_fixation_time_dataset[which(plot_fixation_time_dataset$status == "High"), ],
               aes(x = status, y = meanT),
               shape = 21,
               fill = 'white',
               size = 1.1,
               stroke = 2,
               position = position_nudge(x = -.1)) +
  stat_summary(data = plot_fixation_time_dataset[which(plot_fixation_time_dataset$status == "Low"), ],
               aes(x = status, y = meanT),
               shape = 24,
               fill = 'white',
               size = 1.1,
               stroke = 2,
               position = position_nudge(x = .1)) +
  geom_line(aes(group = classNr), color = 'grey' ) +
  geom_text(x = 2.5, y = 1, label = "benchmark", color = "blue", size = 4, hjust = 1.2, vjust = -0.2) +
  geom_hline(aes(yintercept = 1), color = 'blue') +
  theme_tidybayes() +
  theme(text = element_text(size = 18)) +
  labs(x = '"Influencer" status', 
       y = 'Intervention duration relative to benchmark (T)') +
  guides(shape = 'none') +
  facet_wrap( ~ treatment, nrow=1, labeller = as_labeller(panel_labels))

ggsave('plots/simulations_results_fixation_time_full_supplement.png', height = 6.5, width = 10)

## summary of results across conditions
fixated_B %>% 
  group_by(treatment, status, classNr) %>% 
  summarise(meanS = mean(S)) %>% 
  ungroup () %>% 
  group_by(treatment, status) %>% 
  summarise(mean_S = mean(meanS),
            sd = sd(meanS))

mean(plot_fixation_proporion_dataset$S)

## summary of percentage fixated
results %>% 
  select(S, treatment, status, classNr, perc_B) %>% 
  group_by(treatment, status) %>% 
  summarise(meanS = mean(S),
            perc_B = mean(perc_B)*100)

## summary of fixation time
results %>% 
  filter(result == 1) %>% 
  select(iter, treatment, status, classNr, T_conditional, benchmark_t_sims_conditional) %>% 
  group_by(treatment, status) %>% 
  summarise(meaniter = mean(iter),
            meanT = mean(T_conditional), 
            benchmark_t_sims_conditional = mean(benchmark_t_sims_conditional)) %>% 
  mutate(mean_t_bench = mean(benchmark_t_sims_conditional))