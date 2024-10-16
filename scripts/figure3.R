## load data
base::load("data_simulations//sim_result_3000_1_infector_4treatments.rda")


## create data frame from list
df_sims <- bind_rows(simulations_data_list, .id = "column_label") %>% 
  filter(!if_any(everything(), is.na))  %>% 
  filter(classNr != 103 & classNr!=105) %>% ## remove classes with separate components where diffusion cannot happen
  group_by(classNr) %>% ## group by class to separate network size
  mutate(fixp = 1/net_size, ## theoretical fixation probabily, 1/N
         fixt = 4*net_size) %>% ## theoretical fixation time, 4N
  ungroup()  

benchmark_t <- df_sims %>% 
  group_by(classNr) %>% 
  select(iter, classNr, treatment) %>% 
  filter(treatment == "no_structure_no_status")  %>% 
  summarise(benchmark_t_sims = mean(iter)) 

benchmark_t_conditional <- df_sims %>% 
  group_by(classNr) %>% 
  select(iter, classNr, treatment, result) %>% 
  filter(treatment == "no_structure_no_status" & result == 1)  %>% 
  summarise(benchmark_t_sims_conditional = mean(iter)) 

df_sims <- left_join(df_sims, benchmark_t, by = "classNr")
df_sims <- left_join(df_sims, benchmark_t_conditional, by = "classNr")

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
  filter(iter < 10000 & result == 1) 
ungroup()


## panel C: fixation rates

panel_C_data <- 
  fixated_B %>%
  filter(treatment == "structure_status") %>% 
  group_by(classNr) %>% 
  mutate(side = ifelse(status == "High", -1, 1)) %>%
  group_by(status, treatment, classNr) %>%
  reframe(
    S,
    net_size = net_size,
    classNr = as.character(classNr),
  ) %>%
  distinct() 


## make plot

panel_C <- 
  ggplot(data = panel_C_data, aes(x = status, y = S)) +
  geom_hline(aes(yintercept = 1), color = 'blue') +
  geom_half_boxplot(
    data = panel_C_data[which(panel_C_data$status == "High"), ],
    nudge = .1,
    width = .6,
    outlier.shape = NA,
    errorbar.length = .2
  ) +
  geom_half_boxplot(
    data = panel_C_data[which(panel_C_data$status == "Low"), ],
    side = "r",
    nudge = .1,
    width = .6,
    outlier.shape = NA,
    errorbar.length = .2
  ) +
  geom_point(aes(shape = status)) +
  stat_summary(data = panel_C_data[which(panel_C_data$status == "High"), ],
               shape = 21,
               fill = 'white',
               size = 1.1,
               stroke = 2,
               position = position_nudge(x = -.1)) +
  stat_summary(data = panel_C_data[which(panel_C_data$status == "Low"), ],
               shape = 24,
               fill = 'white',
               size = 1.1,
               stroke = 2,
               position = position_nudge(x = .1)) +
  geom_line(aes(group = classNr), color = 'grey' ) +
  geom_text(
    aes(label = ifelse(
      classNr %in% c("116", "106", "123", ""),
      as.character(classNr),
      ''
    )),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    # hjust = 2,
    color = "red"
  ) +
  geom_text(x = 2.5, y = 1, label = "benchmark", color = "blue", size = 4, hjust = 1.2, vjust = -0.2) +
  geom_hline(aes(yintercept = 1), color = 'blue') +
  theme_tidybayes() +
  theme(text = element_text(size = 18)) +
  labs(y = 'N of succesful interventions', 
       x = '"Influencer" status') +
  guides(shape = 'none') 
  #facet_wrap( ~ treatment, nrow = 1)

panel_C




## panel D: fixation times

## dataset with means
panel_D_data <-
  fixated_B %>%
  filter(treatment == "structure_status") %>% 
  group_by(classNr, status) %>% 
  mutate(side = ifelse(status == "High", -1, 1)) %>%
  select(
    T_conditional,
  ) %>%
  summarise(meanT = mean(T_conditional)) %>% 
distinct() 

panel_D <- 
ggplot(data = panel_D_data, aes(x = status, y = meanT)) +
  geom_hline(aes(yintercept = 1), color = 'blue') +
  geom_text(x = 2.5, y = 1, label = "benchmark", color = "blue", size = 4, hjust = .7, vjust = -0.2) +
  geom_half_boxplot(
    data = panel_D_data[which(panel_D_data$status == "High"), ],
    nudge = .1,
    width = .6,
    outlier.shape = NA,
    errorbar.length = .2
  ) +
  geom_half_boxplot(
    data = panel_D_data[which(panel_D_data$status == "Low"), ],
    side = "r",
    nudge = .1,
    width = .6,
    outlier.shape = NA,
    errorbar.length = .2
  ) +
  geom_point(aes(shape = status)) +
  stat_summary(data = panel_D_data[which(panel_D_data$status == "High"), ],
               aes(x = status, y = meanT),
               shape = 21,
               fill = 'white',
               size = 1.1,
               stroke = 2,
               position = position_nudge(x = -.1)) +
  stat_summary(data = panel_D_data[which(panel_D_data$status == "Low"), ],
               aes(x = status, y = meanT),
               shape = 24,
               fill = 'white',
               size = 1.1,
               stroke = 2,
               position = position_nudge(x = .1)) +
  geom_line(aes(group = classNr), color = 'grey' ) +
  geom_text(
    aes(label = ifelse(
      classNr %in% c("116", "106", "123", ""),
      as.character(classNr),
      ''
    )),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    # hjust = 2,
    color = "red"
  ) +
  geom_hline(aes(yintercept = 1), color = 'blue') +
  theme_tidybayes() +
  theme(text = element_text(size = 18)) +
  labs(x = '"Influencer" status', 
       y = 'Intervention duration') +
  guides(shape = 'none')

panel_D

## make figure

## panles
figure3 <- cowplot::plot_grid(NULL, NULL , panel_C,
                              nrow =  1,
                              labels = c("A", "B", "C"), label_size = 16
)

## 4 panels
# figure3 <- cowplot::plot_grid(NULL, NULL , panel_C, panel_D,
#                               nrow =  2,
#                               ncol =  2,
#                               labels = c("A", "B", "C", "D"), label_size = 16
# )

figure3
ggsave(plot = figure3, 'plots/figure3_3panels.png', width = 18, height = 6)
