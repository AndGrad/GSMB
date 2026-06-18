#################################################################
##                     Behavioral analyses                     ##
#################################################################

## load environment
source("scripts/load_environment.R")

all_data_treatment <- all_data_treatment 

## calculate average conformity rates across treatments
mean_follow_rate_all_rounds <- all_data_treatment %>%
  summarise(donation_rate = mean(choice))

mean_follow_rate_all_rounds

###--------------------- Vizualise data ----------------------------------------

## make a lollipop plot showing conformity rates in the 4 combinations of status (high vs. low) and social information direction (selfish vs. prosocial)
all_data_lollipop <- all_data_treatment

mean_data_condition_lollipop <- all_data_lollipop %>%
  group_by( social_info, status_social_info) %>% 
  summarise(donation_rate = mean(choice)) %>% 
  mutate(social_info_factor = ordered(social_info, levels = c("0", "1"), labels = c('Antisocial', 'Prosocial'))) %>% 
  mutate(status_factor = ordered(status_social_info, levels = c("L", "H"), labels = c("Low", "High"))) %>% 
  mutate(perc_change = ifelse(social_info == 0, 1 - donation_rate, donation_rate))


labels_lollipop <- c(paste0("-", round(mean_data_condition_lollipop$perc_change[1]*100, 0), "%"),
                     paste0("-", round(mean_data_condition_lollipop$perc_change[2]*100, 0), "%"),
                     paste0("+", round(mean_data_condition_lollipop$perc_change[3]*100, 0), "%"),
                     paste0("+", round(mean_data_condition_lollipop$perc_change[4]*100, 0), "%"))

# lollipop plot
lollipop <- 
  ggplot(mean_data_condition_lollipop,
         aes(x = status_factor, y = donation_rate)) +
  geom_segment(
    aes(
      x = status_factor,
      xend = status_factor,
      y = 1 - social_info,
      yend = donation_rate
    ),
    color = "grey",
    linewidth = 2
  ) +
  geom_point(size = 8, aes(color = status_factor)) +
  geom_text(
    aes(label = labels_lollipop, x = status_factor, y = donation_rate),
    vjust = c(1.7, 1.6, -1.1, -.9),
    size = 8
  ) +
  scale_color_viridis_d(option = "D",
                        name = 'Peer status',
                        labels = c('Low', 'High')) +
  labs(
    title = 'Social influence in the experimental treatments',
    subtitle = 'Social information was always opposite to participants\' choice in wave 1',
    x = 'Social information treatment',
    y = 'Donation rate'
  ) +
  #theme_clean()  +
  theme_light() +
  scale_x_discrete(breaks = NULL) +
  facet_grid(cols = vars(social_info_factor), switch = "x") +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle  = element_text(face = "italic", size = 15),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    strip.placement = "outside"
  )

ggsave(lollipop, filename = "plots/lollipop.png", width = 10)


###--------------------- Preregistered regression ------------------------------

## load results if the model has already been run

if (file.exists('modelfits/follow_model_all_data_pre_reg.RData')){
  
  base::load('modelfits/follow_model_all_data_pre_reg.RData')
  
} else {
  
  priors_1 <- c(
    
    prior(normal(0, 1),   class = "Intercept"),
    
    ## main effects 
    prior(normal(0, 1), class = "b", coef = "age_scaled"),
    prior(normal(0, 1), class = "b", coef = "stdinDegreepeer"),
    prior(normal(0, 1), class = "b", coef = "social_info_factor1"),
    
    ## interaction terms
    prior(normal(0, 0.5), class = "b", coef = "age_scaled:stdinDegreepeer"),
    prior(normal(0, 0.5), class = "b", coef = "social_info_factor1:age_scaled")
  )
  
  fit_prior <- brm(
    follow ~  social_info_factor *  age_scaled +  stdinDegreepeer * age_scaled +  (1|classNr/IDself), ## more complex random effect structure: (1 + social_info_factor| classNr/IDself) + (1 | IDother),
    data         = all_data_treatment,
    family       = bernoulli(link = "logit"),
    prior        = priors_1,
    sample_prior = "only",
    cores = 4,
    chains = 4,
    iter = 4000,
    seed =18
  )
  
  pp_check(fit_prior, ndraws = 1000)
  
  ## otherwise run the model
  follow_model_all_data_pre_reg <-
    brm(
      follow ~  social_info_factor *  age_scaled +  stdinDegreepeer * age_scaled +  (1|classNr/IDself),  ## more complex random effect structure: (1 + social_info_factor| classNr/IDself) + (1 | IDother),
      prior = priors_1,
      data = all_data_treatment,
      bernoulli(link = "logit"),
      cores = 4,
      chains = 4,
      iter = 3000,
      seed = 18,
      control = list(adapt_delta = 0.95)
    )
  
  conditional_effects(follow_model_all_data_pre_reg)
  
  save("follow_model_all_data_pre_reg", file = 'modelfits/follow_model_all_data_pre_reg.RData')
}

## make a table with results
sjPlot::tab_model(follow_model_all_data_pre_reg, show.est = TRUE, dv.labels = c("Follow"), pred.labels = c("Intercept", "Social Information [Donate]", "Age", "Peer In-degree", "Social Information [Donate] : Age", " Age : Peer In-degree" ), file = "tables/follow_model_all_data_pre_reg.doc")

## plot regression coefficients
spaghetti_plot <- plot_model(follow_model_all_data_pre_reg, show.values = TRUE, vline.color = "red", dv.labels = c("Follow"), pred.labels = c("Intercept", "Social Information [Donate]", "Age", "Peer In-degree", "Social Information [Donate] : Age", " Age : Peer In-degree" )) +
  theme_classic()

ggsave(spaghetti_plot, filename = "plots/spaghetti_plot.png")

###--------------------- Robustness checks -------------------------------------

## check if model output is present, if not run regression as well
if (file.exists('modelfits/follow_model_no2019_pre_reg.RData')){
  
  base::load('modelfits/follow_model_no2019_pre_reg.RData')
  
} else {
  
  data_model_no2019 <- all_data_treatment %>% 
    filter(wave != "2019")
  
  
  ## pre-registered regression, without Study 1
  follow_model_no2019 <-
    brm(
      follow ~  social_info_factor *  age_scaled +  stdinDegreepeer * age_scaled +  (1|classNr/IDself) ,
      data = data_model_no2019,
      prior = priors_1,
      bernoulli(link = "logit"),
      cores = 4,
      chains = 4,
      iter = 3000,
      seed = 99,
      control = list(adapt_delta = 0.99)
    )
  
  save("follow_model_no2019", file = 'modelfits/follow_model_no2019_pre_reg.RData')
  
}

## make a table
tab_model(follow_model_no2019)

###------------------- friendship robustness checks ---------------------------

if (file.exists('modelfits/follow_model_all_data_pre_reg_robustness_check.RData')){
  
  base::load('modelfits/follow_model_all_data_pre_reg_robustness_check.RData')
  
} else {
  
priors_2 <- c(
  ## intercept
  prior(normal(0, 1),   class = "Intercept"),
  
  ## main effects 
  prior(normal(0, 1), class = "b", coef = "age_scaled"),
  prior(normal(0, 1), class = "b", coef = "stdinDegreepeer"),
  prior(normal(0, 1), class = "b", coef = "social_info_factor1"),
  prior(normal(0, 1), class = "b", coef = "peerisfriend1"),
  
  ## interaction terms
  prior(normal(0, 0.5), class = "b", coef = "age_scaled:stdinDegreepeer"),
  prior(normal(0, 0.5), class = "b", coef = "social_info_factor1:age_scaled"),
  prior(normal(0, 0.5), class = "b", coef = "stdinDegreepeer:peerisfriend1")
  
)

fit_prior <- brm(
  follow ~  social_info_factor *  age_scaled +  stdinDegreepeer * age_scaled  + stdinDegreepeer * peerisfriend +  (1|classNr/IDself),  ## more complex random effect structure: (1 + social_info_factor| classNr/IDself) + (1 | IDother)
  data         = all_data_treatment,
  family       = bernoulli(link = "logit"),
  prior        = priors_2,
  sample_prior = "only",
  cores = 4,
  chains = 4,
  iter = 4000,
  seed = 99,
  control = list(adapt_delta = 0.95)
)

pp_check(fit_prior, ndraws = 100)

follow_model_all_data_pre_reg_robustness_check <-
  brm( 
    follow ~  social_info_factor *  age_scaled +  stdinDegreepeer * age_scaled  + stdinDegreepeer * peerisfriend +  (1|classNr/IDself),  ## more complex random effect structure: (1 + social_info_factor| classNr/IDself) + (1 | IDother)
    prior = priors,
    data = all_data_treatment,
    bernoulli(link = "logit"),
    cores = 4,
    chains = 4,
    iter = 3000,
    seed = 99,
    control = list(adapt_delta = 0.95)
  )

tab_model(follow_model_all_data_pre_reg_robustness_check)

conditional_effects(follow_model_all_data_pre_reg_robustness_check)
plot_model(follow_model_all_data_pre_reg_robustness_check)

save("follow_model_all_data_pre_reg_robustness_check", file = 'modelfits/follow_model_all_data_pre_reg_robustness_check.RData')

}

###------------ analysis of Study 2 part 2 ------------------------------------

## subset dataset and calculate mean donation rate
all_data_intervention %>% 
  filter(treatment == 4) %>% 
  summarise(mean(choice))

## check if model output is present, if not run regression as well
if (file.exists('modelfits/intervention_model.RData')){
  
  base::load('modelfits/intervention_model.RData')
  
} else {
  
## run regression model
intervention_model <- all_data_intervention %>% 
  filter(treatment == 4) %>% 
  brm(data = ., choice ~ (1|IDself), bernoulli(link = "logit"),
      cores = 4,
      chains = 4,
      iter = 5000,
      seed = 18)

save("intervention_model", file = 'modelfits/intervention_model.RData')

}

## make a table
tab_model(intervention_model)


