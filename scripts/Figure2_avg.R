###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###                            MAKE FIGURE 2                            ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------

## Data import and wrangling
source("scripts/functions/check_pkgs.R") # when rendering
source("scripts/load_environment.R")

## read dataset with donation in first wave
data_donations <- read.csv("data_experiments/individual_donations_session1.csv")

## save main dataset into new one for the plot
all_data_lollipop <- all_data_treatment

## create new variables for plotting
mean_data__lollipop <- all_data_lollipop %>%
  group_by( social_info, status_social_info) %>% 
  summarise(donation_rate = mean(choice)) %>% 
  mutate(social_info_factor = ordered(social_info, levels = c("0", "1"), labels = c('Selfish (Keep)', 'Prosocial (Donate)'))) %>% 
  mutate(status_factor = ordered(status_social_info, levels = c("L", "H"), labels = c("Low", "High"))) %>% 
  mutate(perc_change = ifelse(social_info == 0, 1 - donation_rate, donation_rate))

## create labels
labels_lollipop <- c(paste0("-", round(mean_data__lollipop$perc_change[1]*100, 0), "%"),
                     paste0("-", round(mean_data__lollipop$perc_change[2]*100, 0), "%"),
                     paste0("+", round(mean_data__lollipop$perc_change[3]*100, 0), "%"),
                     paste0("+", round(mean_data__lollipop$perc_change[4]*100, 0), "%"))

### Figure 1

## panel A
A <- 
  all_data %>%
  select(IDself, age) %>%
  filter(age < 20) %>%
  distinct() %>%
  ggplot(aes(x = factor(age))) +
  #geom_rect(aes(xmin=3.5,xmax=6.5,ymin=-Inf,ymax=Inf),alpha=0.05,fill="#377EB8",size=0) + 
  geom_bar(color = 'black', fill = 'lightgray') +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    #   position = position_dodge2(preserve = "single", width = 1),
    vjust = -0.5,
    # hjust = -.5,
    size = 4
  ) +
  #  geom_text(stat = 'count', aes(x = factor(age),label = ..count.., vjust = -0.5,   position = position_dodge(width = 0.9),) ) +
  #scale_fill_viridis_d(option = 'plasma', name = 'gender') +
  labs(y = "Count",
       x = "Age") +
  theme_classic() +
  theme(text = element_text(size = 15))+
  ylim(c(0,95))

A

library(ggh4x)
#> Loading required package: ggplot2

# Only colour strips in x-direction
strip <- strip_themed(text_x = element_text(colour = "white", face = "bold", size = 14),
                      background_x = elem_list_rect(fill = c("#C00000","#70AD47")))

## panel B
B <- 
  ggplot(mean_data__lollipop,
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
  geom_point(size = 6, aes(color = status_factor)) +
  geom_text(
    aes(label = labels_lollipop, x = status_factor, y = donation_rate),
    vjust = c(1.7, 1.7, -1.1, -1.1),
    size = 6
  ) +
  scale_color_viridis_d(option = "D",
                        name = 'Peer centrality',
                        labels = c('Low', 'High')) +
  labs(
    #    title = 'Social influence in the experimental treatments',
    #   subtitle = 'Social information was always opposite to participants\' choice in wave 1',
    x = 'Social information treatment',
    y = 'Donation rate'
  ) +
  scale_x_discrete(breaks = NULL) +
  facet_wrap2(~social_info_factor,  strip.position = "bottom", strip = strip) +
  theme_classic() +
  theme(
    text = element_text(size = 15),
    axis.text.x = element_blank(),
    #axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position=c(.7,.85),
    legend.box.background = (element_rect(colour = 'black', fill = "white", linetype='solid')),
    legend.title=element_text(size=13), 
    legend.text=element_text(size=13)
  ) +
  guides(color = guide_legend(override.aes = list(size = 2)))
B


## panel C
C <- 
  all_data_treatment %>% 
  mutate(social_info_factor = ordered(social_info, levels = c("0", "1"), labels = c('Selfish (Keep)', 'Prosocial (Donate)'))) %>% 
  group_by(IDself, social_info) %>%
  mutate(mean_follow = mean(follow)) %>% 
  distinct() %>% 
  ggplot() +
  # geom_count(aes(x = age, y = mean_follow), alpha = .2) +
  stat_summary(aes(x = age, y = follow, color = social_info_factor, shape = social_info_factor), ) +
  scale_x_continuous(breaks = seq(11,19, 1))+
  scale_color_manual(name = 'Social information treatment',
                     values = c ("#C00000","#70AD47")) +
  scale_shape_manual(name = 'Social information treatment',
                     values = c (15,17)) +
  labs(y = 'Mean proportion of conformity', x = 'Age') +
  theme_classic() +
  theme(
    text = element_text(size = 15), 
    legend.position =c (.45,.2),
    legend.box.background = element_rect(colour = "black"),
    legend.title=element_text(size=14), 
    legend.text=element_text(size=14)) +
  guides(color = FALSE,
         shape = guide_legend(override.aes = list(color =  c ("#C00000","#70AD47"))))

C


# upper <- cowplot::plot_grid(C, B, E,
#                             nrow =  1,
#                             ncol =  3,
#                             labels = c("A", "B", "D", "", "C", "E" ),label_size = 16
# )


#### lower panel 

## load data
base::load('modelfits/follow_model_all_data_pre_reg.RData')

## save treatmental effects
c_eff <-brms::conditional_effects(follow_model_all_data_pre_reg)

## dataset for panel F (age x social information), unchanged
## full effect name written out; `$` was partially matching it before
df_oppconf <- as.data.frame(c_eff$`age_scaled:social_info_factor`)


## datasets for panels D and E: effects averaged over social information
## conditional_effects() holds social_info_factor at its reference level
## ("0", keep), so c_eff$stdinDegreepeer and c_eff$age_scaled show keep rounds
## only. Here we predict for keep ("0") and donate ("1"), average the two
## predicted probabilities with equal weight within each posterior draw, and
## summarise with the median and 95% CrI, as conditional_effects() does.
## Random effects are excluded (re_formula = NA), as in conditional_effects().
## Columns are named as in conditional_effects() output, so the panel code
## below is unchanged.

model_data  <- follow_model_all_data_pre_reg$data
info_levels <- levels(model_data$social_info_factor)
info_levels ## should be "0" "1"

## age offset used on the x axis of panels E and F (age = age_scaled + 14)
## check: should return a single value, 14
unique(round(all_data_treatment$age - all_data_treatment$age_scaled, 6))
age_offset <- 14

## helper: average posterior predictions over social information
average_over_info <- function(model, grid, focal) {
  
  epred <- posterior_epred(model, newdata = grid, re_formula = NA) ## draws x rows
  
  bind_rows(lapply(unique(grid[[focal]]), function(v) {
    cols <- which(grid[[focal]] == v)
    avg  <- rowMeans(epred[, cols, drop = FALSE]) ## equal weight: keep and donate
    tibble(
      focal_value = v,
      estimate__  = median(avg),
      lower__     = unname(quantile(avg, 0.025)),
      upper__     = unname(quantile(avg, 0.975))
    )
  }))
}

## panel D data: in-degree effect at the mean age of the model data
grid_deg <- expand_grid(
  stdinDegreepeer    = seq(min(model_data$stdinDegreepeer),
                           max(model_data$stdinDegreepeer),
                           length.out = 100),
  social_info_factor = factor(info_levels, levels = info_levels),
  age_scaled         = mean(model_data$age_scaled)
)

df_deg <- average_over_info(follow_model_all_data_pre_reg,
                            grid  = grid_deg,
                            focal = "stdinDegreepeer") %>% 
  rename(stdinDegreepeer = focal_value)

## mean age in years, for the caption
mean(model_data$age_scaled) + age_offset

## panel E data: age effect at the mean in-degree of the model data
grid_age <- expand_grid(
  age_scaled         = seq(min(model_data$age_scaled),
                           max(model_data$age_scaled),
                           length.out = 100),
  social_info_factor = factor(info_levels, levels = info_levels),
  stdinDegreepeer    = mean(model_data$stdinDegreepeer)
)

df_age <- average_over_info(follow_model_all_data_pre_reg,
                            grid  = grid_age,
                            focal = "age_scaled") %>% 
  rename(age_scaled = focal_value)


## panel 1: status (averaged over social information)
D <- df_deg %>%
  ggplot(aes(x = stdinDegreepeer, y = estimate__,)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              alpha = .3, fill = 'orange'
  ) +
  geom_line(linewidth = 2,
            position = position_dodge(0.05),
            aes(color = 'orange', linetype = "1")) +
  labs(y = 'p(Conformity)', x = 'Peer in-degree [std]') +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  guides(color = 'none', linetype = 'none') +
  ylim(0, 1)

## panel 2: age (averaged over social information)
E <- df_age %>%
  ggplot(aes(x = age_scaled + age_offset, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              alpha = .3,
              fill = 'blue') +
  geom_line(linewidth = 2,
            position = position_dodge(0.05),
            aes( linetype = "1")) +
  scale_x_continuous(breaks = 11:19) +
  labs(x = 'Age',
       y = 'p(Conformity)') +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  guides(color = 'none', linetype = 'none') +
  ylim(0, 1)

## panel 3: opportunistic conformity (social information X age), unchanged
F <- df_oppconf %>%
  ggplot(aes(x = age_scaled + age_offset, y = estimate__,)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = social_info_factor),
              alpha = .3
  ) +
  geom_line(linewidth = 1,
            position = position_dodge(0.05),
            aes(color = social_info_factor, linetype = "1")) +
  labs(y = 'p(Conformity)', x = 'Age') +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  guides(#color = 'none', 
    linetype = 'none',
    #fill = 'none'
  )+
  scale_x_continuous(breaks = 11:19) +
  scale_color_manual(name = 'Social information treatment',
                     values = c ("#C00000","#70AD47"),
                     labels = c('Selfish (Keep)', 'Prosocial (Donate)')) +
  scale_fill_manual(name = 'Social information treatment',
                    values = c ("#C00000","#70AD47"),
                    labels = c('Selfish (Keep)', 'Prosocial (Donate)')) +
  ylim(0, 1) +
  theme(
    legend.position =c (.5,.2),
    legend.box.background = element_rect(colour = "black"),
    legend.title=element_text(size=14), 
    legend.text=element_text(size=14))


## sanity check: the averaged curves in D and E should lie between the keep
## and donate curves at every x value
# plot(brms::conditional_effects(follow_model_all_data_pre_reg,
#                                effects = "stdinDegreepeer:social_info_factor"))


## make figure
figure2 <- cowplot::plot_grid(B, A, C, D, E , F,
                              nrow =  2,
                              ncol =  3,
                              labels = c("A", "B", "C", "D", "E", "F"), label_size = 16
)

figure2 

## save figure
ggsave(figure2, filename = "plots/figure2.png", height = 8, width = 14)
